#=
inmemorystore:
- Julia version: 1.0.2
- Author: flubba86
- Date: 2018-12-08
=#

module RDFLibInMemoryStore
    using Printf
    using ..RDFLibTerms
    using ..RDFLibTripleStore
    export InMemoryStore, InMemoryTripleIterator
    const InMemoryIndex = Dict{Identifier, Dict{Identifier, Dict{Identifier, Bool}}}
    mutable struct InMemoryStore <: TripleStore
        identifier::String

        # indexed by [subject][predicate][object]
        _spo::InMemoryIndex

        # indexed by [predicate][object][subject]
        _pos::InMemoryIndex

        # indexed by [predicate][object][subject]
        _osp::InMemoryIndex

        _namespace::Dict{String, URIRef} #prefix->ns
        _prefix::Dict{URIRef, String} #ns->prefix

        function _add_empty_indexes!(self::InMemoryStore)::InMemoryStore
            self._spo = Dict()
            self._pos = Dict()
            self._osp = Dict()
            self._namespace = Dict()
            self._prefix = Dict()
            return self
        end
        function InMemoryStore(identifier::String)::InMemoryStore
            self = new(identifier)
            return _add_empty_indexes!(self)
        end
        function InMemoryStore()::InMemoryStore
            self = new("anonymous")
            return _add_empty_indexes!(self)
        end
    end

    RDFLibTripleStore.is_context_aware(store::InMemoryStore)::Bool = true
    RDFLibTripleStore.is_transaction_aware(store::InMemoryStore)::Bool = true
    RDFLibTripleStore.is_graph_aware(store::InMemoryStore)::Bool = true
    RDFLibTripleStore.is_formula_aware(store::InMemoryStore)::Bool = true

    create(store::InMemoryStore) = false #We have no dispatcher yet, so this does nothing

    open(store::InMemoryStore)::Union{Int8, Nothing} = nothing

    close(store::InMemoryStore) = false #Do Nothing

    destroy(store::InMemoryStore) = false #Do Nothing

    function RDFLibTripleStore.add(store::InMemoryStore, triple::Triple; context::MaybeNode=nothing, quoted::Bool=false)
        # add dictionary entries for spo[s][p][p] = 1 and pos[p][o][s] = 1, creating the nested dictionaries where they do not yet
        # exits.
        subject, predicate, object = triple
        spo = store._spo
        
        if haskey(spo, subject)
            po = spo[subject]
        else
            po = spo[subject] = Dict{Identifier, Dict{Identifier, Bool}}()
        end
        
        if haskey(po, predicate)
            o = po[predicate]
        else
            o = po[predicate] = Dict{Identifier, Bool}()
        end
        o[object] = true

        pos = store._pos
        
        if haskey(pos, predicate)
            os = pos[predicate]
        else
            os = pos[predicate] = Dict{Identifier, Dict{Identifier, Bool}}()
        end    
        
        if haskey(os, object)
            s = os[object]
        else
            s = os[object] = Dict{Identifier, Bool}()
        end
        s[subject] = true

        osp = store._osp
        if haskey(osp, object)
            sp = osp[object]
        else
            sp = osp[object] = Dict{Identifier, Dict{Identifier, Bool}}()
        end
        if haskey(sp, subject)
            p = sp[subject]
        else
            p = sp[subject] = Dict{Identifier, Bool}()
        end
        p[predicate] = true
    end

    RDFLibTripleStore.add(store::InMemoryStore, quad::Quad; quoted::Bool=false)::Any = false #This needs a dispatcher too

    function RDFLibTripleStore.addN(store::InMemoryStore, triples::Triples; context::MaybeNode=nothing, quoted::Bool=false)
        for t in triples
            RDFLibTripleStore.add(store, t, context=context, quoted=quoted)
        end
    end

    function RDFLibTripleStore.addN(store::InMemoryStore, quads::Quads; quoted::Bool=false)
        for q in quads
            RDFLibTripleStore.add(store, q, quoted=quoted)
        end
    end

    function RDFLibTripleStore.remove!(store::InMemoryStore, triple::Triple; context::MaybeNode=nothing)::Any
        subject, predicate, object_ = triple
        # if the triple exists in the _spo index, then it must exist in all of the indexes.
        if haskey(store._spo, subject) && haskey(store._spo[subject], predicate) && haskey(store._spo[subject][predicate], object_)
            delete!(store._spo[subject][predicate], object_)
            delete!(store._pos[predicate][object_], subject)
            delete!(store._osp[object_][subject], predicate)
        end
    end
    function RDFLibTripleStore.remove!(store::InMemoryStore, tp::TriplePattern; context::MaybeNode=nothing)::Any
        for ((subject, predicate, object_), c) in self.triples(tp)
            delete!(store._spo[subject][predicate], object_)
            delete!(store._pos[predicate][object_], subject)
            delete!(store._osp[object_][subject], predicate)
        end
    end
    RDFLibTripleStore.remove!(store::InMemoryStore, quad::Quad)::Any = false #This needs a dispatcher too

    const _InMemoryTripleIteratorDict1State = Union{Tuple{InMemoryIndex, Int}, Nothing}
    const _InMemoryTripleIteratorDict2State = Union{Tuple{Dict{Identifier, Dict{Identifier, Bool}}, Int}, Nothing}
    const _InMemoryTripleIteratorDict3State = Union{Tuple{Dict{Identifier, Bool}, Int}, Nothing}

    const InMemoryTripleIteratorState = Tuple{Symbol, _InMemoryTripleIteratorDict1State, _InMemoryTripleIteratorDict2State, _InMemoryTripleIteratorDict3State}

    mutable struct InMemoryTripleIterator <: TripleIterator
        store::InMemoryStore
        pattern::TriplePattern
        index_mode::Symbol
        index_key_primary::Union{Identifier, Nothing}
        index_key_secondary::Union{Identifier, Nothing}
        index_key_tertiary::Union{Identifier, Nothing}
        
        function InMemoryTripleIterator(store::InMemoryStore, pattern::TriplePattern)::InMemoryTripleIterator
            return new(store, pattern, :nothing, nothing, nothing, nothing)
        end
    end
    
    Base.iterate(iter::InMemoryTripleIterator)::Union{Nothing, Tuple{Triple, InMemoryTripleIteratorState}} = iterate(iter, (:new, nothing, nothing, nothing))
    function Base.iterate(iter::InMemoryTripleIterator, state::InMemoryTripleIteratorState)::Union{Nothing, Tuple{Triple, InMemoryTripleIteratorState}}
        iterator_state, dict1_state, dict2_state, dict3_state = state
        if iterator_state == :new
            maybe_subject, maybe_predicate, maybe_object = iter.pattern
            if maybe_subject !== nothing
                iter.index_key_primary = maybe_subject
                if maybe_predicate !== nothing
                    iter.index_key_secondary = maybe_predicate
                    if maybe_object !== nothing
                        iter.index_mode = :spo
                        iter.index_key_tertiary = maybe_object
                    else
                        iter.index_mode = :spa
                    end
                else
                    if maybe_object !== nothing
                        iter.index_mode = :sao
                        iter.index_key_tertiary = maybe_object
                    else
                        iter.index_mode = :saa
                    end
                end
            elseif maybe_predicate !== nothing
                iter.index_key_primary = maybe_predicate
                if maybe_object !== nothing
                    iter.index_mode = :poa
                    iter.index_key_secondary = maybe_object
                else
                    iter.index_mode = :paa
                end
            elseif maybe_object !== nothing
                iter.index_key_primary = maybe_object
                iter.index_mode = :oaa
            else
                iter.index_mode = :aaa
            end
            iterator_state = :restart
        end
        if iterator_state == :restart
            store = iter.store
            i_mode = iter.index_mode
            if i_mode == :spo
                spo = store._spo
                if haskey(spo, iter.index_key_primary)
                    pred_dict = spo[iter.index_key_primary]
                    if haskey(pred_dict, iter.index_key_secondary)
                        obj_dict = pred_dict[iter.index_key_secondary]
                        haskey(obj_dict, iter.index_key_tertiary) && return (Triple((iter.index_key_primary, iter.index_key_secondary, iter.index_key_tertiary)), (:done, nothing, nothing, nothing))
                    end
                end
                return nothing
            end
            if i_mode == :spa
                spo = store._spo
                if haskey(spo, iter.index_key_primary)
                    pred_dict = spo[iter.index_key_primary]
                    if haskey(pred_dict, iter.index_key_secondary)
                        obj_dict = pred_dict[iter.index_key_secondary]
                        next = iterate(obj_dict)
                        next == nothing && return nothing
                        ((i,b), d3_state) = next
                        dict3_state = (obj_dict, d3_state)
                        return (Triple((iter.index_key_primary, iter.index_key_secondary, i)), (:continue, nothing, nothing, dict3_state))
                    else
                        return nothing
                    end
                else
                    return nothing
                end
            end
            if i_mode == :sao
                spo = store._spo
                if haskey(spo, iter.index_key_primary)
                    pred_dict = spo[iter.index_key_primary]
                    pred_ids = keys(pred_dict)
                    next = iterate(pred_ids)
                    while next !== nothing
                        (d3_key, d2_state) = next
                        obj_dict = pred_dict[d3_key]
                        if haskey(obj_dict, iter.index_key_tertiary)
                            dict2_state = (pred_dict, d2_state)
                            return (Triple((iter.index_key_primary, d3_key, iter.index_key_tertiary)), (:continue, nothing, dict2_state, nothing))
                        end
                        next = iterate(pred_ids, d2_state)
                    end
                    return nothing
                else
                    return nothing
                end
            end
            if i_mode == :saa
                spo = store._spo
                if haskey(spo, iter.index_key_primary)
                    pred_dict = spo[iter.index_key_primary]
                    pred_ids = keys(pred_dict)
                    d2_state = 0
                    next_p = iterate(pred_ids)
                    while next_p !== nothing
                        prev_d2_state = d2_state
                        (d3_key, d2_state) = next_p
                        obj_dict = pred_dict[d3_key]
                        next_o = iterate(obj_dict)
                        if next_o == nothing
                            next_p = iterate(pred_ids, d2_state)
                            continue
                        end
                        ((i,b), d3_state) = next_o
                        dict2_state = (pred_dict, prev_d2_state)
                        dict3_state = (obj_dict, d3_state)
                        return (Triple((iter.index_key_primary, d3_key, i)), (:continue, nothing, dict2_state, dict3_state))
                    end
                    return nothing
                else
                    return nothing
                end
            end
            if i_mode == :poa
                pos = store._pos
                if haskey(pos, iter.index_key_primary)
                    obj_dict = pos[iter.index_key_primary]
                    if haskey(obj_dict, iter.index_key_secondary)
                        subj_dict = obj_dict[iter.index_key_secondary]
                        next = iterate(subj_dict)
                        next == nothing && return nothing
                        ((i,b), d3_state) = next
                        dict3_state = (subj_dict, d3_state)
                        return (Triple((i, iter.index_key_primary, iter.index_key_secondary)), (:continue, nothing, nothing, dict3_state))
                    else
                        return nothing
                    end
                else
                    return nothing
                end
            end
            if i_mode == :aaa
                spo = store._spo
                sub_ids = keys(spo)
                d1_state = 0
                next_s = iterate(sub_ids)
                while next_s !== nothing
                    prev_d1_state = d1_state
                    d2_key, d1_state = next_s
                    pred_dict = spo[d2_key]
                    pred_ids = keys(pred_dict)
                    d2_state = 0
                    next_p = iterate(pred_ids)
                    while next_p !== nothing
                        prev_d2_state = d2_state
                        (d3_key, d2_state) = next_p
                        obj_dict = pred_dict[d3_key]
                        next_o = iterate(obj_dict)
                        if next_o == nothing
                            next_p = iterate(pred_ids, d2_state)
                            continue
                        end
                        ((i,b), d3_state) = next_o
                        dict1_state = (spo, prev_d1_state)
                        dict2_state = (pred_dict, prev_d2_state)
                        dict3_state = (obj_dict, d3_state)
                        return (Triple((d2_key, d3_key, i)), (:continue, dict1_state, dict2_state, dict3_state))
                    end
                    next_s = iterate(sub_ids, d1_state)
                end
                return nothing
            end
            if i_mode == :paa || i_mode == :oaa || i_mode == :aaa
                error("Uninplemented index state!")
            end
            error("InMemoryTripleIterator invalid index state.")
        elseif iterator_state == :continue
            i_mode = iter.index_mode
            if i_mode == :spa
                (obj_dict, d3_state) = dict3_state
                next = iterate(obj_dict, d3_state)
                next == nothing && return nothing
                ((i, b), d3_state) = next
                dict3_state = (obj_dict, d3_state)
                return (Triple((iter.index_key_primary, iter.index_key_secondary, i)), (:continue, nothing, nothing, dict3_state))
            end
            if i_mode == :sao
                (pred_dict, d2_state) = dict2_state
                pred_ids = keys(pred_dict)
                next = iterate(pred_ids, d2_state)
                while next !== nothing
                    (d3_key, d2_state) = next
                    d3 = pred_dict[d3_key]
                    if haskey(d3, iter.index_key_tertiary)
                        dict2_state = (pred_dict, d2_state)
                        return (Triple((iter.index_key_primary, d3_key, iter.index_key_tertiary)), (:continue, nothing, dict2_state, nothing))
                    end
                    next = iterate(pred_ids, d2_state)
                end
                return nothing
            end
            if i_mode === :saa
                #@printf("got here 1\n")
                (pred_dict, d2_state) = dict2_state
                (obj_dict, d3_state) = dict3_state
                #@printf("%s %s\n", dict2_state, dict3_state)
                pred_ids = keys(pred_dict)
                next_p = iterate(pred_ids, d2_state)
                #TODO: Optimisation, avoid having to look up d2 on every iteration of d3
                while next_p !== nothing
                    prev_d2_state = d2_state
                    (d3_key, d2_state) = next_p
                    obj_dict = pred_dict[d3_key]
                    next_o = iterate(obj_dict, d3_state)
                    if next_o == nothing
                        #TODO: reseting d3_state to 0 here is hacky.
                        #It should be the idxfloor of the next obj_dict.
                        d3_state = 0
                        next_p = iterate(pred_ids, d2_state)
                        continue
                    end
                    ((i,b), d3_state) = next_o
                    dict2_state = (pred_dict, prev_d2_state)
                    dict3_state = (obj_dict, d3_state)
                    return (Triple((iter.index_key_primary, d3_key, i)), (:continue, nothing, dict2_state, dict3_state))
                end
                return nothing
            end    
            if i_mode == :aaa
                (top_dict, d1_state) = dict1_state
                (pred_dict, d2_state) = dict2_state
                (obj_dict, d3_state) = dict3_state
                sub_ids = keys(top_dict)
                next_s = iterate(sub_ids, d1_state)
                while next_s !== nothing
                    prev_d1_state = d1_state
                    d2_key, d1_state = next_s
                    pred_dict = top_dict[d2_key]
                    pred_ids = keys(pred_dict)
                    next_p = iterate(pred_ids, d2_state)
                    while next_p !== nothing
                        prev_d2_state = d2_state
                        (d3_key, d2_state) = next_p
                        obj_dict = pred_dict[d3_key]
                        next_o = iterate(obj_dict, d3_state)
                        if next_o == nothing
                            #TODO: reseting d3_state to 0 here is hacky.
                            #It should be the idxfloor of the next obj_dict.
                            d3_state = 0
                            next_p = iterate(pred_ids, d2_state)
                            continue
                        end
                        ((i,b), d3_state) = next_o
                        dict1_state = (top_dict, prev_d1_state)
                        dict2_state = (pred_dict, prev_d2_state)
                        dict3_state = (obj_dict, d3_state)
                        return (Triple((d2_key, d3_key, i)), (:continue, dict1_state, dict2_state, dict3_state))
                    end
                    #TODO: reseting d2_state to 0 here is hacky.
                    #It should be the idxfloor of the next obj_dict.
                    d2_state = 0
                    next_s = iterate(sub_ids, d1_state)
                end
                return nothing
            end
            if i_mode == :poa
                (subj_dict, d3_state) = dict3_state
                next = iterate(subj_dict, d3_state)
                next == nothing && return nothing
                ((i, b), d3_state) = next
                dict3_state = (subj_dict, d3_state)
                return (Triple((i, iter.index_key_primary, iter.index_key_secondary)), (:continue, nothing, nothing, dict3_state))
            end
            if i_mode == :paa || i_mode == :oaa || i_mode == :aaa
                error("Uninplemented index state!")
            end
        elseif iterator_state == :done
            return nothing
        else
            error("InMemoryTripleIterator invalid iterator state.")
        end
    end
    Base.IteratorSize(t::Type{InMemoryTripleIterator}) = Base.SizeUnknown()
#     Base.iterate(iter::InMemoryQuadIterator, state::Symbol)::Union{Nothing, Tuple{Quad, Symbol}} = nothing
#     Base.IteratorSize(t::Type{InMemoryQuadIterator}) = Base.SizeUnknown()
#     Base.iterate(iter::NamespacesIterator, state::Symbol)::Union{Nothing, Tuple{URIRef, Symbol}} = nothing
#     Base.IteratorSize(t::Type{TripleIterator}) = Base.HasLength()
    
    function RDFLibTripleStore.triples(store::InMemoryStore, pattern::TriplePattern; context::MaybeNode=nothing)::InMemoryTripleIterator
        return InMemoryTripleIterator(store, pattern)
    end
    RDFLibTripleStore.triples(store::InMemoryStore, pattern::QuadPattern)::Triples = false
    RDFLibTripleStore.quads(store::InMemoryStore, pattern::QuadPattern)::Quads = false
    function Base.length(store::InMemoryStore; context::MaybeNode=nothing)::UInt64
        c::UInt64 = 0
        for i in RDFLibTripleStore.triples(store, TriplePattern((nothing, nothing, nothing)), context=context)
            c += 1
        end
        return c
    end
    function RDFLibTripleStore.bind(store::InMemoryStore, prefix::String, namespace::URIRef)::Nothing
        store._namespace[prefix] = namespace
        store._prefix[namespace] = prefix
        return
    end
    function RDFLibTripleStore.prefix(store::InMemoryStore, namespace::Union{String, URIRef})::Union{String, Nothing}
        try
            return store._prefix[namespace]
        catch e
            if e isa KeyError
                return nothing
            end
            throw(e)
        end
    end
    function RDFLibTripleStore.namespace(store::InMemoryStore, prefix::String)::Union{Nothing, String, URIRef}
        try
            return store._namespace[prefix]
        catch e
            if e isa KeyError
                return nothing
            end
            throw(e)
        end
    end
    mutable struct InMemoryNamespacesIterator <: NamespacesIterator
        store::InMemoryStore
    end
    Base.iterate(iter::InMemoryNamespacesIterator)::Union{Nothing, Tuple{Union{Nothing, String => Union{String, URIRef}}, Union{Nothing, Integer}}} = iterate(iter, nothing)
    function Base.iterate(iter::InMemoryNamespacesIterator, state::Union{Nothing, Integer})::Union{Nothing, Tuple{Union{Nothing, String => Union{String, URIRef}}, Union{Nothing, Integer}}}
        if state === nothing
            r1, rstate = iterate(store._namespace)
            return (r1, rstate)
        else
            r1, rstate = iterate(store._namespace, state)
            return (r1, rstate)
        end
    end
    RDFLibTripleStore.namespaces(store::InMemoryStore)::InMemoryNamespacesIterator = InMemoryNamespacesIterator(store)
    RDFLibTripleStore.add_graph(store::InMemoryStore, graph::Any) = false # TODO: add proper graph type
    RDFLibTripleStore.remove_graph(store::InMemoryStore, graph::Any) = false # TODO: add proper graph type
end # End Module RDFLibInMemoryStore
