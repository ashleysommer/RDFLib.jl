module RDFLibTripleStore
    using ..RDFLibTerms
    export TripleStore, namespace
    
    abstract type TripleStore end
    is_context_aware(store::TripleStore)::Bool = false
    is_transaction_aware(store::TripleStore)::Bool = false
    is_graph_aware(store::TripleStore)::Bool = false
    is_formula_aware(store::TripleStore)::Bool = false

    create(store::TripleStore) = false #We have no dispatcher yet, so this does nothing

    open(store::TripleStore)::Union{Int8, Nothing} = nothing

    close(store::TripleStore) = false #Do Nothing

    destroy(store::TripleStore) = false #Do Nothing

    add(store::TripleStore, triple::Triple; context::MaybeNode=nothing, quoted::Bool=false) = false #This needs a dispatcher too
    add(store::TripleStore, quad::Quad; quoted::Bool=false) = false #This needs a dispatcher too

    function addN(store::TripleStore, triples::Triples; context::MaybeNode=nothing, quoted::Bool=false)
        for t in triples
            add(store, t, context=context, quoted=quoted)
        end
    end

    function addN(store::TripleStore, quads::Quads; quoted::Bool=false)
        for q in quads
            add(store, q, quoted=quoted)
        end
    end

    remove!(store::TripleStore, triple::Triple; context::MaybeNode=nothing)::Any = false #This needs a dispatcher too
    remove!(store::TripleStore, triple::TriplePattern; context::MaybeNode=nothing)::Any = false #This needs a dispatcher too
    remove!(store::TripleStore, quad::Quad)::Any = false #This needs a dispatcher too

    triples(store::TripleStore, pattern::TriplePattern; context::MaybeNode=nothing)::TripleIterator = false
    triples(store::TripleStore, pattern::QuadPattern)::TripleIterator = false
    quads(store::TripleStore, pattern::QuadPattern)::QuadsIterator = false
    Base.length(store::TripleStore; context::MaybeNode=nothing)::UInt64 = 0
    bind(store::TripleStore, prefix::String, namespace::URIRef)::Nothing = nothing
    prefix(store::TripleStore, namespace::Union{String, URIRef})::Union{Nothing, String} = error("Not implemented")
    namespace(store::TripleStore, prefix::String)::Union{Nothing, String, URIRef} = error("Not implemented")
    
    namespaces(store::TripleStore)::NamespacesIterator = NamespacesIterator()
    add_graph(store::TripleStore, graph::Any) = false # TODO: add proper graph type
    remove_graph(store::TripleStore, graph::Any) = false # TODO: add proper graph type

end # End Module RDFLibTripleStore
