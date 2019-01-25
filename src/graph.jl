module RDFLibGraph
    using ..RDFLibTerms
    import ..RDFLibNamespace
    import ..RDFLibTripleStore
    import ..RDFLibInMemoryStore
    export Graph, QuotedGraph, AbstractGraph, add, remove!, triples, items, objects, predicates, subjects
    
    const TripleStore = RDFLibTripleStore.TripleStore
    const Namespace = RDFLibNamespace.Namespace
    const RDF = RDFLibNamespace.RDF
    
    abstract type AbstractGraph <: Node end
        
    function add(self::AbstractGraph, triple::Triple)
        """Add a triple with self as context"""
        RDFLibTripleStore.add(self.store, triple, context=self, quoted=false)
    end
    function add(self::AbstractGraph, triple::Tuple{Node, Node, Node})
        s, p, o = triple
        """Add a triple with self as context"""
        add(self, convert(Triple, (s, p, o)))
    end
    

#    function addN(self, quads::Quads)
#        """Add a sequence of triple with context"""
#        #TODO: julia filter like it does in the python comprehension
#         addN(self.store, (s, p, o, c) for s, p, o, c in quads
#                           if c isa Graph &&
#                           c.identifier is self.identifier and
#                           _assertnode(s, p, o)
#                           )
#        RDFLibTripleStore.addN(self.store, (s, p, o, c) for (s, p, o, c) in quads)
#    end
    function remove!(self::AbstractGraph, triple::Triple)
        """Remove a triple from the graph

        If the triple does not provide a context attribute, removes the triple
        from all contexts.
        """
        RDFLibTripleStore.remove!(self.store, triple, context=self)
    end
    
    function remove!(self::AbstractGraph, triple::TriplePattern)
        """Remove a triple pattern from the graph

        If the triple does not provide a context attribute, removes the triple
        from all contexts.
        """
        RDFLibTripleStore.remove!(self.store, triple, context=self)
    end
    
    function set(self::AbstractGraph, triple::TriplePattern)
        """Convenience method to update the value of object
        
        Remove any existing triples for subject and predicate before adding
        (subject, predicate, object).
        """
        (subject, predicate, object_) = triple
        if subject === nothing
            error("s can't be None in .set([s,p,o]), as it would remove (*, p, *)")
        end
        if predicate === nothing
            error("p can't be None in .set([s,p,o]), as it would remove (s, *, *)")
        end
        remove(self, TriplePattern((subject, predicate, nothing)))
        if object_ !== nothing
            add(self, Triple((subject, predicate, object_)))
        end
    end
    
    function triples(self::AbstractGraph, pattern::TriplePattern)::TripleIterator
        """Generator over the triple store

        Returns triples that match the given triple pattern. If triple pattern
        does not provide a context, all contexts will be searched.
        """
        #TODO: Julia, do the same as in python below
#         s, p, o = triple
#         if isinstance(p, Path):
#             for _s, _o in p.eval(self, s, o):
#                 yield (_s, p, _o)
#         else:
#             for (s, p, o), cg in self.__store.triples((s, p, o), context=self):
#                 yield (s, p, o)
        return RDFLibTripleStore.triples(self.store, pattern)
    end
    const ListItemsIteratorState = Tuple{Symbol, Node}

    mutable struct ListItemsIterator <: NodeIterator
        graph::AbstractGraph
        listbase::Node
        history::Array{Node, 1}
        
        
        function ListItemsIterator(graph::AbstractGraph, listbase::Node)::ListItemsIterator
            return new(graph, listbase, [])
        end
    end
    
    Base.iterate(iter::ListItemsIterator)::Union{Nothing, Tuple{Node, ListItemsIteratorState}} = iterate(iter, (:continue, iter.listbase))
    function Base.iterate(iter::ListItemsIterator, state::ListItemsIteratorState)::Union{Nothing, Tuple{Node, ListItemsIteratorState}}
        (state_sym::Symbol, list::Node) = state
        if state_sym == :continue
            if list in iter.history
                error("List contains a recursive rdf:rest reference")
            end
            push!(iter.history, list)
            firsts = collect(triples(iter.graph, TriplePattern((list, RDF["first"], nothing))))
            if length(firsts) < 1
                error("Node is not an rdf:list")
            end
            item::Node = firsts[1].object
            rests = collect(triples(iter.graph, TriplePattern((list, RDF["rest"], nothing))))
            if length(rests) < 1
                # looks like the end of the list
                state_sym = :stop
            end
            list = rests[1].object
            if list == RDF["nil"]
                # this is explicitly the end of the list
                state_sym = :stop
            end
            return (item, (state_sym, list))
        end
        return nothing
    end
    function items(self::AbstractGraph, list::Node)::ListItemsIterator
        """Generator over all items in the resource specified by list

        list is an RDF collection.
        """
        return ListItemsIterator(self, list)
    end
    Base.IteratorSize(t::Type{ListItemsIterator}) = Base.SizeUnknown()
    Base.length(self::AbstractGraph)::UInt64 = Base.length(self.store, context=self)
    function objects(self::AbstractGraph, subject::Node, predicate::Node)::Base.Generator
        iter::TripleIterator = triples(self, TriplePattern((subject, predicate, nothing)))
        return (f.object for f in iter)
    end
    
    function predicates(self::AbstractGraph, subject::Node, object_::Node)::Base.Generator
        iter::TripleIterator = triples(self, TriplePattern((subject, nothing, object_)))
        return (f.predicate for f in iter)
    end
    
    function subjects(self::AbstractGraph, predicate::Node, object_::Node)::Base.Generator
        iter::TripleIterator = triples(self, TriplePattern((nothing, predicate, object_)))
        return (f.subject for f in iter)
    end
    
    
    
    function bind(self::AbstractGraph, prefix::String, namespace::String; override::Bool=true)
        """Bind prefix to namespace

        If override is True will bind namespace to given prefix even
        if namespace was already bound to a different prefix.

        for example:  graph.bind('foaf', 'http://xmlns.com/foaf/0.1/')

        """
        return bind(self.namespacemanager,
                    prefix, namespace, override=override)
    end
    function namespaces(self::AbstractGraph)
        """Generator over all the prefix, namespace tuples"""
        return iterate(self.namespace_manager)
    end
    
    struct NamespaceManager
        graph::RDFLibGraph.AbstractGraph
        _cache::Dict{String, String}
        
        function NamespaceManager(graph::RDFLibGraph.AbstractGraph)::NamespaceManager
            ns_man = new(graph, Dict())
            bind(ns_man, "xml",  "http://www.w3.org/XML/1998/namespace")
            bind(ns_man, "rdf", RDFLibNamespace.RDF)
            bind(ns_man, "rdfs", RDFLibNamespace.RDFS)
            bind(ns_man, "xsd", RDFLibNamespace.XSD)
            return ns_man
        end
    end
    
    store(self::NamespaceManager) = self.graph.store
    
    function bind(self::NamespaceManager, prefix::Union{Nothing, String}, ns::Union{Namespace, URIRef, String}; override::Bool=true, replace::Bool=false)
        """bind a given namespace to the prefix

        if override, rebind, even if the given namespace is already
        bound to another prefix.

        if replace, replace any existing prefix with the new namespace

        """
        
        new_namespace = ns isa Namespace ? URIRef(ns.inner) : URIRef(ns)
        # When documenting explain that override only applies in what cases
        if prefix === nothing
            prefix = ""
        end
        st = store(self)
        bound_namespace = RDFLibTripleStore.namespace(st, prefix)
        # Check if the bound_namespace contains a URI
        # and if so convert it into a URIRef for comparison
        # This is to prevent duplicate namespaces with the
        # same URI
        if bound_namespace !== nothing
            bound_namespace = URIRef(bound_namespace)
        end    
        if bound_namespace !== nothing && bound_namespace != new_namespace
            if replace
                RDFLibTripleStore.bind(st, prefix, new_namespace)
                return
            end
            # prefix already in use for different namespace
            #
            # append number to end of prefix until we find one
            # that's not in use.
            if prefix === nothing || length(prefix) < 1
                prefix = "default"
            end    
            num = 1
            while true
                new_prefix = prefix * string(num)
                tnamespace = RDFLibTripleStore.namespace(st, new_prefix)
                if tnamespace !== nothing && new_namespace == URIRef(tnamespace)
                    # the prefix is already bound to the correct
                    # namespace
                    return
                end    
                if RDFLibTripleStore.namespace(st, new_prefix) === nothing
                    break
                end    
                num += 1
            end    
            RDFLibTripleStore.bind(st, new_prefix, new_namespace)
        else
            bound_prefix = RDFLibTripleStore.prefix(st, new_namespace)
            if bound_prefix === nothing
                RDFLibTripleStore.bind(st, prefix, new_namespace)
            elseif bound_prefix == prefix
                nothing  # already bound
            else
                if override || bound_prefix[1] == '_'  # or a generated prefix
                    RDFLibTripleStore.bind(st, prefix, new_namespace)
                end
            end        
        end           
    end
    
    mutable struct Graph <: AbstractGraph
        store::TripleStore
        identifier::Identifier
        defaultunion::Bool
        namespacemanager::NamespaceManager
        
        function Graph(;identifier::Union{Nothing, Identifier}=nothing, store::Union{Nothing, TripleStore}=nothing)::Graph
            if store == nothing
                if identifier == nothing
                    useStore = RDFLibInMemoryStore.InMemoryStore()
                else
                    useStore = RDFLibInMemoryStore.InMemoryStore(string(identifier))
                end
            else
                useStore = store
            end
            if identifier === nothing
                useIdentifier = URIRef(useStore.identifier)
            else
                useIdentifier = identifier
            end
            g = new(useStore, useIdentifier, false)
            g.namespacemanager = NamespaceManager(g)
            return g
        end
    end
    
    struct QuotedGraph <: AbstractGraph
        """
        Quoted Graphs are intended to implement Notation 3 formulae. They are
        associated with a required identifier that the N3 parser *must* provide
        in order to maintain consistent formulae identification for scenarios
        such as implication and other such processing.
        """
        
        inner::Graph
        function QuotedGraph(;identifier::Union{Nothing, Identifier}=nothing, store::Union{Nothing, TripleStore}=nothing)::QuotedGraph
            new(Graph(identifier=identifier, store=store))
        end
    end
    function add(self::QuotedGraph, triple::Triple)
        """Add a triple with self as context, and quoted"""
        RDFLibTripleStore.add(self.inner.store, triple, context=self, quoted=true)
    end
    
end # End Module RDFLibGraph
