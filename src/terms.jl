module RDFLibTerms
    using UUIDs
    export Node, Identifier, Variable, BNode, URIRef, Literal, Triple, Quad, Triples, Quads, TriplePattern, QuadPattern, MaybeNode, XSD_PREFIX
    export NodeIterator, TripleIterator, QuadIterator, NamespacesIterator, _unique_id
    abstract type Node end
    abstract type Identifier <: Node end
    const XSD_PREFIX = "http://www.w3.org/2001/XMLSchema#"
    const MaybeNode = Union{Nothing, Node}
    const TripleTuple = Tuple{Node, Node, Node}
    const Triple = Base.NamedTuple{(:subject, :predicate, :object), TripleTuple}
    const QuadTuple = Tuple{Node, Node, Node, Node}
    const Quad = Base.NamedTuple{(:subject, :predicate, :object, :graph), QuadTuple}
    const TriplePattern = Base.NamedTuple{(:subject, :predicate, :object), Tuple{MaybeNode, MaybeNode, MaybeNode}}
    const QuadPattern = Base.NamedTuple{(:subject, :predicate, :object, :graph), Tuple{MaybeNode, MaybeNode, MaybeNode, MaybeNode}}
    const Triples = Array{Triple, 1}
    const Quads = Array{Quad, 1}
    abstract type NodeIterator end
    abstract type TripleIterator end
    abstract type QuadIterator end
    abstract type NamespacesIterator end
    Base.iterate(iterator_::TripleIterator, state::Symbol)::Union{Nothing, Tuple{Triple, Symbol}} = nothing
    Base.IteratorSize(t::Type{TripleIterator}) = Base.SizeUnknown()
    Base.iterate(iterator_::QuadIterator, state::Symbol)::Union{Nothing, Tuple{Quad, Symbol}} = nothing
    Base.IteratorSize(t::Type{QuadIterator}) = Base.SizeUnknown()
    Base.iterate(iterator_::NamespacesIterator, state::Symbol)::Union{Nothing, Tuple{Union{Nothing, String, URIRef}, Symbol}} = nothing
    Base.IteratorSize(t::Type{NamespacesIterator}) = Base.HasLength()

    Base.convert(::Type{Triple}, source::TripleTuple) = Triple(source)
    Base.convert(::Type{TripleTuple}, source::Triple) = (source[:subject], source[:predicate], source[:object])
    Base.convert(::Type{Quad}, source::QuadTuple) = Quad(source)
    Base.convert(::Type{QuadTuple}, source::Quad) = (source[:subject], source[:predicate], source[:object], source[:graph])
    
    struct Variable <: Identifier
        inner::String
        function Variable(key::String)::Variable
            return new(key)
        end
    end
    function Base.convert(::Type{T}, self::Variable) where T <: AbstractString
        self.inner
    end    
    struct BNode <: Identifier
        inner::String
        function BNode(identifier::String)::BNode
            return new(identifier)
        end
        function BNode()::BNode
            return new(string(uuid4()))
        end
    end
    function Base.convert(::Type{T}, self::BNode) where T <: AbstractString
        self.inner
    end
    function Base.:(==)(x::BNode, y::T) where T <: AbstractString
       x.inner == y
    end
    function Base.:(==)(x::T, y::BNode) where T <: AbstractString
       x == y.inner
    end
    function show(io::IO, self::BNode)
        print(io, "BNode(_:$(self.inner)")
    end
    
    struct URIRef <: Identifier
        inner::String
        function URIRef(uri::URIRef)::URIRef
            #return a reference to the same one
            return uri
        end
        function URIRef(uri::String)::URIRef
            return new(uri)
        end
    end
    function Base.:(==)(x::URIRef, y::T)::Bool where T <: AbstractString
       x.inner === y
    end
    function Base.:(==)(x::T, y::URIRef)::Bool where T <: AbstractString
       x === y.inner
    end
    function Base.convert(::Type{T}, self::URIRef) where T <: AbstractString
        self.inner
    end
    function show(io::IO, self::URIRef)
        print(io, "URIRef(\"$(self.inner)\"")
    end
    struct Literal <: Identifier
        lexical::String
        language::Union{String, Nothing}
        datatype::Union{URIRef, Nothing}
        
        function Literal(num::Integer)::Literal
            new(string(num), nothing, URIRef(XSD_PREFIX*"string"))
        end
        
        function Literal(lexical::String, datatype::URIRef)::Literal
            new(lexical, nothing, datatype)
        end

        function Literal(lexical::String, language::String)::Literal
            new(lexical, language, nothing)
        end
        
        function Literal(lexical::String)::Literal
            new(lexical, nothing, nothing)
        end
    end
    function Base.convert(::Type{T}, self::Literal) where T <: AbstractString
        self.lexical 
    end
    function show(io::IO, self::Literal)
        print(io, "Literal(\"$(self.lexical)\"")
    end
    _unique_id()::String = "N" # for rdflib backward compatibility

end # End Module RDFLibTerms

