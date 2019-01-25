#=
inmemorystore:
- Julia version: 1.0.2
- Author: flubba86
- Date: 2018-12-08
=#

module ModA
    #export Node, Identifier, BNode, URIRef, Literal #, Triple
    abstract type Node end
    abstract type Identifier <: Node end
    #const Triple = Base.NamedTuple{(:subject, :predicate, :object), Tuple{Identifier, Identifier, Identifier}}

    struct BNode <: Identifier
       a::Int8
    end

    struct URIRef <: Identifier
        a::Int8
    end

    struct Literal <: Identifier
        a::Int8
    end

end
# End Module A


module ModB
    using ..ModA


end # End Module B


module ModC
    using ..ModB


end # End Module C
