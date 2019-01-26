module RDFLib
export RDFLibTripleStore, RDFLibInMemoryStore, RDFLibNamespace, RDFLibTerms, RDFLibGraph, SPARQL
include("terms.jl")
include("namespace.jl")
include("store.jl")
include("inmemorystore.jl")
include("graph.jl")
include("n3.jl")
include("sparql/sparql.jl")
using .RDFLibTerms
using .RDFLibNamespace
using .RDFLibTripleStore
using .RDFLibInMemoryStore
using .RDFLibGraph
using .SPARQL

end # module
