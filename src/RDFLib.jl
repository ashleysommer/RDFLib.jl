module RDFLib
export RDFLibInMemoryStore, RDFLibTerms
include("terms.jl")
include("namespace.jl")
include("store.jl")
include("inmemorystore.jl")
include("graph.jl")
include("n3.jl")
using .RDFLibTerms
using .RDFLibNamespace
using .RDFLibInMemoryStore

end # module
