using Printf
using RDFLib.RDFLibInMemoryStore
using RDFLib.RDFLibTerms
using BenchmarkTools

store = InMemoryStore("bench1")
add(store, Triple((Literal(1), Literal(2), Literal(3))), nothing)
add(store, Triple((Literal(1), Literal(2), Literal(4))), nothing)
add(store, Triple((Literal(1), Literal(5), Literal(6))), nothing)
add(store, Triple((Literal(1), Literal(5), Literal(7))), nothing)
add(store, Triple((Literal(8), Literal(9), Literal(10))), nothing)
add(store, Triple((Literal(8), Literal(11), Literal(10))), nothing)
add(store, Triple((Literal(8), Literal(11), Literal(12))), nothing)


function bench()
    my_iter = triples(store, TriplePattern((Literal(1),nothing,nothing)), nothing)
    my_collection = collect(my_iter)
    passed = (length(my_collection) == 2)
end




b = @benchmarkable bench()
tune!(b)
r = run(b)
a = summary(r)
println(r)
