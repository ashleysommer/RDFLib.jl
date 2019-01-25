using Printf
using RDFLib.RDFLibTripleStore
using RDFLib.RDFLibInMemoryStore
using RDFLib.RDFLibTerms
using RDFLib.RDFLibN3
using RDFLib.RDFLibGraph
using Test
store = InMemoryStore("test1")
RDFLibTripleStore.add(store, Triple((Literal(1), Literal(2), Literal(3))))
RDFLibTripleStore.add(store, Triple((Literal(1), Literal(2), Literal(4))))
RDFLibTripleStore.add(store, Triple((Literal(1), Literal(5), Literal(6))))
RDFLibTripleStore.add(store, Triple((Literal(1), Literal(5), Literal(7))))
RDFLibTripleStore.add(store, Triple((Literal(8), Literal(9), Literal(10))))
RDFLibTripleStore.add(store, Triple((Literal(8), Literal(11), Literal(10))))
RDFLibTripleStore.add(store, Triple((Literal(8), Literal(11), Literal(12))))


function test_spo_index_mode_spa()::Bool
    my_iter = RDFLibTripleStore.triples(store, TriplePattern((Literal(1),Literal(2),nothing)))
    my_collection = collect(my_iter)
    passed = (length(my_collection) == 2)
    passed |= Triple((Literal(1), Literal(2), Literal(3))) in my_collection
    passed |= Triple((Literal(1), Literal(2), Literal(4))) in my_collection
    return passed
end
function test_spo_index_mode_saa()::Bool
    my_iter = RDFLibTripleStore.triples(store, TriplePattern((Literal(1),nothing,nothing)))
    my_collection = collect(my_iter)
    passed = (length(my_collection) == 4)
    passed |= Triple((Literal(1), Literal(2), Literal(3))) in my_collection
    passed |= Triple((Literal(1), Literal(2), Literal(4))) in my_collection
    passed |= Triple((Literal(1), Literal(5), Literal(7))) in my_collection
    passed |= Triple((Literal(1), Literal(5), Literal(7))) in my_collection
    my_iter2 = RDFLibTripleStore.triples(store, TriplePattern((Literal(8),nothing,nothing)))
    my_collection2 = collect(my_iter2)
    passed |= (length(my_collection2) == 2)
    passed |= Triple((Literal(8), Literal(9), Literal(10))) in my_collection2
    passed |= Triple((Literal(8), Literal(11), Literal(10))) in my_collection2
    return passed
end
function test_spo_index_mode_sap()::Bool
    my_iter = RDFLibTripleStore.triples(store, TriplePattern((Literal(8),nothing,Literal(10))))
    my_collection = collect(my_iter)
    passed = (length(my_collection) == 2)
    passed |= Triple((Literal(8), Literal(9), Literal(10))) in my_collection
    passed |= Triple((Literal(8), Literal(11), Literal(10))) in my_collection
    return passed
end
@test test_spo_index_mode_spa()
@test test_spo_index_mode_saa()
@test test_spo_index_mode_sap()

const test_files_manifest = abspath("./test/TurtleTests/manifest.ttl")
# t1 = open(test_files_manifest)
# str1 = read(t1, String)
# close(t1)
# for i = firstindex(str1):lastindex(str1)
#     try
#         println(str1[i])
#     catch e
#         println("error at $(i)")
#         throw(e)
#         ignore the index error
#     end
# end


manifest_graph = Graph()
parsefile(test_files_manifest, manifest_graph)

Namespace("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#")

iterator = RDFLibTripleStore.triples(manifest_graph.store, TriplePattern((nothing, nothing, nothing)))
my_collection = collect(iterator)
println(my_collection)


# all_files = readdir(test_files_loc)
# all_full_paths = map(f -> abspath(joinpath(test_files_loc, f)), all_files)
# @printf("%s", all_full_paths)
# for f in all_full_paths
#     graph = Graph()
#     parseFile(f, graph)
# end
