using Printf
using RDFLib.RDFLibNamespace
using RDFLib.RDFLibTripleStore
using RDFLib.RDFLibInMemoryStore
using RDFLib.RDFLibTerms
using RDFLib.RDFLibN3
using RDFLib.RDFLibGraph
using Test

const MF = Namespace("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#")
const RDFT = Namespace("http://www.w3.org/ns/rdftest#")
const QT = Namespace("http://www.w3.org/2001/sw/DataAccess/tests/test-query#")
const test_files_manifest = abspath("./test/NTriplesTests/manifest.ttl")

manifest_graph = Graph()
parsefile(test_files_manifest, manifest_graph)
manifests = collect(subjects(manifest_graph, RDF["type"], MF["Manifest"]))
if length(manifests) < 1
    error("No mf:Manifest instances found in the manifest file.")
end
manifest = manifests[1]
manifest_entries_lists = collect(objects(manifest_graph, manifest, MF["entries"]))
if length(manifest_entries_lists) < 1
    error("mf:Manifest has no mf:entries")
end
entries_list = manifest_entries_lists[1]
entries_values = items(manifest_graph, entries_list)
for e in entries_values
    e_types = collect(objects(manifest_graph, e, RDF["type"]))
    if length(e_types) < 1
        error("manifest entry $(e) does not have an rdf:type")
    end
    e_type = e_types[1]
     actions = collect(objects(manifest_graph, e, MF["action"]))
    if length(actions) < 1
        error("manifest entry $(e) has no mf:action!")
    end
    action_nt = actions[1]
    println(action_nt)
    if e_type == RDFT["TestNTriplesPositiveSyntax"]
        test_graph = Graph()
        parsefile(action_nt, test_graph, ntriples=true)
    elseif e_type == RDFT["TestNTriplesNegativeSyntax"]
        test_graph = Graph()
        excepted = try
            parsefile(action_nt, test_graph, ntriples=true)
            iterator = RDFLibGraph.triples(test_graph, TriplePattern((nothing, nothing, nothing)))
            my_collection = collect(iterator)
            for c in my_collection
                println(c)
            end
            false
        catch e
            if isa(e, BadSyntax)
                true
            else
                rethrow(e)
            end
        end
        @assert excepted
    end

    
end
exit()

iterator = RDFLibGraph.triples(manifest_graph, TriplePattern((nothing, nothing, nothing)))
my_collection = collect(iterator)
println(my_collection)


# all_files = readdir(test_files_loc)
# all_full_paths = map(f -> abspath(joinpath(test_files_loc, f)), all_files)
# @printf("%s", all_full_paths)
# for f in all_full_paths
#     graph = Graph()
#     parseFile(f, graph)
# end
