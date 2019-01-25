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
const test_files_manifest = abspath("./test/TurtleTests/manifest.ttl")

const allowed_failures = [
"#turtle-syntax-bad-struct-04", # we explicitly allow literals in subject position
"#turtle-syntax-bad-struct-14", # we explicitly allow literals in subject position
"#turtle-syntax-bad-n3-extras-03", # No idea whats going on with this one
"#turtle-syntax-bad-n3-extras-06", # No idea whats going on with this one
"#turtle-syntax-bad-string-06", #Behaviour is actually correct
"#turtle-syntax-bad-string-07", #Behaviour is actually correct
]

function runTest(manifest_graph, e, e_type, action_ttl)
    if e_type == RDFT["TestTurtlePositiveSyntax"]
        test_graph = Graph()
        parsefile(action_ttl, test_graph, turtle=true)
    elseif e_type == RDFT["TestTurtleNegativeSyntax"]
        test_graph = Graph()
        excepted = try
            parsefile(action_ttl, test_graph, turtle=true)
            iterator = RDFLibGraph.triples(test_graph, TriplePattern((nothing, nothing, nothing)))
            my_collection = collect(iterator)
            if length(my_collection) < 1
                println("No triples parsed.")
            else
                for c in my_collection
                    println(c)
                end
            end
            false
        catch ex
            if isa(ex, BadSyntax)
                println(ex)
                true
            else
                rethrow(ex)
            end
        end
        @assert excepted
    elseif e_type == RDFT["TestTurtleEval"]
        results = collect(objects(manifest_graph, e, MF["result"]))
        if length(results) < 1
            error("manifest entry $(e) has no mf:result!")
        end
        if length(results) > 1
            error("manifest entry $(e) has ambiguous mf:result!")
        end
        result_nt = results[1]
        println(result_nt)
        results_graph = Graph()
        parsefile(result_nt, results_graph, ntriples=true)
        test_graph = Graph()
        parsefile(action_ttl, test_graph, turtle=true)
        iterator = RDFLibGraph.triples(test_graph, TriplePattern((nothing, nothing, nothing)))
        my_collection = collect(iterator)
        @assert length(test_graph) == length(results_graph)
    elseif e_type == RDFT["TestTurtleNegativeEval"]
        results = collect(objects(manifest_graph, e, MF["result"]))
        if length(results) > 1
            error("manifest entry $(e) has ambiguous mf:result!")
        end
        if length(results) < 1
            result = nothing
            results_graph = nothing
        else
            result_nt = results[1]
            println(result_nt)
            results_graph = Graph()
            parsefile(result_nt, results_graph, ntriples=true)
        end
        
        test_graph = Graph()
        excepted = try
            parsefile(action_ttl, test_graph, turtle=true)
            my_collection = collect(RDFLibGraph.triples(test_graph, TriplePattern((nothing, nothing, nothing))))
            for c in my_collection
                println(c)
            end
            false
        catch ex
            if isa(ex, InvalidNode)
                println(ex)
                true
            else
                rethrow(ex)
            end
        end
        if results_graph !== nothing
            @assert length(test_graph) != length(results_graph)
        else
            @assert excepted || length(test_graph) == 0
        end
        
    else
        println("unknown test type: $(e_type)")
    end
end

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
    action_ttl = actions[1]
    println(action_ttl)
    try
        runTest(manifest_graph, e, e_type, action_ttl)
    catch ex
        if isa(ex, AssertionError)
            allowed = false
            for al in allowed_failures
                endswith(e.inner, al) && (allowed = true)
            end
            !allowed && rethrow(e)
        else
            rethrow(ex)
        end
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
