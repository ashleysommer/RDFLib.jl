using Printf
using Test
using RDFLib.SPARQL

const SPARQLParser = SPARQL.SPARQLParser

simple_one = """
BASE <http://example.com/ns1#>
PREFIX foaf:   <http://xmlns.com/foaf/0.1/>
PREFIX two:   <http://xmlns.com/two/0.1/>
SELECT DISTINCT ?x ?name ( COUNT(A) AS ?d )
WHERE  { ?x foaf:name ?name }
"""

res = SPARQLParser.parse(simple_one)
