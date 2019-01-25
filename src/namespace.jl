module RDFLibNamespace
    export Namespace, XSD, RDF, RDFS, OWL, SKOS, DOAP, FOAF, DC, DCTERMS, VOID, term
    import ..RDFLibTerms
    
    const URIRef = RDFLibTerms.URIRef
    
    struct Namespace <: AbstractString
       inner::String 
    end
    
    function title(self::Namespace)::URIRef
        return URIRef(self.inner * "title")
    end
    function term(self::Namespace, name::Union{AbstractString, AbstractChar})::URIRef
        return URIRef(self.inner * name)
    end
    function Base.get(self::Namespace, key::String, default::Union{Nothing, URIRef})::URIRef
        return term(self, key)
    end    
    function Base.getindex(self::Namespace, index::String)::URIRef
        return term(self, index)
    end
    function getfield(self::Namespace, name::Symbol)::URIRef
        return self.term(string(name))
    end
    function show(io::IO, self::Namespace)
        print(io, "Namespace(",self.inner,")")
    end
    function Base.convert(::Type{T}, self::Namespace) where T <: AbstractString
        self.inner
    end
    const RDF = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    const RDFS = Namespace("http://www.w3.org/2000/01/rdf-schema#")
    const XSD = Namespace("http://www.w3.org/2001/XMLSchema#")
    const OWL = Namespace("http://www.w3.org/2002/07/owl#")
    const SKOS = Namespace("http://www.w3.org/2004/02/skos/core#")
    const DOAP = Namespace("http://usefulinc.com/ns/doap#")
    const FOAF = Namespace("http://xmlns.com/foaf/0.1/")
    const DC = Namespace("http://purl.org/dc/elements/1.1/")
    const DCTERMS = Namespace("http://purl.org/dc/terms/")
    const VOID = Namespace("http://rdfs.org/ns/void#")
    
end #End Module RDFLibNamespace 
