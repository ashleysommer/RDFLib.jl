"""
notation3.py - Standalone Notation3 Parser
Derived from CWM, the Closed World Machine

Authors of the original suite:

* Dan Connolly <@@>
* Tim Berners-Lee <@@>
* Yosi Scharf <@@>
* Joseph M. Reagle Jr. <reagle@w3.org>
* Rich Salz <rsalz@zolera.com>

http://www.w3.org/2000/10/swap/notation3.py

Copyright 2000-2007, World Wide Web Consortium.
Copyright 2001, MIT.
Copyright 2001, Zolera Systems Inc.

License: W3C Software License
http://www.w3.org/Consortium/Legal/copyright-software

Modified by Sean B. Palmer
Copyright 2007, Sean B. Palmer.

Modified to work with rdflib by Gunnar Aastrand Grimnes
Copyright 2010, Gunnar A. Grimnes

"""
module RDFLibN3
import ..RDFLibTerms
import ..RDFLibGraph
import ..RDFLibTripleStore
import Base.PCRE
using UUIDs
using Decimals
using Printf
export parsefile, BadSyntax, InvalidNode
const Identifier = RDFLibTerms.Identifier
const Variable = RDFLibTerms.Variable
const URIRef = RDFLibTerms.URIRef
const Literal = RDFLibTerms.Literal
const BNode = RDFLibTerms.BNode
const Graph = RDFLibGraph.Graph
const QuotedGraph = RDFLibGraph.QuotedGraph
const TripleStore = RDFLibTripleStore.TripleStore

const NodeOrLiteralType = Union{Identifier, Bool, Integer, Decimal, Float32, Float64}

abstract type Parser end

function splitFragP(uriref::String, punct=0)::Tuple{String, String}
    """split a URI reference before the fragment

    Punctuation is kept.

    e.g.

    >>> splitFragP("abc#def")
    ('abc', '#def')

    >>> splitFragP("abcdef")
    ('abcdef', '')

    """

    found = findlast("#", uriref)
    found === nothing && return (uriref, "")
    return (uriref[1:found.start-1], uriref[found.start:end])
end

function join(here::String, there::String)::String
    """join an absolute URI and URI reference
    (non-ascii characters are supported/doctested;
    haven't checked the details of the IRI spec though)

    ``here`` is assumed to be absolute.
    ``there`` is URI reference.

    >>> join('http://example/x/y/z', '../abc')
    'http://example/x/abc'

    Raise ValueError if there uses relative path
    syntax but here has no hierarchical path.

    >>> join('mid:foo@example', '../foo') # doctest: +NORMALIZE_WHITESPACE
    Traceback (most recent call last):
        raise ValueError(here)
    ValueError: Base <mid:foo@example> has no slash
    after colon - with relative '../foo'.

    >>> join('http://example/x/y/z', '')
    'http://example/x/y/z'

    >>> join('mid:foo@example', '#foo')
    'mid:foo@example#foo'

    We grok IRIs

    >>> len(u'Andr\\xe9')
    5

    >>> join("http://example.org/", u'#Andr\\xe9')
    u'http://example.org/#Andr\\xe9'
    """

#    assert(here.find("#") < 0), \
#        "Base may not contain hash: '%s'" % here  # why must caller splitFrag?
    slashl = findfirst("/", there)
    colonl = findfirst(":", there)

    # join(base, 'foo:/') -- absolute
    if colonl !== nothing
        if colonl.start >= 1
            if slashl === nothing || colonl.start < slashl.start
                return there
            end
        end
    end
    bcolonl = findfirst(":", here)
    if bcolonl === nothing
        error("Base uri '$(here)' is not absolute")
    end
    
    (path, frag) = splitFragP(there)
    if length(path) < 1
        return here * frag
    end
     # join('mid:foo@example', '../foo') bzzt
    if here[bcolonl.start + 1:bcolonl.start + 1] != "/"
        error("Base <$(here)> has no slash after "*
             "colon - with relative '$(there)'.")
    end
    if here[bcolonl.start + 1:bcolonl.start + 2] == "//"
        bpathl_found = findnext("/", here, bcolonl.start + 3)
        if bpathl_found === nothing
            bpathl = 0
        else
            bpathl = bpathl_found.start
        end
    else
        bpathl = bcolonl.start + 1
    end
     # join('http://xyz', 'foo')
    if bpathl <= 0
        bpathl = length(here)
        here = here * "/"
    end
     # join('http://xyz/', '//abc') => 'http://abc'
    if length(there) >1 && there[1:2] == "//"
        return here[1:bcolonl.start + 1] * there
    end
     # join('http://xyz/', '/abc') => 'http://xyz/abc'
    there[1] == '/' && return here[1:bpathl] * there

    slashr_found = findlast("/", here)
    if slashr_found === nothing
        slashr = 0
    else
        slashr = slashr_found.start
    end

    while true
        path_len = length(path)
        if path_len >= 2 && path[1:2] == "./"
            path = path[3:end]
            path_len -= 2
        end
        if path == "."
            path = ""
        elseif (path_len >= 3 && path[1:3] == "../") || path == ".."
            path = path_len > 3 ? path[4:end] : ""
            i_found = findprev("/", here, slashr-1)
            if i_found !== nothing
                i = i_found.start
                here = here[1:i]
                slashr = i
            end
        else
            break
        end
    end
    return here[1:slashr] * path * frag
end

function base()::String
    """The base URI for this process - the Web equiv of cwd

    Relative or abolute unix-standard filenames parsed relative to
    this yeild the URI of the file.
    If we had a reliable way of getting a computer name,
    we should put it in the hostname just to prevent ambiguity

    """
     # return "file://" + hostname + os.getcwd() + "/"
    return "file://" * _fixslash(pwd()) * "/"
end

function _fixslash(s::String)::String
    """ Fix windowslike filename to unixlike - (#ifdef WINDOWS)"""
    s = replace(s, "\\"=>"/")
    if s[1] !== "/" && s[2] === ":"
        s = s[3:end]   #Hack when drive letter present
    end
    return s
end

const CONTEXT = 0
const PRED = 1
const SUBJ = 2
const OBJ = 3

const PARTS = (PRED, SUBJ, OBJ)
const ALL4 = (CONTEXT, PRED, SUBJ, OBJ)

const SYMBOL = 0
const FORMULA = 1
const LITERAL = 2
const LITERAL_DT = 21
const LITERAL_LANG = 22
const ANONYMOUS = 3
const XMLLITERAL = 25

const Logic_NS = "http://www.w3.org/2000/10/swap/log#"
const NODE_MERGE_URI = Logic_NS * "is"  # Pseudo-property indicating node merging

const RDF_type_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
const RDF_NS_URI = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
const OWL_NS = "http://www.w3.org/2002/07/owl#"
const DAML_sameAs_URI = OWL_NS * "sameAs"
const parsesTo_URI = Logic_NS * "parsesTo"
const RDF_spec = "http://www.w3.org/TR/REC-rdf-syntax/"

const List_NS = RDF_NS_URI  # From 20030808

const N3_forSome_URI = Logic_NS * "forSome"
const N3_forAll_URI = Logic_NS * "forAll"
const forSome = Symbol(N3_forSome_URI)
const forAll = Symbol(N3_forAll_URI)

const N3_first = (SYMBOL, List_NS * "first")
const N3_rest = (SYMBOL, List_NS * "rest")
const N3_li = (SYMBOL, List_NS * "li")
const N3_nil = (SYMBOL, List_NS * "nil")
const N3_List = (SYMBOL, List_NS * "List")
const N3_Empty = (SYMBOL, List_NS * "Empty")

runNamespaceValue = nothing

function runNamespace()::Union{String, Nothing}
    "Return a URI suitable as a namespace for run-local objects"
     # @@@ include hostname (privacy?) (hash it?)
    global runNamespaceValue
    if runNamespaceValue === nothing
        runNamespaceValue = join(base(), RDFLibTerms._unique_id()) * '#'
    end
    return runNamespaceValue
end

nextu = 0

function uniqueURI()::String
    "A unique URI"
    global nextu
    nextu += 1
    return runNamespace() * "u_" * string(nextu)
end

chatty_flag = 50

# from why import BecauseOfData, becauseSubexpression


function BecauseOfData(args...)::Nothing
     # print args, kargs
    return
end

function becauseSubexpression(args...)::Nothing
     # print args, kargs
    return
end



# Magic resources we know about

const RDF_type = URIRef(RDF_type_URI)
const DAML_sameAs = URIRef(DAML_sameAs_URI)

const LOG_implies_URI = "http://www.w3.org/2000/10/swap/log#implies"

const BOOLEAN_DATATYPE = URIRef(RDFLibTerms.XSD_PREFIX * "boolean")
const DECIMAL_DATATYPE = URIRef(RDFLibTerms.XSD_PREFIX * "decimal")
const DOUBLE_DATATYPE = URIRef(RDFLibTerms.XSD_PREFIX * "double")
const FLOAT_DATATYPE = URIRef(RDFLibTerms.XSD_PREFIX * "float")
const INTEGER_DATATYPE = URIRef(RDFLibTerms.XSD_PREFIX * "integer")

option_noregen = 0    # If set, do not regenerate genids on output

# @@ I18n - the notname chars need extending for well known unicode non-text
# characters. The XML spec switched to assuming unknown things were name
# characaters.
# _namechars = string.lowercase + string.uppercase + string.digits + '_-'
const _notQNameChars = "\t\r\n !\"#\$&'()*,+/;<=>?@[\\]^`{|}~"  # else valid qname :-/
const _notKeywordsChars = _notQNameChars * "."
const _notNameChars = _notQNameChars * ":"  # Assume anything else valid name :-/

const hexChars = "ABCDEFabcdef0123456789"
const escapeChars = "(_~.-!\$&'()*+,;=/?#@%)"  # valid for \ escapes in localnames

function unicodeExpand(m::RegexMatch)::Char
    try
        return Char(parse(Int, m.captures[1], base=16))
    catch e
        error("Invalid unicode code point: " * m.captures[1])
    end
end
function unicodeExpand(m::SubString)::Char
    points::String = (m[1:2] == "\\u" || m[1:2] == "\\U") ? string(m[3:end]) : string(m)
    try
        return Char(parse(Int, points, base=16))
    catch e
        error("Invalid unicode code point: " * points)
    end
end

const unicodeEscape4 = r"\\u([0-9a-fA-F]{4})"
const unicodeEscape8 = r"\\U([0-9a-fA-F]{8})"
const N3CommentCharacter = '#'  # For unix script  # ! compatabilty

########################################## Parse string to sink
#
# Regular expressions:
const eol = r"[ \t]*(#[^\n]*)?\r?\n"  # end  of line, poss. w/comment
const eof = r"[ \t]*(#[^\n]*)?$"  # end  of file, poss. w/comment
const ws = r"[ \t]+"  # Whitespace not including NL
const integer_syntax = r"[-+]?[0-9]+"
const decimal_syntax = r"[-+]?[0-9]*\.[0-9]+"
const exponent_syntax = Regex("[-+]?(?:[0-9]+\\.[0-9]*(?:e|E)[-+]?[0-9]+|"*
                             "\\.[0-9](?:e|E)[-+]?[0-9]+|"*
                             "[0-9]+(?:e|E)[-+]?[0-9]+)")
const interesting = r"[\\\r\n\"\']"
const langcode = r"[a-zA-Z]+(-[a-zA-Z0-9]+)*"

###############################################################################
formula_number = 0
mutable struct Formula
    uuid::String
    counter::Int
    number::Int
    existentials::Dict{URIRef, BNode}
    universals::Dict{Any, Any}
    quotedgraph::Any

    function Formula(parent::Any)::Formula
        global formula_number
        formula_number += 1
        self = new(string(uuid4()), 0, formula_number, Dict(), Dict())
        self.quotedgraph = QuotedGraph(
            store=parent.store, identifier=id(self))
        return self
    end

    function show(io::IO, self::Formula)
        print(io, "_:Formula")
        print(io, String(self.number))
    end
end
id(self::Formula)::BNode = BNode("_:Formula" * string(self.number))

function newBlankNode(self::Formula; uri::Union{Nothing, String}=nothing)::BNode
    if uri === nothing
        self.counter += 1
        bn = BNode("f$(self.uuid)b$(self.counter)")
    else
        bn = BNode(replace(first(split(uri, "#")), "_"=>"b"))
    end
    return bn
end
function newUniversal(self::Formula, uri::String)::BNode
    return Variable(first(split("#", uri)))
end

function declareExistential(self::Formula, x::URIRef)::Nothing
    self.existentials[x] = newBlankNode(self)
end

function close(self::Formula)::Any
    return self.quotedgraph
end

#######################

struct BadSyntax <: Exception
    uri::Any
    lines::Any
    str::String
    i::Int
    why::String
    function BadSyntax(_thisDoc::Any, lines::Any, argstr::String, i::Int, why::String)
      new(_thisDoc, lines, argstr, i, why)
    end
    
end
function Base.show(io::IO, self::BadSyntax)
    argstr = self.str
    i = self.i
    st = 1
    if i > 61
        pre = "..."
        st = i - 60
    else
        pre = ""
    end    
    if lastindex(argstr) - i > 60
        post = "..."
    else
        post = ""
    end
    print(io, "Syntax Error at line ")
    print(io, self.lines+1)
    print(io, " of <")
    print(io, self.uri)
    print(io, ">:\nBad syntax (")
    print(io, self.why)
    print(io, ") at ^ in:\n")
    print(io, pre)
    print(io, argstr[st:i-1])
    print(io, "^")
    endl = min(lastindex(argstr), i+60)
    print(io, argstr[i:thisind(argstr, endl)])
    print(io, post)
    print(io, "\"")
end

struct InvalidNode <: Exception
    uri::Any
    lines::Any
    str::String
    i::Int
    why::String
    function InvalidNode(_thisDoc::Any, lines::Any, argstr::String, i::Int, why::String)
      new(_thisDoc, lines, argstr, i, why)
    end
    
end
function Base.show(io::IO, self::InvalidNode)
    argstr = self.str
    i = self.i
    st = 1
    if i > 61
        pre = "..."
        st = i - 60
    else
        pre = ""
    end    
    if lastindex(argstr) - i > 60
        post = "..."
    else
        post = ""
    end
    print(io, "Invalid Node Error at line ")
    print(io, self.lines+1)
    print(io, " of <")
    print(io, self.uri)
    print(io, ">:\nInvalid Node (")
    print(io, self.why)
    print(io, ") at ^ in:\n")
    print(io, pre)
    print(io, argstr[st:i-1])
    print(io, "^")
    endl = min(lastindex(argstr), i+60)
    print(io, argstr[i:thisind(argstr, endl)])
    print(io, post)
    print(io, "\"")
end
################################3

mutable struct RDFSink
    rootFormula::Union{Nothing, Formula}
    counter::Int
    graph::Graph


    function RDFSink(graph::Graph)::RDFSink
        new(nothing, 0, graph)
    end
end
function newFormula(self::RDFSink)::Formula
    @assert RDFLibTripleStore.is_formula_aware(self.graph.store)
    Formula(self.graph)
end

newGraph(self::RDFSink, identifier)::Graph = Graph(self.graph.store, identifier)

newSymbol(self::RDFSink, args...) = URIRef(args[1])

function newBlankNode(self::RDFSink, arg::Union{Nothing, Formula, Graph}=nothing; uri::Union{Nothing, String}=nothing)::BNode
    if isa(arg, Formula)
        return newBlankNode(arg, uri=uri)
    elseif (isa(arg, Graph)) || (arg === nothing)
        self.counter += 1
        bn = BNode("n" * String(self.counter))
    else
        bn = BNode(String(arg[1]).split("#").pop().replace("_", "b"))
    end
    return bn
end
function newLiteral(self::RDFSink, s::String, dt::Union{Nothing, URIRef}, lang::Union{Nothing, String})::Literal
    dt !== nothing && return Literal(s, dt)
    lang !== nothing && return Literal(s, lang)
    Literal(s)
end
function newList(self::RDFSink, n::Array{NodeOrLiteralType, 1}, f::Formula)::Union{BNode, URIRef}
    nil = newSymbol(self,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
    )
    if length(n) < 1
        return nil
    end
    first = newSymbol(self,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
    )
    rest = newSymbol(self,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
    )
    af::BNode = ab::BNode = newBlankNode(self, f)

    for ne in n[1:end-1]
        makeStatement(self, (f, first, ab, ne))
        an::BNode = newBlankNode(self, f)
        makeStatement(self, (f, rest, ab, an))
        ab = an
    end
    makeStatement(self, (f, first, ab, n[end]))
    makeStatement(self, (f, rest, ab, nil))
    return af
end

newSet(self::RDFSink, args...)::Set = Set(args)

# this does nothing?!
setDefaultNamespace(self::RDFSink, args...)::String = Base.join([repr(n) for n in args], ":")

function makeStatement(self::RDFSink, quadruple::Tuple{Any, Any, Any, Any})::Nothing
    f, p, s, o = quadruple

    if isa(p, Formula) #hasattr(p, "formula")
        error("Formula used as predicate")
    end
    s = normalise(self, f, s)
    p = normalise(self, f, p)
    o = normalise(self, f, o)

    if f == self.rootFormula
            # print s, p, o, '.'
        RDFLibGraph.add(self.graph, (s, p, o))
    elseif isa(f, Formula)
        RDFLibGraph.add(f.quotedgraph, (s, p, o))
    else
        RDFLibGraph.add(f, (s,p,o))
    end
        # return str(quadruple)
    return
end

function normalise(self::RDFSink, f, n)::Union{Literal, URIRef, BNode}
    if isa(n, Tuple)
        return URIRef(string(n[2]))
    end
    if isa(n, Bool)
        s = Literal(lowercase(string(n)), BOOLEAN_DATATYPE)
        return s
    end
    if isa(n, Integer)
        s = Literal(string(n), INTEGER_DATATYPE)
        return s
    end
    if isa(n, Decimal)
        value::String = string(n)
        if value == "-0"
            value = "0"
        end
        s = Literal(value, DECIMAL_DATATYPE)
        return s
    end
    if isa(n, Float64)
        s = Literal(string(n), DOUBLE_DATATYPE)
        return s
    end
    if isa(n, Float32)
        s = Literal(string(n), FLOAT_DATATYPE)
        return s
    end
    if isa(n, Formula)
        n in keys(f.existentials) && return f.existentials[n]
    end
        # if isinstance(n, Var):
        #    if f.universals.has_key(n):
        #       return f.universals[n]
        #    f.universals[n] = f.newBlankNode()
        #    return f.universals[n]

    return n
end

function intern(self::RDFSink, something::Any)::Any
    return something
end

function bind(self::RDFSink, pfx, uri)::Nothing
    nothing  # print pfx, ':', uri
end
function startDoc(self::RDFSink, formula)::Nothing
    self.rootFormula = formula
    return
end

function endDoc(self::RDFSink, formula)::Nothing
    nothing
end

##################

mutable struct SinkParser <: Parser
    _baseURI::Union{Nothing, String}
    _bindings::Dict{String, String}
    _anonymousNodes::Dict{String, BNode}
    _variables::Dict{URIRef, Variable}
    _parentVariables::Dict{URIRef, Variable}
    _store::Union{RDFSink, TripleStore}
    _thisDoc::String
    _genPrefix::String
    _formula::Formula
    _context::Union{Formula, Graph}
    _parentContext::Union{Nothing, String}
    lines::Int
    startOfLine::Int
    keywords::Array{String, 1}
    keywordsSet::Bool
    turtle::Bool
    ntriples::Bool
    stringDelimiters::Array{Char, 1}
    
    function SinkParser(store::Union{RDFSink, TripleStore}; openFormula::Union{Nothing, Formula}=nothing, thisDoc::String="", baseURI::Union{Nothing, String}=nothing, genPrefix::String="", turtle::Bool=false, ntriples::Bool=false)::SinkParser
        if baseURI !== nothing
            _baseURI = baseURI
        else
            if thisDoc != ""
                _baseURI = thisDoc
            else
                _baseURI = nothing
            end
        end
        self = new(_baseURI, Dict{String, String}(), Dict(), Dict(), Dict(), store, thisDoc, genPrefix)
        _init!(self, openFormula, baseURI, turtle, ntriples)
        return self
    end
    
    function _init!(self::SinkParser, openFormula::Union{Nothing, Formula}, baseURI::Union{Nothing, String}, turtle::Bool, ntriples::Bool)
        """ note: namespace names should *not* end in  # ;
        the  # will get added during qname processing """
        thisDoc = self._thisDoc
        if thisDoc !== ""
            if !(':' in thisDoc)
                error("Document URI not absolute: <%s>", thisDoc)
            end
            self._bindings[""] = thisDoc * "#"   # default
        end
        store = self._store
        genPrefix = self._genPrefix
        if genPrefix != ""
            setGenPrefix(store, genPrefix)  # pass it on
        end
        
        self.lines = 0               # for error handling
        self.startOfLine = 0         # For calculating character number
        self.keywords = ["a", "this", "bind", "has", "is", "of",
                         "true", "false"]
        self.keywordsSet = false  # Then only can others be considerd qnames
        #self._reason = why   # Why the parser was asked to parse this

        self.turtle = turtle # throw error when encountering N3 extensions
        self.ntriples = ntriples # throw error when encountering N3 or Turtle extensions

        # Turtle allows single or double quotes around strings, whereas N3 and NT 
        # only allows double quotes.
        self.stringDelimiters = (turtle && (!ntriples)) ? ['\"', '\''] : ['\"']
        if !((self._baseURI === nothing) || (':' in self._baseURI))
            error("BaseURI must have ':' in it.")
        end
        
        if genPrefix == ""
            if thisDoc != ""
                self._genPrefix = thisDoc * "#_g"
            else
                self._genPrefix = uniqueURI()
            end
        end
        if openFormula === nothing
            if thisDoc != ""
                self._formula = newFormula(store, thisDoc * "#_formula")
            else
                self._formula = newFormula(store)
            end
        else
            self._formula = openFormula
        end
        self._context = self._formula
        self._parentContext = nothing
    end
end
function BadSyntax(self::SinkParser, argstr::String, i::Int, msg::String)::Nothing
    throw(BadSyntax(self._thisDoc, self.lines, argstr, i, msg))
end
function InvalidNode(self::SinkParser, argstr::String, i::Int, msg::String)::Nothing
    throw(InvalidNode(self._thisDoc, self.lines, argstr, i, msg))
end
function here(self::SinkParser, i::Int)::String
    """String generated from position in file

    This is for repeatability when refering people to bnodes in a document.
    This has diagnostic uses less formally, as it should point one to which
    bnode the arbitrary identifier actually is. It gives the
    line and character number of the '[' charcacter or path character
    which introduced the blank node. The first blank node is boringly
    _L1C1. It used to be used only for tracking, but for tests in general
    it makes the canonical ordering of bnodes repeatable."""

    return "$(self._genPrefix)_L$(self.lines)C$(i - self.startOfLine + 1)"
end

function formula(self::SinkParser)::String
    return self._formula
end

function loadStream(self::SinkParser, stream::IOStream)::Any
    return loadBuf(self, read(stream, String))    # Not ideal
end
function loadBuf(self::SinkParser, buf::String)::Any
    """Parses a buffer and returns its top level formula"""
    startDoc!(self)
    feed!(self, buf)
    return endDoc!(self)     # self._formula
end
function feed!(self::SinkParser, s::String)::Nothing
    """Feed an octet stream tothe parser

    if BadSyntax is raised, the string
    passed in the exception object is the
    remainder after any statements have been parsed.
    So if there is more data to feed to the
    parser, it should be straightforward to recover."""

    i::Int = 1
    j::Int = 0
    while i >= 1
        j = skipSpace(self, s, i)
        j < 1 && return
        i = directiveOrStatement(self, s, j)
        #print("# next char: %s" % s[j])
        i < 1 && BadSyntax(self, s, j,
                           "expected directive or statement")
    end
end
function directiveOrStatement(self::SinkParser, argstr::String, h::Int)::Int
    i::Int = skipSpace(self, argstr, h)
    i < 1 && return i  # eof
    if self.turtle
        j = sparqlDirective(self, argstr, i)
        j >= 1 && return j
    end
    if (!self.ntriples)
        j = directive(self, argstr, i)
        j >= 1 && return checkDot(self, argstr, j)
    end
    j = statement(self, argstr, i)
    j >= 1 && return checkDot(self, argstr, j)
    return j
end

    # @@I18N
    # _namechars = string.lowercase + string.uppercase + string.digits + '_-'

function tok(self::SinkParser, tok::String, argstr::String, i::Int; colon::Bool=false)::Int
    """Check for keyword.  Space must have been stripped on entry and
    we must not be at end of file.

    if colon, then keyword followed by colon is ok
    (@prefix:<blah> is ok, rdf:type shortcut a must be followed by ws)
    """
    if occursin(tok[1], _notNameChars)  # not for punctuation
        error("Token cannot have punctionation in it.")
    end
    if argstr[i] == '@'
        i = i + 1
    else
        if !(tok in self.keywords)
            return 0  # No, this has neither keywords declaration nor "@"
        end
    end
    tok_len = length(tok)
    if (i+tok_len) > lastindex(argstr)
        # we don't want to scan past the end of the file
        return 0
    end
    try
        if (argstr[i:i + tok_len - 1] == tok
            && ( argstr[i + tok_len] in _notKeywordsChars)
            || (colon && argstr[i+tok_len] == ':'))
            i = i + tok_len
            return i
        else
            return 0
        end
    catch e
        if isa(e, StringIndexError)
            # tok tried to index into a multi-byte char.
            # no tokens are multi-byte, so lets just return
            return 0
        else
            rethrow(e)
        end
    end
end

function sparqlTok(self::SinkParser, tok::String, argstr::String, i::Int)::Int
    """Check for SPARQL keyword.  Space must have been stripped on entry
    and we must not be at end of file.
    Case insensitive and not preceeded by @
    """

    if occursin(tok[1], _notNameChars)  # not for punctuation
        error("Sparql Token cannot have punctionation in it.")
    end
    tok_len = length(tok)
    if (i+tok_len) > lastindex(argstr)
        # we don't want to scan past the end of the file
        return 0
    end
    if (lowercase(argstr[i:i + tok_len - 1]) == lowercase(tok)
            && (argstr[i + tok_len] in _notQNameChars))
        i = i + tok_len
        return i
    else
        return 0
    end    
end

function directive(self::SinkParser, argstr::String, i::Int)::Int
    j::Int = skipSpace(self, argstr, i)
    j < 1 && return j  # eof
    res = []

    j = tok(self, "bind", argstr, i)         # implied "#". Obsolete.
    j > 1 && BadSyntax(self, argstr, i, "keyword @bind is obsolete: use @prefix")
    
    j = tok(self, "keywords", argstr, i)
    if j > 1
        if self.turtle
            BadSyntax(self, argstr, i, "Found '@keywords' when in Turtle mode.")
        end
        i = commaSeparatedList(self, argstr, j, res, bareWord)
        i < 1 && BadSyntax(self, argstr, i,
                           "'@keywords' needs comma separated list of words")
        setKeywords(self, res[1:end])
        return i
    end
    j = tok(self, "forAll", argstr, i)
    if j > 1
        if self.turtle
            BadSyntax(self, argstr, i, "Found '@forAll' when in Turtle mode.")
        end
        i = commaSeparatedList(self, argstr, j, res, uri_ref2)
        i < 1 && BadSyntax(self, argstr, i,
                           "Bad variable list after @forAll")
        for x in res
                # self._context.declareUniversal(x)
            if !(x in self._variables) || (x in self._parentVariables)
                self._variables[x] = newUniversal(self._context, x)
            end
        end
        return i
    end
    j = tok(self, "forSome", argstr, i)
    if j > 1
        if self.turtle
            BadSyntax(self, argstr, i, "Found '@forSome' when in Turtle mode.")
        end
        i = commaSeparatedList(self, argstr, j, res, uri_ref2)
        i < 1 && BadSyntax(self, argstr, i,
                           "Bad variable list after @forSome")
        for x in res
            declareExistential(self._context, x)
        end
        return i
    end
    j = tok(self, "prefix", argstr, i, colon=true)    # no implied "#"
    if j >= 1
        t::Array{Tuple{String,String}} = []
        i = qname(self, argstr, j, t)
        i < 1 && BadSyntax(self, argstr, j,
                           "expected qname after @prefix")
        q_pfx, t_ln = t[1]
        t_ln != "" && BadSyntax(self, argstr, j,
                      "prefix declaration must not include a localname with the namespace")
        uri_ref::Array{Union{BNode, Variable, URIRef}, 1} = []                   
        j = uri_ref2(self, argstr, i, uri_ref)
        j < 1 && BadSyntax(self, argstr, i,
                           "expected <uriref> after @prefix _qname_")
        ns = uriOf(self, uri_ref[1])

        if self._baseURI !== nothing
            ns = join(self._baseURI, ns)
        elseif !(occursin(":", ns))
            BadSyntax(self, argstr, j,
                "With no base URI, cannot use " +
                "relative URI in @prefix <" + ns + ">")
        end
        if !(occursin(":", ns))  # must be absolute
            error("@prefix Namespace must be absolute!")
        end
        self._bindings[q_pfx] = ns
        bind(self, q_pfx, hexify(ns))
        return j
    end
    j = tok(self, "base", argstr, i)
    if j >= 1
        uri_refs::Array{Union{BNode, Variable, URIRef}, 1} = []
        i = uri_ref2(self, argstr, j, uri_refs)
        i < 1 && BadSyntax(self, argstr, j,
                           "expected <uri> after @base ")
        ns = uriOf(self, uri_refs[1])
        
        if (self._baseURI !== nothing)
            ns = join(self._baseURI, ns)
        else
            BadSyntax(self, argstr, j,
                "With no previous base URI, cannot use " +
                "relative URI in @base  <" + ns + ">")
        end
        if !(occursin(":", ns))  # must be absolute
            error("@base Namespace must be absolute!")
        end
        self._baseURI = ns
        return i
    end
    return 0   # Not a directive, could be something else.
end
function sparqlDirective(self::SinkParser, argstr::String, i::Int)::Int
    """
    turtle and trig support BASE/PREFIX without @ and without
    terminating .
    """

    j::Int = skipSpace(self, argstr, i)
    j < 1 && return j  # eof
    j = sparqlTok(self, "PREFIX", argstr, i)
    if j >= 1
        t::Array{Tuple{String,String}} = []
        i = qname(self, argstr, j, t)
        i < 1 && BadSyntax(self, argstr, j,
                           "expected qname after PREFIX")
        q_pfx, t_ln = t[1]
        t_ln != "" && BadSyntax(self, argstr, j,
                      "PREFIX declaration must not include a localname with the namespace")
        uri_ref::Array{Union{BNode, Variable, URIRef}, 1} = []                    
        j = uri_ref2(self, argstr, i, uri_ref)
        j < 1 && BadSyntax(self, argstr, i,
                           "expected <uriref> after PREFIX _qname_")
        ns = uriOf(self, uri_ref[1])

        if self._baseURI !== nothing
            ns = join(self._baseURI, ns)
        elseif !occursin(":", ns)
            BadSyntax(self, argstr, j,
                "With no base URI, cannot use " +
                "relative URI in PREFIX <" + ns + ">")
        end
        if !occursin(":", ns)  # must be absolute
            error("PREFIX Namespace must be absolute!")
        end
        self._bindings[q_pfx] = ns
        bind(self, q_pfx, hexify(ns))
        return j
    end
    j = sparqlTok(self, "BASE", argstr, i)
    if j >= 1
        uri_refs::Array{Union{BNode, Variable, URIRef}, 1} = []   
        i = uri_ref2(self, argstr, j, uri_refs)
        i < 1 && BadSyntax(self, argstr, j,
                          "expected <uri> after BASE ")
        ns = uriOf(self, uri_refs[1])
        
        if self._baseURI != nothing
            ns = join(self._baseURI, ns)
        else
            BadSyntax(self, argstr, j,
                "With no previous base URI, cannot use " +
                "relative URI in BASE  <" + ns + ">")
        end
        if !occursin(":", ns)  # must be absolute
            error("BASE Namespace must be absolute!")
        end
        self._baseURI = ns
        return i
    end
    return 0   # Not a directive, could be something else.
end

function bind(self::SinkParser, qn::String, uri::String)::Nothing
    if qn == ""
        _ = setDefaultNamespace(self._store, uri)
        return
    else
        bind(self._store, qn, uri)
    end
end

function setKeywords(self::SinkParser, k::Union{Nothing, Array{String, 1}})
    "Takes a Array of Strings"
    if k == nothing
        self.keywordsSet = false
    else
        self.keywords = k
        self.keywordsSet = true
    end
end

function startDoc!(self::SinkParser)
    startDoc(self._store, self._formula)
end

function endDoc!(self::SinkParser)
    """Signal end of document and stop parsing. returns formula"""
    endDoc(self._store, self._formula)   # don't canonicalize yet
    return self._formula
end

function makeStatement(self::SinkParser, quadruple)::Nothing
        # $$$$$$$$$$$$$$$$$$$$$
        # print "# Parser output: ", `quadruple`
    makeStatement(self._store, quadruple)
end
    
function statement(self::SinkParser, argstr::String, i::Int)::Int
    r::Array{NodeOrLiteralType, 1} = []
    i = subject(self, argstr, i, r)
    i < 1 && return i  # No object (start of statement) found
    j = propertyList(self, argstr, i, r[1])
    j < 1 && BadSyntax(self, argstr, i, "expected propertylist")
    return j
end

function subject(self::SinkParser, argstr::String, i::Int, res::Array{NodeOrLiteralType, 1})::Int
    # Allow literal for subject - extends RDF
    j::Int = item(self, argstr, i, res)
    j < 1 && return j
    subjectItem = res[1]
    # But still forbid boolean keywords for subject
    isa(subjectItem, Bool) && BadSyntax(self, argstr, i, "Found a boolean keyword in the Subject position.")
    return j
end
function verb(self::SinkParser, argstr::String, i::Int, res::Array{Tuple{String, Identifier}, 1})::Int
    """ has _prop_
    is _prop_ of
    a
    =
    _prop_
    >- prop ->
    <- prop -<
    _operator_"""

    j::Int = skipSpace(self, argstr, i)
    j < 1 && return j  # eof
    
    r::Array{Identifier, 1} = []
    j = tok(self, "has", argstr, i)
    if j >= 1
        self.ntriples && BadSyntax(self, argstr, i, "Found 'has' keyword in NTriples mode")
        self.turtle && BadSyntax(self, argstr, i, "Found 'has' keyword in Turtle mode")
        i = pred(self, argstr, j, r)
        i < 1 && BadSyntax(self, argstr, j,
                    "expected pred after 'has'")
        push!(res, ("->", r[1]))
        return i
    end
    j = tok(self, "is", argstr, i)
    if j >= 1
        self.ntriples && BadSyntax(self, argstr, i, "Found 'is' keyword in NTriples mode")
        self.turtle && BadSyntax(self, argstr, i, "Found 'is' keyword in Turtle mode")
        i = pred(self, argstr, j, r)
        i < 1 && BadSyntax(self, argstr, j,
                        "expected <pred> after 'is'")
        j = skipSpace(self, argstr, i)
        j < 1 && BadSyntax(self, argstr, i,
                        "End of file found, expected pred after 'is'")
        i = j
        j = tok(self, "of", argstr, i)
        j < 1 && BadSyntax(self, argstr, i,
                        "expected 'of' after 'is' <pred>")
        push!(res, ("<-", r[1]))
        return j
    end
    j = tok(self, "a", argstr, i)
    if j >= 1
        push!(res, ("->", RDF_type))
        return j
    end
    if argstr[i:i + 1] == "<="
        self.ntriples && BadSyntax(self, argstr, i,
                                    "Found '<=' in NTriples mode. ")
        self.turtle && BadSyntax(self, argstr, i,
                                    "Found '<=' in Turtle mode. ")
        push!(res, ("<-", newSymbol(self._store, Logic_NS * "implies")))
        return i + 2
    end
    if argstr[i:i] == "="
        self.ntriples && BadSyntax(self, argstr, i, "Found '=' in NTriples mode")
        self.turtle && BadSyntax(self, argstr, i, "Found '=' in Turtle mode")
        if argstr[i + 1:i + 1] == ">"
            push!(res, ("->", newSymbol(self._store, Logic_NS * "implies")))
            return i + 2
        end
        push!(res, ("->", DAML_sameAs))
        return i + 1
    end
    if argstr[i:i + 1] == ":="
        self.ntriples && BadSyntax(self, argstr, i, "Found ':=' in NTriples mode")
        self.turtle && BadSyntax(self, argstr, i, "Found ':=' in Turtle mode")
            # patch file relates two formulae, uses this    @@ really?
        push!(res, ("->", Logic_NS * "becomes"))
        return i + 2
    end
    j = pred(self, argstr, i, r)
    if j >= 1
        push!(res, ("->", r[1]))
        return j
    end
    (argstr[i:i + 1] == ">-" || argstr[i:i + 1] == "<-") && BadSyntax(self, argstr, j,
                                                            ">- ... -> syntax is obsolete.")
    return 0
end

function pred(self::SinkParser, argstr::String, i::Int, res::Array{Identifier, 1})::Int
    i_res1::Array{NodeOrLiteralType, 1} = []
    j::Int = item(self, argstr, i, i_res1)
    if j >= 1
        item1 = i_res1[1]
        isa(item1, BNode) && BadSyntax(self, argstr, i, "Got a Blank Node in predicate location.")
        isa(item1, Literal) && BadSyntax(self, argstr, i, "Got a Literal in predicate location.")
        if isa(item1, Identifier)
            push!(res, item1)
        else
            BadSyntax(self, argstr, i, "Got a non-Identifier in predicate location.")
        end
    end
    return j
end

item(self::SinkParser, argstr::String, i::Int, res::Array{NodeOrLiteralType, 1})::Int = path(self, argstr, i, res)

function object(self::SinkParser, argstr::String, i::Int, res::Array{NodeOrLiteralType, 1})::Int
    j::Int = item(self, argstr, i, res)
    j >= 1 && return j
    j = skipSpace(self, argstr, i)
    j < 1 && return 0
    i = j
    if argstr[i] in self.stringDelimiters
        error("Got here?!")
        if argstr[i:i + 2] == argstr[i] ^ 3
            delim = argstr[i] ^ 3
        else
            delim = string(argstr[i])
        end
        i = i + length(delim)
        j, s = strconst(self, argstr, i, delim)
        push!(res, newLiteral(self._store, s))
        return j
    else
        return 0
    end
end

function nodeOrLiteral(self::SinkParser, argstr::String, i::Int, res::Array{NodeOrLiteralType, 1})::Int
    n_res::Array{Identifier, 1} = []
    j::Int = node(self, argstr, i, n_res)
    if j >= 1
        push!(res, n_res[1])
        return j
    end
    startline = self.lines  # Remember where for error messages
    j = skipSpace(self, argstr, i)
    j < 1 && return 0 # Hit EOF
    i = j

    ch = argstr[i]
    if occursin(ch, "-+0987654321.")
        self.ntriples && throw(
                         BadSyntax(self._thisDoc, startline, argstr, i, "NTriples literals must be quoted."))
        m = match(exponent_syntax, argstr, i, PCRE.ANCHORED)
        if m !== nothing
            j = m.offset + length(m.match)
            push!(res, parse(Float64, argstr[i:j-1]))
            return j
        end
        m = match(decimal_syntax, argstr, i, PCRE.ANCHORED)
        if m !== nothing
            j = m.offset + length(m.match)
            push!(res, parse(Decimal,argstr[i:j-1]))
            return j
        end
        m = match(integer_syntax, argstr, i, PCRE.ANCHORED)
        if m !== nothing
            j = m.offset + length(m.match)
            push!(res, parse(Int64, argstr[i:j-1]))
            return j
        end
        # return 0  ## or fall through?
    elseif ch == 't'
        j = tok(self, "true", argstr, i)
        if j >= 1
            push!(res, true)
            return j
        end
    elseif ch == 'f'
        j = tok(self, "false", argstr, i)
        if j >= 1
            push!(res, false)
            return j
        end
    end
    if ch in self.stringDelimiters #cannot use occursin here.
        delim = try
            if argstr[i:i + 2] == argstr[i] ^ 3
                self.ntriples && throw(
                         BadSyntax(self._thisDoc, startline, argstr, i, "Long quote delimiters are invalid in NTriples mode."))
                argstr[i] ^ 3
            else
                string(argstr[i])
            end
        catch e
            if isa(e, StringIndexError)
                # we indexed into a multi-byte char, while looking for a triple delimiter.
                string(argstr[i])
            else
                rethrow(e)
            end
        end
        i = i + length(delim)
        dt::Union{URIRef, Nothing} = nothing
        j, s = strconst(self, argstr, i, delim)
        lang::Union{String, Nothing} = nothing
        if argstr[j:j] == "@"  # Language?
            m = match(langcode, argstr, j + 1, PCRE.ANCHORED)
            m === nothing && throw(BadSyntax(
                    self._thisDoc, startline, argstr, i,
                    "Bad language code syntax on string " *
                    "literal, after @"))
            i = m.offset + length(m.match)
            lang = argstr[j + 1:i-1]
            j = i
        end
        if argstr[j:j + 1] == "^^"
            res2::Array{Union{BNode, URIRef, Variable},1} = []
            j = uri_ref2(self, argstr, j + 2, res2)  # Read datatype URI
            dt = res2[1]
        end
        if lang !== nothing && dt !== nothing
            throw(BadSyntax(self._thisDoc, startline, argstr, i,
                    "Literal cannot have both a datatype and language code."))
        end
        push!(res, newLiteral(self._store, s, dt, lang))
        return j
    else
        return 0
    end
end

function blankNode(self::SinkParser; identifier::Union{Nothing, String}=nothing)
    return newBlankNode(self._store, self._context; uri=identifier)
end

function path(self::SinkParser, argstr::String, i::Int, res::Array{NodeOrLiteralType, 1})::Int
    """Parse the path production.
    """
    j::Int = nodeOrLiteral(self, argstr, i, res)
    j < 1 && return j   # nope
    (self.ntriples || self.turtle) && return j  #Don't even try to traverse an N3 path
    while occursin(argstr[j], "!^")   # no spaces, must follow exactly (?)
        ch::Char = argstr[j]
        subj = pop!(res) #TODO: Assume this is the last element?
        obj = blankNode(self, identifier=here(self,j))
        old_j = j
        j = node(self, argstr, j + 1, res)
        j < 1 && BadSyntax(self, argstr, old_j,
                    "EOF found in middle of path syntax")
        pred = pop!(res)
        if ch == '^'  # Reverse traverse
            makeStatement(self, (self._context, pred, obj, subj))
        else
            makeStatement(self, (self._context, pred, subj, obj))
        end
        push!(res, obj)
    end
    return j
end

function anonymousNode(self::SinkParser, ln::String)::BNode
    """Remember or generate a term for one of these _: anonymous nodes"""
    term::Union{Nothing, Union{URIRef, Literal, BNode}} = get(self._anonymousNodes, ln, nothing)
    term !== nothing && return term
    term = newBlankNode(self._store, self._context)
    self._anonymousNodes[ln] = term
    return term
end

function node(self::SinkParser, argstr::String, i::Int, res::Array{Identifier, 1}, subjectAlready::Union{Nothing, Any}=nothing)
    """Parse the <node> production.
    """
    subj::Union{Nothing, Any} = subjectAlready

    j::Int = skipSpace(self, argstr, i)
    j < 1 && return j  # eof
    i = j
    ch::Char = argstr[i]   # Quick 1-character checks first:

    if ch == '['
        bnodeID = here(self, i)
        j = skipSpace(self, argstr, i + 1)
        j < 1 && BadSyntax(self, argstr, i,
                "EOF after '['")
        # Hack for "is" binding name to anon node
        if argstr[j:j] == "="
            self.turtle && BadSyntax(self, argstr, j, "Found '[=' or '[ =' when in turtle mode.")
            i = j + 1
            objs = []
            j = objectList(self, argstr, i, objs)
            j < 1 && BadSyntax(self, argstr, i,
                    "objectList expected after [= ")
            subj = objs[1]
            if length(objs) > 1
                for obj in objs
                    makeStatement(self, (self._context,
                                        DAML_sameAs, subj, obj))
                end
            end
            j = skipSpace(self, argstr, j)
            j < 1 && BadSyntax(self, argstr, i,
                            "EOF when objectList expected after [ = ")
            if argstr[j:j] == ";"
                j = j + 1
            end
        end
        if subj === nothing
            subj = blankNode(self, identifier=bnodeID)
        end
        i = propertyList(self, argstr, j, subj)
        i < 1 && BadSyntax(self, argstr, j,
                    "propertyList expected")

        j = skipSpace(self, argstr, i)
        j < 1 && BadSyntax(self, argstr, i,
                    "EOF when ']' expected after [ <propertyList>")
        argstr[j:j] != "]" && BadSyntax(self, argstr, j,
                                    "']' expected")
        push!(res, subj)
        return j + 1
    end
    if (!self.turtle) && (ch == '{')
        # if self.turtle:
        #     BadSyntax(self, argstr, i,
        #                     "found '{' while in Turtle mode, Formulas not supported!")
        ch2::Char = argstr[i + 1]
        if ch2 == '\$'
            # a set
            i += 1
            j = i + 1
            _list::Array{Identifier, 1} = []
            first_run = true
            while true
                i = skipSpace(self, argstr, j)
                i < 1 && BadSyntax(self, argstr, i,
                            "needed '\$}', found end.")
                if argstr[i:i + 1] == "\$}"
                    j = i + 2
                    break
                end

                if !first_run
                    if argstr[i:i] == ","
                        i += 1
                    else
                        BadSyntax(self, argstr, i, "expected: ','")
                    end
                else
                    first_run = false
                end
                items::Array{Identifier, 1} = []
                j = item(self, argstr, i, items)  # @@@@@ should be path, was object
                j < 1 && BadSyntax(self, argstr, i,
                            "expected item in set or '\$}'")
                push!(_list, intern(self._store, items[1]))
            end    
            push!(res, newSet(self._store, _list, self._context))
            return j
        else
            error("Parsing formula is not implemented!")
        end
    end
    if ch == '('
        thing_type = newList
        ch2 = argstr[i + 1]
        if ch2 == '\$'
            thing_type = newSet
            i += 1
        end
        j = i + 1

        _list2::Array{NodeOrLiteralType, 1} = []
        while true
            i = skipSpace(self, argstr, j)
            i < 1 && BadSyntax(self, 
                        argstr, i, "needed ')', found end.")
            if argstr[i:i] == ")"
                j = i + 1
                break
            end

            items2::Array{NodeOrLiteralType, 1} = []
            j = item(self, argstr, i, items2)  # @@@@@ should be path, was object
            j < 1 && BadSyntax(self, argstr, i,
                        "expected item in list or ')'")
            push!(_list2, intern(self._store, items2[1]))
        end
        push!(res, thing_type(self._store, _list2, self._context))
        return j
    end
    j = tok(self, "this", argstr, i)    # This context
    j >= 1 && BadSyntax(self, argstr, i,
                "Keyword 'this' was ancient N3. Now use " +
                "@forSome and @forAll keywords.")

    if subj === nothing  # If this can be a named node, then check for a name.
        res2::Array{Union{BNode, Variable, URIRef}, 1} = []
        j = uri_ref2(self, argstr, i, res2)
        if j >= 1
            push!(res, res2[1])
            return j
        end
    end
    return 0
end

function propertyList(self::SinkParser, argstr::String, i::Int, subj::NodeOrLiteralType)::Int
    """Parse property list
    Leaves the terminating punctuation in the buffer
    """
    while true
        j::Int = 0
        while true # skip repeat ;
            j = skipSpace(self, argstr, i)
            j < 1 && BadSyntax(self, argstr, i,
                                "EOF found when expected verb in property list")
            argstr[j] != ';' && break
            if self.ntriples
                BadSyntax(self, argstr, j, "Found properties list with ';' in NTriples mode")
            end
            i = j+1
        end
        if argstr[j:j + 1] == ":-"
            self.ntriples && BadSyntax(self, argstr, j, "Found in ':-' in NTriples mode")
            self.turtle && BadSyntax(self, argstr, j, "Found in ':-' in Turtle mode")
            i = j + 2
            res = []
            j = node(self, argstr, i, res, subj)
            j < 1 && BadSyntax(self, argstr, i,
                        "bad {} or () or [] node after :- ")
            i = j
            continue
        end
        i = j
        v::Array{Tuple{String, Identifier}, 1} = []
        j = verb(self, argstr, i, v)
        if j <= 1
            return i  # void but valid
        end
        objs::Array{NodeOrLiteralType, 1} = []
        i = objectList(self, argstr, j, objs)
        i < 1 && BadSyntax(self, argstr, j,
                    "objectList expected")
        for obj in objs
            dira, sym = v[1]
            if dira == "->"
                makeStatement(self, (self._context, sym, subj, obj))
            else
                makeStatement(self, (self._context, sym, obj, subj))
            end
        end
        old_j = j
        j = skipSpace(self, argstr, i)
        j < 1 && BadSyntax(self, argstr, old_j,
                    "EOF found in propertiesObjects list. Expecting ';' or '.'")
        argstr[j] != ';' && return j
        i = j + 1  # skip semicolon and continue
    end
end

function commaSeparatedList(self::SinkParser, argstr::String, j::Int, res, what)::Int
    """return value: -1 bad syntax; >1 new position in argstr
    res has things found appended
#     """
    i::Int = skipSpace(self, argstr, j)
    i < 1 && BadSyntax(self, argstr, j,
                "EOF found expecting comma sep list")
    argstr[i] == '.' && return j  # empty list is OK
    i = what(self, argstr, i, res)
    i < 1 && return 0
    while true
        j = skipSpace(self, argstr, i)
        j < 1 && return j  # eof
        ch::Char = argstr[j]
        if ch != ','
            if ch != '.'
                return 0
            end
            return j  # Found but not swallowed "."
        end
        i = what(self, argstr, j + 1, res)
        i < 1 && BadSyntax(self, argstr, i, "bad list content")
    end
end

function objectList(self::SinkParser, argstr::String, i::Int, res::Array{NodeOrLiteralType, 1})::Int
    j::Int = object(self, argstr, i, res)
    j < 1 && return 0
    self.ntriples && return j # don't look for more objects in NTriples mode
    i = j
    while true
        j = skipSpace(self, argstr, i)
        j < 1 && BadSyntax(self, argstr, i,
                 "EOF found after object. Expecting '.', ';' or ','.")
        if argstr[j:j] != ","
            return j     # Found something else!
        end
        i = object(self, argstr, j + 1, res)
        i < 1 && return i
    end
end

function checkDot(self::SinkParser, argstr::String, i::Int)::Int
    j = skipSpace(self, argstr, i)
    j < 1 && return j  # eof
    if argstr[j:j] == "."
        return j + 1   # skip
    end
    if argstr[j:j] == "}"
        return j  # don't skip it
    end
    if argstr[j:j] == "]"
        return j # don't skip it
    end
    BadSyntax(self, argstr, j,
        "expected '.' or '}' or ']' at end of statement")
end

function uri_ref2(self::SinkParser, argstr::String, i::Int, res::Array{Union{BNode, Variable, URIRef}, 1})::Int
    """Generate uri from n3 representation.

    Note that the RDF convention of directly concatenating
    NS and local name is now used though I prefer inserting a '#'
    to make the namesapces look more like what XML folks expect.
    """
    qn::Array{Tuple{String,String}} = []
    j = qname(self, argstr, i, qn)
    if j >= 1
        pfx, ln = qn[1]
        if pfx === nothing
            error("Can't get here?")
            ns = self._baseURI * "#"
        else
            ns = try
                self._bindings[pfx]
            catch e
                if isa(e, KeyError)
                    if pfx == "_"  # Magic prefix 2001/05/30
                        push!(res, anonymousNode(self, ln))
                        return j
                    end
                    if (!self.turtle) && (pfx == "")
                        join(self._baseURI === nothing ? "" : self._baseURI, "#")
                    else
                        BadSyntax(self, argstr, i, "Prefix $(pfx) not bound")
                    end
                else
                    rethrow(e)
                end
            end
        end
        symb = newSymbol(self._store, ns * ln)
        if symb in keys(self._variables)
            push!(res, self._variables[symb])
        else
            push!(res, symb)  # @@@ "#" CONVENTION
        end
        return j
    end
    i = skipSpace(self, argstr, i)
    i < 1 && return 0
    if argstr[i] == '?'
        self.ntriples && BadSyntax(self, argstr, i, "?variables are not allowed in NTriples")
        v = []
        j = self.variable(argstr, i, v)
        if j > 1 # Forget varibles as a class, only in context.
            push!(res, v[1])
            return j
        end    
        return 0
    elseif argstr[i] == '<'
        i = i + 1
        st = i
        ch::Char = '\0'
        while i < lastindex(argstr)
            try
                ch = argstr[i]
            catch StringIndexError
                i = nextind[i]
                continue
            end
            if occursin(ch, " \0\n\r\t<")
                BadSyntax(self, argstr, i, "Bad char $(Int(ch)) in URIRef")
            end
            if ch == '>'
                prev_char_ind = thisind(argstr, i-1)
                uref = argstr[st:prev_char_ind]  # the join should dealt with "":
                # expand unicode escapes
                uref = replace(uref, unicodeEscape8 => unicodeExpand)
                uref = replace(uref, unicodeEscape4 => unicodeExpand)
                if occursin("\\u", uref) || occursin("\\U", uref)
                    # an escape sequence that doesn't match unicodeEscape4 or 8.
                    BadSyntax(self, argstr, i, "Bad escape sequence in URIRef")
                end
                if occursin("\\n", uref) || occursin("\\r", uref) || occursin("\\/", uref)
                    # an escape sequence that doesn't match unicodeEscape4 or 8.
                    BadSyntax(self, argstr, i, "Character escapes not allowed in URIRef")
                end
                if self.ntriples
                    !occursin(":", uref) && BadSyntax(self, argstr, i, "Relative URIs not allowed in NTriples mode.")
                else
                    if self._baseURI !== nothing
                        uref = join(self._baseURI, uref)  # was: uripath.join
                    else
                        !occursin(":", uref) && error("With no base URI, cannot deal with relative URIs")
                    end  
                end
                if argstr[prev_char_ind] == '#' && !(uref[end] == '#')
                    uref = uref * "#"  # She meant it! Weirdness in urlparse?
                end 
                for ch2 in " \0\n\r\t<>{}"
                    if occursin(ch2, uref)
                        InvalidNode(self, argstr, i, "Bad char $(Int(ch2)) in URIRef after evaluation.")
                    end
                end
                symb = newSymbol(self._store, uref)
                if symb in keys(self._variables)
                    push!(res, self._variables[symb])
                else
                    push!(res, symb)
                end    
                return i + 1
            end    
            i = i + 1
        end    
        BadSyntax(self, argstr, j, "unterminated URI reference")

    elseif self.keywordsSet
        v = []
        j = bareWord(self, argstr, i, v)
        j < 1 && return 0       # Forget varibles as a class, only in context.
        if v[1] in self.keywords
            BadSyntax(self, argstr, i, "Keyword \"$(v[1])\" not allowed here.")
        end        
        push!(res, newSymbol(self._store, self._bindings[""] * v[1]))
        return j
    else
        return 0
    end
end
function skipSpace(self::SinkParser, argstr::String, i::Int)::Int
    """Skip white space, newlines and comments.
    return 0 if EOF, else position of first non-ws character"""
    while true
        m = match(eol, argstr, i, PCRE.ANCHORED)
        m === nothing && break
        self.lines = self.lines + 1
        i = m.offset + length(m.match)    # Point to first character unmatched
        self.startOfLine = i
    end
    m2 = match(ws, argstr, i, PCRE.ANCHORED)
    if m2 !== nothing
        i = m2.offset + length(m2.match)
    end
    m3 = match(eof, argstr, i, PCRE.ANCHORED)
    if m3 !== nothing
        return 0
    end
    return i
end

function variable(self::SinkParser, argstr::String, i::Int, res)::Int
    """     ?abc -> variable(:abc)
    """

    j = skipSpace(self, argstr, i)
    j < 1 && return 0

    (argstr[j:j] != "?") && return 0
    j = j + 1
    i = j
    (argstr[j] in "0123456789-") && BadSyntax(self, argstr, j,
            "Varible name can't start with '%s'" % argstr[j])
    ch::Char = '\0'
    while i <= lastindex(argstr)
        try
            ch = argstr[i]
        catch
            i = i + 1
            continue
        end    
        if !(occursin(ch, _notKeywordsChars))
            i = i + 1
        else
            break
        end
    end
    if self._parentContext === nothing
        varURI = newSymbol(self._store, self._baseURI * "#" * argstr[j:i])
        if !(varURI in self._variables)
            self._variables[varURI] = newUniversal(self._context, varURI)
        end
        push!(res, self._variables[varURI])
        return i
            # @@ was:
            # BadSyntax(self, argstr, j,
            #     "Can't use ?xxx syntax for variable in outermost level: %s"
            #     % argstr[j-1:i])
    end
    varURI = newSymbol(self._store, self._baseURI * "#" * argstr[j:i])
    if !(varURI in self._parentVariables)
        self._parentVariables[varURI] = newUniversal(self._parentContext,
            varURI, why=self._reason2)
    end
    push!(res, self._parentVariables[varURI])
    return i
end

function bareWord(self::SinkParser, argstr::String, i::Int, res)::Int
    """     abc -> :abc
    """
    j::Int = skipSpace(self, argstr, i)
    j < 1 && return 0

    if occursin(argstr[j], "0123456789-") || occursin(argstr[j], _notKeywordsChars)
        return 0
    end
    i = j
    while i <= lastindex(argstr)
        try
            ch = argstr[i]
        catch
            i = i + 1
            continue
        end    
        if !(occursin(ch, _notKeywordsChars))
            i = i + 1
        else
            break
        end
    end
    push!(res, argstr[j:i])
    return i
end

function qname(self::SinkParser, argstr::String, i::Int, res::Array{Tuple{String,String}})::Int
    """
    xyz:def -> ('xyz', 'def')
    If not in keywords and keywordsSet: def -> ('', 'def')
    :def -> ('', 'def')
    """

    i = skipSpace(self, argstr, i)
    i < 1 && return 0
    c::Char = argstr[i]
    occursin(c, "0123456789-+.") && return 0
    ln::String = ""
    if !occursin(c, _notNameChars)
        ln = string(c)
        i = nextind(argstr, i)
        while i <= lastindex(argstr)
            c = argstr[i]
            if !occursin(c, _notNameChars)
                ln = ln * c
                i = nextind(argstr, i)
            else
                break
            end
        end
        if argstr[thisind(argstr,i - 1)] == '.'  # qname cannot end with "."
            ln = ln[1:end-1]
            ln == "" && return 0
            i -= 1
        end
    end
    if (i < lastindex(argstr)) && (argstr[i] == ':')
        pfx = ln
        # bnodes names have different rules
        if pfx == "_"
            allowedChars = _notNameChars
        else
            allowedChars = _notQNameChars
        end
        i = nextind(argstr, i)
        first_char = argstr[i]
        if first_char in "-."
            throw(BadSyntax(self._thisDoc, self.lines, argstr, i,
                            "Localname cannot start with dash or fullstop"))
        end
        lastslash = false
        # start = i # TODO first char .
        ln = ""
        while i <= lastindex(argstr)
            c = argstr[i]
            if (!lastslash) && (c == '\\')
                lastslash = true
                i += 1
            elseif lastslash || !occursin(c, allowedChars)
                if lastslash
                    if !occursin(c, escapeChars)
                        throw(BadSyntax(self._thisDoc, self.lines, argstr, i,
                                        "illegal escape " * c))
                    end
                elseif c == '%'
                    if !occursin(argstr[i+1], hexChars) || !occursin(argstr[i+2], hexChars)
                        throw(BadSyntax(self._thisDoc, self.lines, argstr, i,
                                        "illegal hex escape % " * argstr[i+1:i+2]))
                    end
                end
                ln = ln * c
                i = nextind(argstr, i)
                lastslash = false
            else
                break
            end
        end
        lastslash && throw(BadSyntax(
                self._thisDoc, self.lines, argstr, i,
                "qname cannot end with \\"))

        if argstr[thisind(argstr, i-1)] == '.'
            # localname cannot end in .
            ln = ln[1:end-1]
            ln == "" && return 0
            i -= 1
        end
        push!(res, (pfx, ln))
        return i
    else   # delimiter was not ":"
        if (length(ln) > 0) && self.keywordsSet && !(ln in self.keywords)
            push!(res, ("", ln))
            return i
        end
        return 0
    end
end

function uriOf(self::SinkParser, sym)::String
    if isa(sym, Tuple)
        return sym[2]  # old system for --pipe
    end
        # return sym.uriref()  # cwm api
    return sym
end

function strconst(self::SinkParser, argstr::String, i::Int, delim::String)::Tuple{Int, String}
    """parse an N3 string constant delimited by delim.
       return index, val
    """
    delim1::Char = delim[1]
    delim2, delim3, delim4, delim5 = delim1 ^ 2, delim1 ^ 3, delim1 ^ 4, delim1 ^ 5
    delim1isdelim::Bool = (string(delim1) == delim)
    delim3isdelim::Bool = (!delim1isdelim) && (delim3 == delim)
    
    j::Int = i
    ustr::String = ""    # Empty unicode string
    startline = self.lines  # Remember where for error messages
    chchar::Char = '\0'
    lastindargstr = lastindex(argstr)
    while j <= lastindargstr
        chchar = try
            argstr[j]
        catch
            j = j + 1
            continue
        end
        if chchar == delim1
            if delim1isdelim  # done when delim is " or '
                i = j + 1
                return i, ustr
            elseif delim3isdelim  # done when delim is """ or ''' and, respectively ...
                if j+4 <= lastindargstr && argstr[j:j + 4] == delim5  # ... we have "" or '' before """
                    i = j + 5
                    ustr = ustr * delim2
                    return i, ustr
                end
                if j+3 <= lastindargstr && argstr[j:j + 3] == delim4  # ... we have " or ' before """
                    i = j + 4
                    ustr = ustr * delim1
                    return i, ustr
                end
                if j+2 <= lastindargstr && argstr[j:j + 2] == delim3  # current " or ' is part of delim
                    i = j + 3
                    return i, ustr
                end
                # we are inside of the string and current char is " or '
                j = j + 1
                ustr = ustr * delim1
                continue
            end
        end
        m = match(interesting, argstr, j)   # was argstr[j:].
        m === nothing && error("Quote expected in string at ^ in:\n$(argstr[j - 20:j-1])^$(argstr[j:j + 20])")  # at least need a quote
        i = m.offset
        try
            ustr = ustr * argstr[j:thisind(argstr, i-1)]
        catch e
            if isa(e, StringIndexError)
                err = ""
                for c in [j:i-1]
                    err = try err * argstr[c]
                    catch e
                        break
                    end
                end
                throw(BadSyntax(
                    self._thisDoc, startline, argstr, j,
                    "StringIndexError appending characters \"$(err)\" to string"))
            #elseif isa(e, UnicodeError)
            #    err = ""
            #    for c in argstr[j:i-1]
            #        err = err * @sprintf(" %02x",Int(c))
            #    end
            #    throw(BadSyntax(
            #        self._thisDoc, startline, argstr, j,
            #        "Unicode error appending characters" *
            #        err * " to string"))
            else
                println("$(typeof(e))")
                rethrow(e)
            end
        end
            # print "@@@ i = ",i, " j=",j, "m.end=", m.end()

        ch::Char = argstr[i]
        if ch == delim1
            j = i
            continue
        elseif (ch in ('\"', '\'')) && (ch != delim1)
            ustr = ustr * ch
            j = i + 1
            continue
        elseif ch in ('\r', '\n')
            delim1isdelim && throw(BadSyntax(
                    self._thisDoc, startline, argstr, i,
                    "newline found in string literal"))
            self.lines = self.lines + 1
            ustr = ustr * ch
            j = i + 1
            self.startOfLine = j
        elseif ch == '\\'
            j = i + 1
            try
                ch = argstr[j]
            catch    
                throw(BadSyntax(
                      self._thisDoc, startline, argstr, i,
                      "unterminated string literal (2)"))
            end
            k = findfirst(string(ch), "abfrtvn\\\"\'")
            if k !== nothing && k.start >= 1
                uch = "\a\b\f\r\t\v\n\\\"\'"[k]
                ustr = ustr * uch
                j = j + 1
            elseif ch == 'u'
                j, ch = uEscape(self, argstr, j + 1, startline)
                ustr = ustr * ch
            elseif ch == 'U'
                j, ch = UEscape(self, argstr, j + 1, startline)
                ustr = ustr * ch
            else
                BadSyntax(self, argstr, i, "Bad escape in string literal.")
            end
        end
    end
    BadSyntax(self, argstr, i, "unterminated string literal")
end

function _unicodeEscape(self::SinkParser, argstr::String, i::Int, startline, reg::Regex, n::Int, prefix::String)::Tuple{Int, Char}
    lastindex(argstr) < i+n && throw(BadSyntax(
                self._thisDoc, startline, argstr, i,
                "unterminated string literal(3)"))
    esc_string::String = "\\"*prefix*argstr[i:thisind(argstr,i+n-1)]
    p = try
        replace(esc_string, reg => unicodeExpand)
    catch e
        throw(BadSyntax(
            self._thisDoc, startline, argstr, i,
            "bad string literal hex escape: "*argstr[i:thisind(argstr, i+n-1)]))
    end
    if occursin("\\"*prefix, p)
        throw(BadSyntax(
        self._thisDoc, startline, argstr, i,
        "bad string literal unicode escape sequence: "*argstr[i:thisind(argstr, i+n-1)]))
    end
    if length(p) > 1
        throw(BadSyntax(
        self._thisDoc, startline, argstr, i,
        "unicode escape sequence expanded to >1 character: "*argstr[i:thisind(argstr, i+n-1)]))
    end
    return nextind(argstr, i+n-1), p[1]
    
end

uEscape(self::SinkParser, argstr::String, i::Int, startline)::Tuple{Int, Char} = _unicodeEscape(self, argstr, i, startline, unicodeEscape4, 4, "u")
UEscape(self::SinkParser, argstr::String, i::Int, startline)::Tuple{Int, Char} = _unicodeEscape(self, argstr, i, startline, unicodeEscape8, 8, "U")

# class BadSyntax(SyntaxError):
#     def __init__(self, uri, lines, argstr::String, i::Int, why):
#         self._str = argstr.encode(
#             'utf-8')  # Better go back to strings for errors
#         self._i = i
#         self._why = why
#         self.lines = lines
#         self._uri = uri
# 
#     def __str__(self):
#         argstr = self._str
#         i = self._i
#         st = 0
#         if i > 60:
#             pre = "..."
#             st = i - 60
#         else:
#             pre = ""
#         if len(argstr) - i > 60:
#             post = "..."
#         else:
#             post = ""
# 
#         return 'at line %i of <%s>:\nBad syntax (%s) at ^ in:\n"%s%s^%s%s"' \
#                % (self.lines + 1, self._uri, self._why, pre,
#                   argstr[st:i], argstr[i:i + 60], post)
# 
#     @property
#     def message(self):
#         return str(self)

###################################################
#
#  Utilities
#



function hexify(ustr::String)::String
    """Use URL encoding to return an ASCII string
    corresponding to the given UTF8 string

    >>> hexify("http://example/a b")
    %(b)s'http://example/a%%20b'

    """
     # s1=ustr.encode('utf-8')
    s = ""
    for ch in ustr   # .encode('utf-8')
        if Int(ch) > 126 || Int(ch) < 33
            ch = @sprintf("%%%02X", Int(ch))
        else
            ch = @sprintf("%c", Int(ch))
        end
        s = s * ch
    end
    return s
end

abstract type ParseSource end

getPublicId(self::ParseSource)::Union{Nothing, String} = nothing
getSystemId(self::ParseSource)::Union{Nothing, String} = nothing

struct ParseFileSource <: ParseSource
    filename::AbstractString
end
getPublicId(self::ParseFileSource)::Union{Nothing, String} = "file://"*self.filename
function getIOStream(self::ParseFileSource)::IOStream
    return open(self.filename, "r")
end

struct TurtleParser <: Parser
    """
    An RDFLib parser for Turtle

    See http://www.w3.org/TR/turtle/
    """
end
function parseN3(self::TurtleParser, source::ParseSource, graph::Graph; encoding::Union{String, Nothing}="utf-8", turtle::Bool=false, ntriples=false)::Nothing
    if encoding !== nothing && encoding != "utf-8"
        error("N3/Turtle/NT files are always utf-8 encoded, I was passed: $(encoding)")
    end
    sink = RDFSink(graph)
    useUri = getPublicId(source)
    if useUri === nothing
       useUri = getSystemId(source)
       if useUri === nothing
          useUri = "" 
       end
    end
    
    #baseURI = absolutize(graph, useUri) ##TODO: Absolutize
    baseURI = useUri
    p = SinkParser(sink, baseURI=baseURI, turtle=turtle, ntriples=ntriples)
    stream = getIOStream(source)
    loadStream(p, stream)
    Base.close(stream)
    for (prefix, namespace) in p._bindings
        RDFLibGraph.bind(graph, prefix, namespace)
    end
end

struct N3Parser <: Parser
    """
    An RDFLib parser for Notation3

    See http://www.w3.org/DesignIssues/Notation3.html

    """
end
function parseN3(self::N3Parser, source::ParseSource, graph::Graph; encoding::Union{String, Nothing}="utf-8")::Nothing
        # we're currently being handed a Graph, not a ConjunctiveGraph
    @assert graph.store.context_aware  # is this implied by formula_aware
    @assert graph.store.formula_aware

    conj_graph = ConjunctiveGraph(store=graph.store)
    conj_graph.default_context = graph  # TODO: CG __init__ should have a
                                        # default_context arg
        # TODO: update N3Processor so that it can use conj_graph as the sink
    conj_graph.namespace_manager = graph.namespace_manager

    parseN3(TurtleParser(), source, conj_graph, encoding=encoding, turtle=false, ntriples=false)
end
function parseN3(source::ParseSource, graph::Graph; encoding::Union{String, Nothing}="utf-8", turtle::Bool=false, ntriples::Bool=false)::Nothing
    ntriples && return parseN3(TurtleParser(), source, graph, encoding=encoding, turtle=true, ntriples=true)
    turtle && return parseN3(TurtleParser(), source, graph, encoding=encoding, turtle=true)
    return parseN3(N3Parser(), source, graph, encoding=encoding)
end
function parsefile(source::String, graph::Graph; encoding::Union{String, Nothing}="utf-8", turtle::Bool=true, ntriples::Bool=false)::Nothing
    file_source = ParseFileSource(source)
    parseN3(file_source, graph, encoding=encoding, turtle=turtle, ntriples=ntriples)
end
function parsefile(source::URIRef, graph::Graph; encoding::Union{String, Nothing}="utf-8", turtle::Bool=false, ntriples::Bool=false)::Nothing
    source_filename::String = convert(String, source)
    if startswith(source_filename, "file://")
        source_filename = source_filename[7:end]
    elseif startswith(source_filename, "http://") || startswith(source_filename, "https://")
        error("Cannot parse http or https files yet")
    end
    return parsefile(source_filename, graph, encoding=encoding, turtle=turtle, ntriples=ntriples)
end
end #End Module RDFLibN3
