module SPARQLParser
    using Printf
    import Base.PCRE

    export SPARQLUpdate, SPARQLQuery, Parser, BadSyntax, parse

    const PN_CHARS_BASE = "A-Za-z\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD"
    const PN_CHARS_U = PN_CHARS_BASE*'_'
    const PN_CHARS = PN_CHARS_U*"0-9\\u00B7\\u0300-\\u036F\\u203F-\\u2040\\-}"
    println("[$(PN_CHARS_BASE)](?:(?:[$(PN_CHARS)\\.])*[$(PN_CHARS)])?")
    const PN_PREFIX = Regex("[$(PN_CHARS_BASE)](?:(?:[$(PN_CHARS)\\.])*[$(PN_CHARS)])?")
    const VAR_NAME = "(?:[$(PN_CHARS_U)0-9])(?:[$(PN_CHARS_U)0-9\\u00B7\\u0300-\\u036F\\u203F\\u2040])*"
    const VAR1 = "\\?$(VAR_NAME)"
    const VAR2 = "\\\$$(VAR_NAME)"
    const VAR = "(?:$(VAR1)|$(VAR2))"
    const IRIREF_RE = r"<([^<>\"{}|^`\]\-[\x00-\x20])*>"i


    struct IRIRef
        inner::String
    end

    struct BaseDecl
        iriref::IRIRef
    end

    struct PrefixDecl
        pname_ns::String
        iriref::IRIRef
    end

    const SCDAQuery = Union{SelectQuery, ConstructQuery, DeleteQuery, AskQuery}

    struct SPARQLQuery
        decls::Array{Union{BaseDecl, PrefixDecl}}
    end

    struct SPARQLUpdate
        decls::Array{Union{BaseDecl, PrefixDecl}}
    end

    mutable struct Parser
        lines::Int
        function Parser()::Parser
            p = new()
            return _init!(p)
        end
        function _init!(p::Parser)::Parser
            p.lines = 0
            return p
        end
    end



    struct BadSyntax <: Exception
        lines::Any
        qstr::String
        i::Int
        msg::String
        function BadSyntax(lines::Any, qstr::String, i::Int, msg::String)
          new(lines, qstr, i, msg)
        end
    end
    function Base.show(io::IO, self::BadSyntax)
        qstr = self.qstr
        i = self.i
        st = 1
        if i > 61
            pre = "..."
            st = i - 60
        else
            pre = ""
        end
        if lastindex(qstr) - i > 60
            post = "..."
        else
            post = ""
        end
        print(io, "Syntax Error at line ")
        print(io, self.lines+1)
        print(io, ":\nBad syntax (")
        print(io, self.msg)
        print(io, ") at ^ in:\n")
        print(io, pre)
        print(io, qstr[st:i-1])
        print(io, "^")
        endl = min(lastindex(qstr), i+60)
        print(io, qstr[i:thisind(qstr, endl)])
        print(io, post)
        print(io, "\"")
    end
    function BadSyntax(self::Parser, qstr::String, i::Int, msg::String)
        throw(BadSyntax(self.lines, qstr, i, msg))
    end

    function parse(self::Parser, qstr::String)::Union{SPARQLQuery,SPARQLUpdate}
        return queryOrUpdate(self, qstr)
    end

    function parse(qstr::String)::Union{SPARQLQuery,SPARQLUpdate}
        parser = Parser()
        return parse(parser, qstr)
    end


    function queryOrUpdate(self::Parser, qstr::String)::Union{SPARQLQuery, SPARQLUpdate}
        prologueDecls::Array{Union{BaseDecl, PrefixDecl}} = []
        i::Int = prologue(self, qstr, 1, prologueDecls)
        i = scdaQuery(self, qstr, i)
        if i > 0
            return SPARQLQuery(prologueDecls)
        end
    end

    function skipspace(self::Parser, qstr::String, i::Int; skipNewline::Bool=true)::Int
        i = thisind(qstr, i)
        lastind = lastindex(qstr)
        ch::Char = '\0'
        while i <= lastind
            ch = qstr[i]
            if occursin(ch, " \t")
                i = nextind(qstr, i)
            elseif (skipNewline && ch == '\n')
                i = nextind(qstr, i)
                self.lines = self.lines + 1
            elseif (skipNewline && ch == '\r')
                t1 = nextind(qstr, i)
                qstr[t1] != '\n' && error("Got \\r not followed by \\n")
                i = nextind(qstr, t1)
                self.lines = self.lines + 1
            else
                return i
            end
        end
        error("EOF")
    end

    function prologue(self::Parser, qstr::String, i::Int, res::Array{Union{BaseDecl, PrefixDecl}})::Int
        baseRE = r"BASE"i
        prefixRE = r"PREFIX"i
        c::Bool = true
        while c
            c = false
            i = skipspace(self, qstr, i)
            m = match(baseRE, qstr, i, PCRE.ANCHORED)
            if m !== nothing
                println(m)
                i = m.offset + length(m.match)
                i = skipspace(self, qstr, i)
                m2 = match(IRIREF_RE, qstr, i)
                m2 === nothing && BadSyntax(self, qstr, i, "Expected <IRIREF> after 'BASE' keyword.")
                println(m2)
                iriref = m2.match
                push!(res, BaseDecl(IRIRef(iriref)))
                i = m2.offset + length(m2.match)
                c = true
            end
            m = match(prefixRE, qstr, i, PCRE.ANCHORED)
            if m !== nothing
                println(m)
                i = m.offset + length(m.match)
                i = skipspace(self, qstr, i)
                m2 = match(PN_PREFIX, qstr, i, PCRE.ANCHORED)
                m2 === nothing && BadSyntax(self, qstr, i, "Expected PN_PREFIX after 'PREFIX' keyword.")
                println(m2)
                pn_prefix = m2.match
                i = m2.offset + length(m2.match)
                i = skipspace(self, qstr, i)
                m3 = match(IRIREF_RE, qstr, i)
                m3 === nothing && BadSyntax(self, qstr, i, "Expected <IRIREF> after prefix at PREFIX keyword.")
                println(m3)
                iriref = m3.match
                push!(res, PrefixDecl(pn_prefix, IRIRef(iriref)))
                i = m3.offset + length(m3.match)
                c = true
            end
            # didn't match BASE or PREFIX, now we leave.
        end
        return i
    end

    function scdaQuery(self::Parser, qstr::String, i::Int)::Int
        r1::Array{SCDAQuery, 1} = []
        j::Int = selectQuery(self, qstr, i, r1)
        if j > 0

        end
        j = constructQuery(self, qstr, i)


        return i
    end

    function selectQuery(self::Parser, qstr::String, i::Int, res::Array{Union{SelectQuery, ConstructQuery, DeleteQuery, AskQuery}, 1})::Int
         # SELECT clause
         i = skipspace(self, qstr, i)
         select = "SELECT\\s+(?:(DISTINCT|REDUCED)\\s)?((?:$(VAR)\\s+|(?:\\(\\s*.*\\s+AS\\s+$(VAR)\\s*\\)\\s+))+|\\*)"
         println(select)
         select_re = Regex(select, "i")
         m = match(select_re, qstr, i, PCRE.ANCHORED)
         m === nothing ? return 0
         println("$(m)")
         println("$(m.captures)")
          i = m.offset + length(m.match)
         end
         return i
    end




end