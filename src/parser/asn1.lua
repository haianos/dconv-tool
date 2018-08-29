--[[
    Simple ASN1 parser
   Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
   License: MIT
--]]

local lpeg     = require('lpeglabel')
local re       = require('relabel')
local location = require('location')
local ac       = require('ansicolors')
local utf8     = require('lua-utf8')
local split    = require('utils').split

-- inspect = require('inspect') --debug only
--- for fancyerr
local function errmsg(...)
   return ac.bright(ac.red(table.concat({...}, ' ')))
end

local function infomsg(...)
  return ac.cyan(table.concat({...},' '))
end


local function fancyerr(err)
  local str = errmsg('Program Syntax Error, with message: ')
  str = str..err.label.."\n"
  str = str..infomsg("  Error detected at (filename,line,column): ")..'('..err.pos.filename..", "..err.pos.line..", "..err.pos.col..")\n"
  str = str..infomsg("  full line: ")..ac.yellow(err.fulline).."\n"
  return str
end

local function typedecl(prefix, types)
    local constructors = {}
    for typename, conss in pairs(types) do
        for consname, fields in pairs(conss) do
            local tag = prefix .. "." .. consname
            constructors[consname] = function(...)
                local args = table.pack(...)
                if args.n ~= #fields then
                    error("missing arguments for " .. consname)
                end
                local node = { _tag = tag }
                for i, field in ipairs(fields) do
                    node[field] = args[i]
                end
                return node
            end
        end
    end
    return constructors
end

local op= typedecl("Op", {
  Decl = {
    Module      = {'name','defs'},
    Import      = {'from','list'},
    Boolean     = {'name'},
    Integer     = {'name','params'},  -- min/max ignored
    Real        = {'name','params'},  -- min/max ignored
    Enumerated  = {'name','elist'},
    Constant    = {'name','typeof','value'},
    Alias       = {'name','aliasof','params'},
    Octet       = {'name','params'},
    Sequence    = {'name','fields'},
    FieldName   = {'name','typeof'},
    FieldSeqOf  = {'name','from','to','typeof'},
    ParamList   = {'params'}
  }
})

local syntax_errors = {
  errors = {
    fail                = "Syntax Error",
  },
  fun = {}
}

function set_syntax_errors(etab)
  local mt = {
    __index = function(t,k)
        if type(t.errors[k]) == 'string' then return t.errors[k] end
        if t.fun[k] then return t.fun[k](error_stack) end
        return k
      end
  }
  setmetatable(etab,mt)
end

set_syntax_errors(syntax_errors)

local lexer = {}
local defs  = {}

local symbols = {
  TDEF     = "::=",
  DDOT     = "..",
  LPARENT  = "(", RPARENT  = ")",
  LCURLY   = "{", RCURLY = "}",
  COLON    = ":",
  COMMA     = ",",
  SEMICOLON = ";" -- used only to end IMPORTS
}

for i,v in pairs(symbols) do
  lexer[i] = lpeg.P(v)
end

-- keywords and names
local idstart = lpeg.P("_") + lpeg.R("AZ", "az")
local idrest  = lpeg.P("_") + lpeg.P("-") + lpeg.R("AZ", "az", "09")
local possiblename = idstart * idrest^0
local keywords = {
  'DEFINITIONS',
  'BEGIN',
  'END',
  'IMPORTS',
  'FROM',
  'ENUMERATED',
  'SEQUENCE',
  'OF',
  'SIZE',
  'INTEGER',
  'BOOLEAN',
  'REAL',
  'STRING'
}

local captured_keywords = {
  'BOOLEAN',
  'REAL',
  'INTEGER',
  'OCTET'
}

--- longstring and short string (for STRINGLIT) borrowed FROM TITAN
local linebreak =
    lpeg.P("\n\r") +
    lpeg.P("\r\n") +
    lpeg.P("\n") +
    lpeg.P("\r")

lexer.SPACE = lpeg.S(" \t\n\v\f\r")^1

local longstring
do
    local equals = lpeg.P("=")^0
    local open  = lpeg.P("[") * lpeg.Cg(equals, "open")  * lpeg.P("[") * linebreak^-1
    local close = lpeg.P("]") * lpeg.Cg(equals, "close") * lpeg.P("]")

    local matching_close =
        close * lpeg.Cmt( lpeg.Cb("open") * lpeg.Cb("close"),
            function(source, i, open, close)
                return open == close
            end)

    local contents = (-matching_close * lpeg.P(1)) ^0

    longstring = (
        open * (
            lpeg.C(contents) * close +
            lpeg.T("UnclosedLongString")
        )
    ) / function(contents) return contents end -- hide the group captures
end

local shortstring
do

    local delimiter = lpeg.P('"') + lpeg.P("'")

    local open  = lpeg.Cg(delimiter, "open")
    local close = lpeg.Cg(delimiter, "close")

    local matching_close =
        close * lpeg.Cmt( lpeg.Cb("open")* lpeg.Cb("close"),
            function(source, i, open, close)
                return open == close
            end)

    -- A sequence of up to 3 decimal digits
    -- representing a non-negative integer less than 256
    local decimal_escape = lpeg.P("1") * lpeg.R("09") * lpeg.R("09") +
        lpeg.P("2") * lpeg.R("04") * lpeg.R("09") +
        lpeg.P("2") * lpeg.P("5") * lpeg.R("05") +
        lpeg.P("0") * lpeg.R("09") * lpeg.R("09") +
        lpeg.R("09") * lpeg.R("09") * -lpeg.R("09")  +
        lpeg.R("09") * -lpeg.R("09") +
        lpeg.R("09") * lpeg.T("MalformedEscape_decimal")

    local escape_sequence = lpeg.P("\\") * (
        (-lpeg.P(1) * lpeg.T("UnclosedShortString")) +
        (lpeg.P("a")  / "\a") +
        (lpeg.P("b")  / "\b") +
        (lpeg.P("f")  / "\f") +
        (lpeg.P("n")  / "\n") +
        (lpeg.P("r")  / "\r") +
        (lpeg.P("t")  / "\t") +
        (lpeg.P("v")  / "\v") +
        (lpeg.P("\\") / "\\") +
        (lpeg.P("\'") / "\'") +
        (lpeg.P("\"") / "\"") +
        (linebreak / "\n") +
        lpeg.C(decimal_escape) / tonumber / string.char +
        (lpeg.P("u") * (lpeg.P("{") * lpeg.C(lpeg.R("09", "af", "AF")^0) * lpeg.P("}") * lpeg.Cc(16) + lpeg.T("MalformedEscape_u")))
             / tonumber / utf8.char +
        (lpeg.P("x") * (lpeg.C(lpeg.R("09", "af", "AF") * lpeg.R("09", "af", "AF")) * lpeg.Cc(16) + lpeg.T("MalformedEscape_x"))) / tonumber / string.char +
        (lpeg.P("z") * lexer.SPACE^0) +
        lpeg.T("InvalidEscape")
    )

    local part = (
        (lpeg.S("\n\r") * lpeg.T("UnclosedShortString")) +
        escape_sequence +
        (lpeg.C(lpeg.P(1)))
    )

    local contents = (-matching_close * part)^0

    shortstring = (
        open * (
            lpeg.Ct(contents) * close +
            lpeg.T("UnclosedShortString")
        )
    ) / function(parts) return table.concat(parts) end
end

lexer.STRINGLIT = shortstring + longstring

for _, keyword in ipairs(keywords) do
    lexer[keyword:upper()] = lpeg.P(keyword) * -idrest
end

for _, keyword in ipairs(captured_keywords) do
    lexer[keyword:upper()] = lpeg.C(lpeg.P(keyword)) * -idrest
end

local is_keyword = {}
for _, keyword in ipairs(keywords) do
    is_keyword[keyword] = true
end
for _, keyword in ipairs(captured_keywords) do
    is_keyword[keyword] = true
end

lexer.NAME = lpeg.Cmt(lpeg.C(possiblename), function(_, pos, s)
    if not is_keyword[s] then
        return pos, s
    else
        return false
    end
end)

local comment_start = lpeg.P("--")
local short_comment = comment_start * (lpeg.P(1) - lpeg.P("\n"))^0 * lpeg.P("\n")^-1

lexer.COMMENT = short_comment

-- local number_start = lpeg.P(".")^-1 * lpeg.R("09")
-- -- local num_neg = lpeg.C(lpeg.S("+-")^-1)
-- local expo = lpeg.S("EePp") * lpeg.S("+-")^-1
-- local possible_number = (expo + lpeg.R("09", "AZ", "az") + ".")^1
-- local good_number = lpeg.Cmt(possible_number, function(_, i, s)
--    local n = tonumber(s)
--     if n then
--         return i, n
--     else
--         return false
--     end
-- end)
-- lexer.NUMBER =  #number_start * (good_number + lpeg.T("MalformedNumber"))

local number_start = lpeg.S("+-.")^-1 * lpeg.R("09")
local expo = lpeg.S("EePp") * lpeg.S("+-")^-1
local possible_number = ( lpeg.S("+-") + lpeg.R("09") + "." + expo)^1
local good_number = lpeg.Cmt(possible_number, function(_,i,s)
    local n = tonumber(s)
    if n then
        return i, n
    else
      local ridx = 1
      local n2 = s:sub(-ridx)
      while n2 == '.' do
        ridx = ridx+1
        n2=s:sub(-ridx)
      end
      ridx = ridx+1
      print(s:sub(1,-ridx),tonumber(s:sub(1,-ridx)),i)
      n = tonumber(s:sub(1,-ridx))
      if n then return i, n end
      return false
    end
end)  
lexer.NUMBER = #number_start * (good_number + lpeg.T("MalformedNumber"))

    
for tokname, tokpat in pairs(lexer) do 
  defs[tokname] = tokpat
end

defs['Decl_Test'] = function(...)
   local args = {...}
   print(inspect(args))
   return args
end

defs['Decl_Module'] = function(name,...)
  local defs = {...}
  return op.Module(name,defs)
end

defs['Decl_ImportModule'] = function(t)
   local from = t[#t]
   t[#t] = nil
   return op.Import(from,t)
end

defs['Decl_Parametric'] = function(...)
  return op.ParamList({...})
end

defs['Decl_ParamType'] = function(name,aliasof,params)
  return op.Alias(name,aliasof,params)
end

defs['Decl_Boolean'] = function(t)
   return op.Boolean(t)
end

defs['Decl_Integer'] = function(name,...)
   -- Support for params (maybe)
   -- this impl is not 'clean', params is a table
   -- if there are no params, should be a string ('INTEGER')
   local args   = {...}
   local params = false
   if type(args[1]) == 'table' then params = args[1] end
   return op.Integer(name,params)
end

defs['Decl_Real'] = function(name,...)
   -- Support for params (maybe)
   -- this impl is not 'clean', params is a table
   -- if there are no params, should be a string ('INTEGER')
   local args   = {...}
   local params = false
   if type(args[1]) == 'table' then params = args[1] end
   return op.Real(name,params)
end

defs['Decl_Constant'] = function(name,typeof,value)
   return op.Constant(name,typeof,value)
end

defs['Decl_Alias'] = function(name,aliasof)
  return op.Alias(name,aliasof,false)
end

defs['Decl_Enumerated'] = function(name,...)
  return op.Enumerated(name,{...})
end

defs['Decl_Sequence'] = function(name,...)
  return op.Sequence(name,{...})
end

defs['Decl_FieldName'] = function(name,typeof)
  return op.FieldName(name,typeof)
end

defs['Decl_FieldSeqOf'] = function(name,from,to,typeof)
  return op.FieldSeqOf(name,from,to,typeof)  
end

defs['Decl_Octet'] = function(name)
  return op.Octet(name,false)
end

local THIS_FILENAME = nil
defs.get_loc = function(s, pos)
  return true, location.from_pos(THIS_FILENAME, s, pos)
end

local sgrammar = 
[[
  program <- SKIP*  ( mdef
                        )* !.
  
  mdef     <- ( NAME DEFINITIONS TDEF BEGIN 
                    ( importmodules
                      / tdef)*
                    END)                                          ->Decl_Module                            
  
  importmodules  <- ( IMPORTS (importmodule)* (SEMICOLON)? )
  importmodule   <- ( {| NAME (COMMA NAME)* FROM NAME |})         ->Decl_ImportModule
  
  tdef           <- (  booltype 
                     / realtype
                     / inttype
                     / enumtype
                     / octettype
                     / seqtype
                     / paramtype
                     / consttype
                     / aliastype )                           -- ->Decl_Test
  
  booltype  <- (NAME TDEF BOOLEAN)                                                         ->Decl_Boolean
  consttype <- (NAME NAME TDEF NUMBER)                                                     ->Decl_Constant
  aliastype <- (NAME TDEF NAME)                                                            ->Decl_Alias
  paramtype <- (NAME TDEF NAME LCURLY {|NAME (COMMA NAME)*|} RCURLY)                       ->Decl_ParamType
  -- The following can be parametric
  realtype  <- (NAME (parametric)? TDEF REAL (LPARENT ( NAME / NUMBER) DDOT ( NAME / NUMBER) RPARENT)? ) ->Decl_Real
  inttype   <- (NAME (parametric)? TDEF INTEGER  
                    (LPARENT (NAME / NUMBER)  DDOT (NAME / NUMBER) RPARENT)? )             ->Decl_Integer
  octettype <- (NAME (parametric)? TDEF OCTET 
                         STRING LPARENT
                           SIZE LPARENT 
                             (NAME/NUMBER) DDOT (NAME/NUMBER)
                          RPARENT
                       RPARENT)                                                            ->Decl_Octet
  fieldname <- (NAME NAME)                                                                 ->Decl_FieldName
  enumtype  <- (NAME TDEF ENUMERATED LCURLY
                  NAME (COMMA NAME)*
                RCURLY)                                                                    ->Decl_Enumerated
  seqtype   <- (NAME (parametric)? TDEF SEQUENCE LCURLY
                   (seqof
                     / fieldname)
                   (COMMA (seqof / fieldname))*
               RCURLY)                                                                     ->Decl_Sequence
  seqof    <- (NAME SEQUENCE
                    LPARENT
                       SIZE LPARENT 
                         (NAME/NUMBER) DDOT (NAME/NUMBER) 
                       RPARENT
                    RPARENT OF NAME)                                                        ->Decl_FieldSeqOf
  parametric <- (LCURLY {|NAME COLON NAME|} 
                        (COMMA {| NAME COLON NAME|})* 
                RCURLY)                                                                     ->Decl_Parametric
  
  P <- {} => get_loc
  
  SKIP     <- (%SPACE / %COMMENT)
  
  NAME                  <- %NAME         SKIP*
  LCURLY                <- %LCURLY       SKIP*
  RCURLY                <- %RCURLY       SKIP*
  COMMA                 <- %COMMA        SKIP*
  TDEF                  <- %TDEF         SKIP*
  DDOT                  <- %DDOT         SKIP*
  SEMICOLON             <- %SEMICOLON    SKIP*
  COLON                 <- %COLON        SKIP*
  RPARENT               <- %RPARENT      SKIP*
  LPARENT               <- %LPARENT      SKIP*
  DEFINITIONS           <- %DEFINITIONS  SKIP*
  BEGIN                 <- %BEGIN        SKIP*
  END                   <- %END          SKIP*
  IMPORTS               <- %IMPORTS      SKIP*
  FROM                  <- %FROM         SKIP*
  REAL                  <- %REAL         SKIP*
  INTEGER               <- %INTEGER      SKIP*
  OCTET                 <- %OCTET        SKIP*
  STRING                <- %STRING       SKIP*
  ENUMERATED            <- %ENUMERATED   SKIP*
  BOOLEAN               <- %BOOLEAN      SKIP*
  SEQUENCE              <- %SEQUENCE     SKIP*
  OF                    <- %OF           SKIP*
  SIZE                  <- %SIZE         SKIP*
  NUMBER                <- %NUMBER       SKIP*
]]

local grammar = re.compile(sgrammar, defs)

local parser = {}

function parser.parse(filename,input,fd)
  local fd  = fd or nil
  local op, err, errpos = grammar:match(input)
  
  if op then
    return op
  else
    local myerr = syntax_errors[err] or  err
    local pos   = location.from_pos(filename,input,errpos)
    local line  = split(input, "\n")[pos.line]
    local errret = {label=myerr, pos=pos, fulline=line }
    if fd then
      fd:write(fancyerr(errret))
    end
    return false, errret
  end
end

function parser.parse_file(filename)
  fd=io.open(filename,'r')
  input = fd:read('*all')
  fd:close()
  
  return parser.parse(filename,input,io.stdout)
end

return parser