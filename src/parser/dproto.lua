--[[
    Data Modelling Parser
   Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
   License: MIT
--]]

local lpeg     = require('lpeglabel')
local re       = require('relabel')
local location = require('location')
local ac       = require('ansicolors')
local preproc  = require('utils').preproc
local split    = require('utils').split
local utf8     = require('lua-utf8')
local mapp     = require('parser.map')
-- inspect = require('inspect')

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
--   ..ac.yellow('>'..err.fulline..'<').."\n"
--   str = str..infomsg("\t (filename,line,column) : ")
--   str = str..infomsg("\t  * filename: ")..err.pos.filename.."\n"
--   str = str..infomsg("\t  * line    : ")..err.pos.line.."\n"
--   str = str..infomsg("\t  * column  : ")..err.pos.col.."\n"
  return str
end

--[[ The same of decl.lua ]]--
-- this is borrowed by "titan-lang" compiler
local function typedecl(prefix, types)
    local constructors = {}
    for typename, conss in pairs(types) do
        for consname, fields in pairs(conss) do
            local tag = consname
            if prefix then
              tag = prefix .. "." .. consname
            end
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

--gen-uid
local counter = 0
local function gen_uid()
  counter = counter +1
  return counter
end

local decl= typedecl(nil, {
  Decl = {
    reference   = {'symname'},
    anonymdef   = {'domain','content'},
    load        = {'name'},
    alias       = {'lhs', 'rhs', 'relation'},
    ddr         = {'name','mmid','mid'},
    units       = {'name','mmid','ref','map'},
    algebraic   = {'class','name','elements'},
    view        = {'lhs','rhs','relation'},
    conversion  = {'from', 'to', 'fproto'},
    dproto      = {'name','model','semantic','coord','algebraic','ddr','dr','units'},
    dproto_comp = {'name','model','semantic','composes','dr','usingddr'},
    fproto      = {'name','library','fname','args'},
    dotnotation = {'sequence'},
    -- non-geometry domain
    jointstate  = {'name','model','semantic','algebraic','ddr','dr'}
  }
})

local fieldtype= typedecl("Field", {
    Decl = {
      Name        = {'name'},
      Value       = {'value'},
      Pair        = {'value1','value2'},
      DotNotation = {'sequence'}
    }
})

local syntax_errors = {
  errors = {
    fail                = "Syntax Error",
    DprotoUncomplete    = "'dproto' definition not complete (missing ddr, or semantic or coord)",
    FprotoUncomplete    = "'fproto' definition not complete (missing library, fname or args)",
    UnclosedShortString = "unclosed long string or long comment",
    UnclosedLongString  = "unclosed short string",
    ExpectedLiteral     = "expected literal value",
    CoordOptionNoValid = "<coord> option does not exist",
    MalformedEscape_decimal = "",
    MalformedEscape_u       = "",
    MalformedEscape_x       = "",
    InvalidEscape           = ""
  },
  fun = {
    CoordOptionNotValid = function(s)
      return "unvalid <coord> option ("..s[1]..")"
    end
  }
}

local error_stack = {}

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
  COLON    = ":", DCOLON = "::",
  LCURLY   = "{", RCURLY = "}",
  LBRACKET = "[", RBRACKET = "]",
  RARROW   = "=>",
  LARROW   = "<=",
  SEMICOLON = ";",
  ASSIGN    = "=",
  LITERAL   = '"',
  RDIR      = '->',
  COMMA     = ",",
  DOT       = '.',
  ALIASSYM  = '<->'
}

for i,v in pairs(symbols) do
  lexer[i] = lpeg.P(v)
end

-- keywords and names
local idstart = lpeg.P("_") + lpeg.R("AZ", "az")
local idrest  = lpeg.P("_") + lpeg.P("-") + lpeg.R("AZ", "az", "09")
local possiblename = idstart * idrest^0
local keywords = {
  'alias',
  'coord',
  'conversion',
  'dproto',
  'fproto',
  'algebraic',
  'view',
  'domain',
  'load',
  'units',
  'composes'
}

local captured_keywords = {
--   'fname', 'library', 'args'
  'semantic','coord', 'ddr', 'library','args','fname','Scalar','Vector','Matrix',
  'algebraic','dr', 'usingddr','units'
}
--- longstring and short string (for STRINGLIT) borrowed FROM TITAN
local linebreak =
    lpeg.P("\n\r") +
    lpeg.P("\r\n") +
    lpeg.P("\n") +
    lpeg.P("\r")

lexer.SPACE = lpeg.S(" \t\n\v\f\r")^1
lexer.DELEGATE = lpeg.P{ "{" * lpeg.C(((1 - lpeg.S"{}") + lpeg.V(1))^0) * "}" }

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
-- END
-- I have to capture those
lexer.INARROW = lpeg.Cmt(lpeg.C(lpeg.P("<=")), function(_,pos,s)
  return pos, 'IN'
  end
)
lexer.OUTARROW = lpeg.Cmt(lpeg.C(lpeg.P("=>")), function(_,pos,s)
  return pos, 'OUT'
  end
)

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


local comment_start = lpeg.P("//")
local short_comment = comment_start * (lpeg.P(1) - lpeg.P("\n"))^0 * lpeg.P("\n")^-1

lexer.COMMENT = short_comment

-- TODO: improve me, negative number
local number_start = lpeg.P(".")^-1 * lpeg.R("09")
-- local num_neg = lpeg.C(lpeg.S("+-")^-1)
local expo = lpeg.S("EePp") * lpeg.S("+-")^-1
local possible_number = (expo + lpeg.R("09", "AZ", "az") + ".")^1
local good_number = lpeg.Cmt(possible_number, function(_, i, s)
    local n = tonumber(s)
    if n then
        return i, n
    else
        return false
    end
end)
lexer.NUMBER =  #number_start * (good_number + lpeg.T("MalformedNumber"))

lexer.VNUMBER = lpeg.C(lpeg.R("09") * (lpeg.P(".") * lpeg.R("09"))^1)
lexer.IDNUM = lpeg.C(lpeg.R("09")^1)

lexer.VALUE = lexer.NAME + lexer.NUMBER
        
for tokname, tokpat in pairs(lexer) do 
  defs[tokname] = tokpat
end

defs['Decl_Test'] = function(...)
   local args = {...}
   print(inspect(args))
   return args
end

defs['Decl_Anonymdef'] = function(...)
  local args = {...}
  return decl.anonymdef({unpack(args,1,#args-1)},args[#args])
end

defs['Decl_Reference'] = function(symval)
 return decl.reference(symval)
end

defs['Decl_ValueField'] = function(value)
  return fieldtype.Value(value)
end

defs['Decl_NameField'] = function(name)
  return fieldtype.Name(name)
end

defs['Decl_PairField'] = function(v1,v2)
  return fieldtype.Pair(v1,v2)
end

defs['Decl_DotNotationField' ] = function(...)
 return fieldtype.DotNotation({...})
end

defs['Decl_Load'] = function(libname)
  return decl.load(libname)
end

defs['Decl_Alias'] =  function(pos,lhs,rhs,relation)
--   local rel = {}
--   for i,v in pairs(relation) do
--       print(v[1].name,v[2].value)
--       rel[v[1]] = v[2]
--   end
--   return decl.Alias(lhs,rhs,rel)
  return decl.alias(lhs,rhs,relation)
end

defs['Decl_DDRDef'] = function(key,symname,mmid,content)
  return decl.ddr(symname,mmid,content)
end

defs['Decl_Units'] = function(name,mmid,ref,map)
  return decl.units(name,mmid,ref,map)
end

defs['Decl_Algebraic'] = function(pos,obj,name,...)
  local atype = {...} --algebraic type, Scalar, Vector or Matrix
--   for i,v in pairs(atype) do print(i,v) end
  return decl.algebraic(atype[1][1],name,atype[1][2])
end

defs['Decl_View'] = function(pos,lhs,rhs,relation)
  local lhs = lhs
  local rhs = rhs
  if type(lhs) == 'table' then
    lhs = decl.dotnotation(lhs)
  end
  if type(rhs) == 'table' then
    rhs = decl.dototation(rhs)
  end
--   if type(relation) == 'table' then
--     for i,v in pairs(relation[1]) do print(i,v) end
--   end
  return decl.view(lhs,rhs, relation)
end

defs['Decl_Conversion'] = function(pos,from,to,fproto)
  return decl.conversion(from,to,fproto)
end

--[[ CONFIGURATION --]]
-- Valid (modelled) dproto annotations
local dpan = {
  semantic  = 'semantic',
  coord     = 'coord',
  ddr       = 'ddr',
  dr        = 'dr',
  composes  = 'composes',
  usingddr  = 'usingddr',
  units     = 'units',
  algebraic = 'algebraic'
}

-- Tagging annotation field
local function tag_an(tag,tab)
  if not tab._tag then -- do not override
    tab._tag = dpan[tag]
  end
--   print(inspect(tab))
  return tab
end

--[[ END CONFIGURATION --]]

--[[ Specific `Solve_` implementation --]]
defs['Solve_Units'] = function(...)
  local args = {...}
  local units = args[2]  -- necessary for retrocompatibility
  if args[2]._tag == 'anonymdef' then
    local content = mapp:match(args[2].content)
    units = decl.units('anonymous',args[2].domain[1],args[2].domain[2],content)
  end
--   return 'units', units  -- This is necessary for retrocompatibility
--   return tag_an('units',units)
--   print('unit tag is',units._tag)
  return {_tag='units', sval=units}
end

defs['Solve_DDR'] = function(...)
  local args = {...}
  if args[2]._tag == "reference" then --return 'ddr', args[2] end
    return tag_an('ddr',args[2]) end
  return tag_an('ddr', {mid=args[2].content, mmid = args[2].domain[1] })
--   return 'ddr', {mid=args[2].content, mmid = args[2].domain[1] }
end

--[[ Generated `Solve_` from `dpan` for single value (sval) fields--]] 
local function gen_solvers()
  for i,v in pairs(dpan) do
    local  name = 'Solve_'..v:gsub("^%l", string.upper)
    if not defs[name] then
      defs[name] = function(s,sval) return tag_an(s,{sval=sval}) end
    end
  end
end

gen_solvers()
--  Aliases, for the sake of the poor Enea's memory (to be removed)
defs['Solve_DR']       = defs.Solve_Dr
defs['Solve_DDR']      = defs.Solve_Ddr
defs['Solve_UsingDDR'] = defs.Solve_Usingddr

-- defs['Solve_Semantic'] = function(s,sval)
--   return tag_an(s,{sval=sval})
-- --   return 'semantic', {_tag=s, sval=sval }
-- end
-- 
-- defs['Solve_Coord'] = function(s,sval)
-- --   return 'coord', {_tag=s, sval=sval }
-- end
-- 
-- defs['Solve_DR'] = function(s,sval)
--   return 'dr', {_tag=s, sval=sval }
-- end
-- 
-- defs['Solve_UsingDDR'] = function(s,sval)
--   return 'usingddr', {_tag=s, sval=sval }
-- end
-- 
defs['Solve_Composes'] = function(...)
  local args = {...}
  args._tag = 'composes'
--   print(inspect(args))
  return args
end

-- This version is stateless, so better :-)
--TODO: make gen calls structural, such that it is easy to add more
defs['Decl_Dproto'] = function(pos,name,model,...)
  local args   = {...}
  local schema = schemas[model]
  selection = 'newgeom'
  --[[ Check schema --]]
  if schema._tag == 'oneOf' then
    ok, selection = oneof(function(s)
                        return check_schema(s,args)
                      end,schema.schema)
    if not ok then print('schema failed (oneOf)') return false end
  elseif schema then
    local ok = check_schema(schema,args)
    if not ok then print('schema failed') return false end
  end
--   local fields = {}
--   for i,v in pairs(args) do
--     fields[v[1]] = {unpack(v,2)}
--   end
  if selection == 'newgeom' then
    ---------- parse ALGEBRAIC
--     local alge = args.fields.algebraic[1]
--     -----------------------------------------------------
--     ---------- parse DDR
--     local ddr = fields.ddr[1]
--     -----------------------------------------------------
--     ---------- parse UNITS
--     local units = fields.units[1]
--     -----------------------
--     local coord    = fields.coord[1]
--     local semantic = fields.semantic[1]
--     local dr       = fields.dr[1]
--     return decl.Dproto(name,model,semantic,coord,alge,ddr,dr,units)
    return {_tag='dproto',name=name,model=model,...}
    
  elseif selection == 'composed' then
    local composes = {}
--     print(inspect(args))
--     print(inspect(args))
--     for i,v in pairs(fields.composes) do
--       composes[v[1]] = v[2]
--     end
--     local usingddr = false
--     if fields.usingddr then
--       usingddr = {}
--       for i,v in pairs(fields.usingddr) do usingddr[v[1]] = v[2] end
--     end
--     return decl.dproto_comp(name,model,fields.semantic[1],composes,fields.dr[1],usingddr)
    return { _tag='dproto_comp', name=name,model=model,...}
  elseif selection == 'jointstate' then
    local ddr = {}
    for i,v in pairs(fields.ddr) do ddr[v[1]] = v[2] end
    return decl.jointstate(name,model,fields.algebraic[1],fields.semantic[1],ddr,fields.dr[1])
  end
  print("shouldn't get here: "..selection.." not supported")
  return false
end

defs['Decl_Fproto'] = function(pos,name,content)
  local args = {}
  for i,v in pairs(content) do
      args[v[1]] = {unpack(v,2)}
  end
  return decl.fproto(name,args.library,args.fname,args.args)
end

local function check_occurrence(ulist,t)
  local found = {}
  for i,v in pairs(t) do
    local count = found[v[1]] or 0
    if (not ulist[v[1]]) or (ulist[v[1]] and (ulist[v[1]] < count) ) then
--       print(v[1],count,ulist[v[1]])
      return false
    end
    found[v[1]] = count+1
  end
  return true
end

defs['check_dproto'] = function(_,pos,...)
  if check_occurrence({semantic=1,ddr=1,coord=1},{...}) then
    return pos, {...}
  end
  return false
end

defs['check_fproto'] = function(_,pos,...)
  if check_occurrence({library=1,args=1,fname=1},{...}) then
    return pos, {...}
  end
  return false
end

function oneof(fun,tab)
  for i,v in pairs(tab) do
    if fun(v) then return true, i end
  end
  return false
end

local check_module = false

schemas = {
  custom = {
    _tag = 'oneOf',
    schema = {
      jointstate = {
--         qudt = 1
        ddr       = 1,
        semantic  = 1,
        dr        = 1,
        algebraic = 1
      }
    }
  },
  geometry = {
    _tag = 'oneOf',
    schema = {
     newgeom = {
       coord     = 1,
       ddr       = 1,
       semantic  = 1,
       algebraic = 1,
       dr        = -1,
       ddr       = 1,
       units     = -1 -- Optional, not punitive
     },
     composed = {
       semantic = 1,
       composes = 1,
       dr       = 1,
       usingddr = -1 -- optional
     }
    }
  },
  geometry_alt = {
    ['Decl.Alias'] =  1,
    ['Decl.Conversion'] =  1
  }
}

function check_schema(schema,parsed)
  local t = {}
--   print(inspect(parsed))
  for i,v in pairs(parsed) do
    if not t[v._tag] then t[v._tag] = 0 end
    t[v._tag] = t[v._tag] + 1
--     if v._tag == 'reference' then print(inspect(v)) end
--     if v._tag == 'ddr' then print(inspect(v)) end
  end
  -- this implementation is not ok yet
  for i,v in pairs(t) do 
    if not schema[i] then return false end
    if (schema[i] ~= -1) and v ~= schema[i] then return false end
  end
  return true
end


--[[
    This checks if <coord> field is appropriate
   * list hardcoded: MODEL ME!
   * proper return handling (error code + arguments)
   * only single-value check, no high-order, such as OK for semantic field
--]]
defs['check_coord_exists'] = function(_,pos,c)
  local coordset = {'cartesian', 'polar', 'rot_matrix', 'quaternion','homogeneous_transformation'}
  for i,v in pairs(coordset) do
    if v == c then return pos, c end
  end
  error_stack[1] = c
  return false
end


local THIS_FILENAME = nil
defs.get_loc = function(s, pos)
--   print('loc',s,pos)
--   return true, pos
  return true, location.from_pos(THIS_FILENAME, s, pos)
end

local static_grammar = 
[[
    program <- SKIP* {| (  loadtag
                          / ddrdef
                          / unitsdef
                          / alias
                          / algebraic
                          / view
                          / conversion 
                          / dproto
                          / fproto )* |} !.
                          
    
--     dprotostatement <- ( $(dproto_aggregate) )
    
    loadtag <- ( LOAD NAME)                                      ->Decl_Load
       
    fproto <- (P FPROTO NAME (DCOLON)? LCURLY
                             ( ( {| fprotolib / fprotofname / fprotoargs |} )^3 )^FprotoUncomplete =>check_fproto 
                              RCURLY)                            ->Decl_Fproto
                              
    dproto <- (P DPROTO NAME (DCOLON)? NAME LCURLY
                         dprotostatement ((COMMA)?  dprotostatement )*
                    RCURLY)                                       ->Decl_Dproto
    

    dprotostatement <- ( dprotosemantic
                         / dprotocoord 
                         / dprotoalgebraic 
                         / dprotodr 
                         / dprotoddr 
                         / dprotocomposes 
                         / dprotousingddr 
                         / dprotounits )
                         
--     coord  <- (P COORD NAME LCURLY ( vecdef
--                                   / (namedef)*)
--                            RCURLY)                              ->Decl_Coord
    
    alias  <- (P ALIAS NAME ALIASSYM NAME LCURLY
                 {| mapdef2 ( (COMMA)? mapdef2)*|}
               RCURLY)                                          ->Decl_Alias
               
    ddrdef <- (DDR NAME DCOLON NAME DELEGATE)                   ->Decl_DDRDef
    
    unitsdef <- (UNITS NAME DCOLON NAME COMMA NAME LCURLY 
                      {| mapdef2 ( (COMMA)? mapdef2)* |} 
                RCURLY)                                         ->Decl_Units
               
    view   <- (P VIEW ( {| namedotnot |} / NAME )
                 RDIR 
                 ( {| namedotnot |} / NAME )
               (
                   (ASSIGN NAME) 
                 / ((ASSIGN)? LCURLY {| mapdef2 ( (COMMA)? mapdef2)* |} RCURLY)
               ))                    ->Decl_View
               
    algebraic <- (P ALGEBRAIC NAME DCOLON
                                {| ascalar / avector / amatrix |})    ->Decl_Algebraic

                                
    ascalar   <- (SCALAR LCURLY {| NAME (COMMA NAME)* |} RCURLY)
    avector   <- (VECTOR LCURLY {| IDNUM |} RCURLY)
    amatrix   <- (MATRIX LCURLY {| IDNUM COMMA IDNUM |} RCURLY)
    
    
    
    conversion <- (P CONVERSION NAME RDIR NAME ASSIGN NAME)     ->Decl_Conversion
    
                              
    mapdef2   <- ( {|  mapel ASSIGN mapel |})
                 
    mapel     <- (    drdotnot
                    / namefield 
                    / valuefield
                    / pairfield
                    )
                              
    namedef  <- ( {| NAME COLON NAME|})  
    vecdef   <- ( {| LBRACKET NUMBER RBRACKET COLON NAME |})   
      
--     dprotocoord     <- ( COORD    ASSIGN NAME=>check_coord_exists^CoordOptionNotValid)
    dprotocoord     <- ( COORD    ASSIGN NAME)                                          ->Solve_Coord
    dprotosemantic  <- ( SEMANTIC ASSIGN NAME)                                          ->Solve_Semantic
    dprotoddr       <- (   (DDR reference)
                         / (DDR anonymdef) )                                            ->Solve_DDR
                         
    dprotoddrdep    <- ( DDR  (ASSIGN)? LCURLY
                             ( {| NAME ASSIGN STRINGLIT^ExpectedLiteral|})^2   --TODO: fields are 'mid' and 'mmid'
                                  RCURLY )                                     --DEPRECATED!!! 
                                  
    dprotoalgebraic <- (   (ALGEBRAIC reference)
                         --/ (ALGEBRAIC anonymdef) -- NYI
                         / (ALGEBRAIC ASSIGN NAME) -- deprecated
                       )                                                                ->Solve_Algebraic
                       
    dprotounits     <- ( (UNITS  reference) 
                       / (UNITS  anonymdef) )                                           ->Solve_Units
    dprotodr        <- ( DR ASSIGN LCURLY {| mapdef2 ( (COMMA)? mapdef2)* |} RCURLY)    ->Solve_DR
    
    dprotocomposes  <- ( COMPOSES ASSIGN LCURLY
                           ( {| NAME ASSIGN NAME |} ( (COMMA)? {| NAME ASSIGN NAME |})* ) ->Solve_Composes
                            RCURLY)
                            
    dprotousingddr  <- ( USINGDDR (ASSIGN)? LCURLY                             --same as dprotoddr, make it better!
                             {| ( {| NAME ASSIGN STRINGLIT^ExpectedLiteral|})^2 |}  --TODO: fields are 'mid' and 'mmid'
                                  RCURLY )                                              ->Solve_UsingDDR
                                  
    fprotolib       <- ( LIBRARY ASSIGN STRINGLIT^ExpectedLiteral)
    fprotofname     <- ( FNAME   ASSIGN STRINGLIT^ExpectedLiteral)
    fprotoargs      <- ( ARGS (ASSIGN)? LCURLY
                                  {| argcaus ( (COMMA)? (argcaus))* |}
                                  RCURLY)
    
    argcaus         <- ( {|(IDNUM) (  %{INARROW}  NAME
                                 /  %{OUTARROW} NAME ) |})

    namedotnot      <- ( NAME (DOT NAME)^1 )
    
    -- Commonly used
    reference       <- (ASSIGN NAME)                                                      ->Decl_Reference
    anonymdef       <- (ASSIGN DCOLON NAME (COMMA NAME)* DELEGATE)                        ->Decl_Anonymdef
    
    -- definition of dot notation in dr, to allow tree traversal of inner datastructs
    drdotnot        <- ( (NAME /IDNUM) (DOT (NAME /IDNUM))^1 (DOT (NAME /IDNUM))* )       ->Decl_DotNotationField
    namefield       <- NAME                                                               ->Decl_NameField
    pairfield       <- (LCURLY IDNUM (COMMA IDNUM)^1 RCURLY)                              ->Decl_PairField
    valuefield      <- (IDNUM / LCURLY IDNUM RCURLY)                                      ->Decl_ValueField
    
    
    P <- {} => get_loc
    
    SKIP     <- (%SPACE / %COMMENT)

    NAME                  <- %NAME         SKIP*
    LCURLY                <- %LCURLY       SKIP*
    RCURLY                <- %RCURLY       SKIP*
    LBRACKET              <- %LBRACKET     SKIP*
    RBRACKET              <- %RBRACKET     SKIP*
    COLON                 <- %COLON        SKIP*
    DCOLON                <- %DCOLON       SKIP*
    SEMICOLON             <- %SEMICOLON    SKIP*
    DOT                   <- %DOT          SKIP*
    ALIASSYM              <- %ALIASSYM     SKIP*
    COMMA                 <- %COMMA        SKIP*
    ASSIGN                <- %ASSIGN       SKIP*
    RARROW                <- %RARROW       SKIP*
    LARROW                <- %LARROW       SKIP*
    INARROW               <- %INARROW      SKIP*
    OUTARROW              <- %OUTARROW     SKIP*
    RDIR                  <- %RDIR         SKIP*
    COORD                 <- %COORD        SKIP*
    ALGEBRAIC             <- %ALGEBRAIC    SKIP*
    ALIAS                 <- %ALIAS        SKIP*
    LOAD                  <- %LOAD         SKIP*
    VIEW                  <- %VIEW         SKIP*
    UNITS                 <- %UNITS        SKIP*
    DOMAIN                <- %DOMAIN       SKIP*
    DPROTO                <- %DPROTO       SKIP*
    FPROTO                <- %FPROTO       SKIP*
    SEMANTIC              <- %SEMANTIC     SKIP*
    DDR                   <- %DDR          SKIP*
    DR                    <- %DR           SKIP*
    COMPOSES              <- %COMPOSES     SKIP*
    USINGDDR              <- %USINGDDR     SKIP*
    FNAME                 <- %FNAME        SKIP*
    LIBRARY               <- %LIBRARY      SKIP*
    ARGS                  <- %ARGS         SKIP*
    CONVERSION            <- %CONVERSION   SKIP*
    SCALAR                <- %SCALAR       SKIP*
    VECTOR                <- %VECTOR       SKIP*
    MATRIX                <- %MATRIX       SKIP*
    NUMBER                <- %NUMBER       SKIP*
    VALUE                 <- %VALUE        SKIP*
    VNUMBER               <- %VNUMBER      SKIP*
    IDNUM                 <- %IDNUM        SKIP*
    STRINGLIT             <- %STRINGLIT    SKIP*
    DELEGATE              <- %DELEGATE     SKIP*
]]

--[[ Configure the static grammar with config/composable schema elements--]]
function gen_grammar(static_grammar)
--   dprotofields = {'dprotosemantic','dprotocoord','dprotoalgebraic','dprotodr','dprotoddr','dprotocomposes','dprotousingddr','dprotounits'}
--   local ok, dyn = preproc(static_grammar,
--     {table=table,
--      dproto_aggregate=table.concat(dprotofields,'/')})
--   if not ok then error(dyn) end
--   return dyn
  return static_grammar
end

local dynamic_grammar = gen_grammar(static_grammar)

local grammar = re.compile(dynamic_grammar, defs)

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

--[[ Filters --]]
for i,v in pairs(fieldtype) do
  parser['is_'..i] = function(obj,k)
    if obj._tag and obj._tag == "Field."..i then return true end
    return false
  end
end

-- inspect=require('inspect')
-- print(inspect(defs))

-- ret, err = parser.parse_file('dmodelsimple.test')
-- print(inspect(mapp:match('x=2,y=Z')))

return parser