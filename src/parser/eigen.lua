--[[
    Simple parser and utilities to support Eigen-based
   datatypes/datamodel for static code generation
   Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
   License: MIT
--]]

local lpeg     = require('lpeglabel')
local re       = require('relabel')

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

local decl= typedecl("Decl", {
  Decl = {
    Matrix = {'name','rows','cols'},
    Vector = {'name','size'}
  }
})

local lexer = {}
local defs  = {}

local symbols = {
  LTEMP = "<", 
  RTEMP = ">", 
  COMMA = ",",
  SEMICOLON = ";"
}

local keywords = {
  'Matrix',
  'typedef'
}
local captured_keywords = {
  'Matrix',
}

for i,v in pairs(symbols) do
  lexer[i] = lpeg.P(v)
end


local idstart = lpeg.P("_") + lpeg.R("AZ", "az")
local idrest  = lpeg.P("_") + lpeg.R("AZ", "az", "09")
local possiblename = idstart * idrest^0

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

lexer.SPACE = lpeg.S(" \t\n\v\f\r")^1
lexer.IDNUM = lpeg.C(lpeg.R("09")^1)

-- Support for Vector typedefs in Eigen library
lexer.VECTOR = lpeg.C(lpeg.P("Vector")) * lpeg.C(lpeg.R("09")^1) * lpeg.C(lpeg.P('f') + lpeg.P('d'))

for tokname, tokpat in pairs(lexer) do 
  defs[tokname] = tokpat
end

defs['Decl_Unnamed'] = function(...)
  local args = {...}
  if args[1] == 'Matrix' then
    if #args == 4 then -- expected 4 if it has precision
      return decl.Matrix('unnamed',args[3],args[4]) --args[2] is precision, don't care
    else  --expected 3 if has no precision (shortcut)
      return decl.Matrix('unnamed',args[2],args[3])
    end
  else  -- must be Vector
    return decl.Vector('unnamed',args[2]) --args[2] is precision, don't care
  end
end

defs['Decl_Named'] = function(...)
  local args = {...}
  if args[1] == 'Matrix' then
    if #args == 5 then -- expected 5 if it has precision
      return decl.Matrix(args[5],args[3],args[4]) --args[2] is precision, don't care
    else   --expected 4 if no precision (shortcut)
      return decl.Matrix(args[4],args[2],args[3])
    end
  else  -- must be Vector
    return decl.Vector(args[4],args[2])         --args[3] is precision, don't care
  end
end

local grammar =
[[
  program  <- SKIP* ( unnamed
                     / named )* !.
  
  unnamed   <- (matrix / VECTOR)                         ->Decl_Unnamed
  named     <- (TYPEDEF (matrix/VECTOR) NAME SEMICOLON)  ->Decl_Named
  
  matrix    <- (MATRIX LTEMP (NAME COMMA)? IDNUM COMMA IDNUM RTEMP)

  SKIP      <- (%SPACE)
  
  NAME      <- %NAME      SKIP*
  LTEMP     <- %LTEMP     SKIP*
  RTEMP     <- %RTEMP     SKIP*
  TYPEDEF   <- %TYPEDEF   SKIP*
  SEMICOLON <- %SEMICOLON SKIP*
  COMMA     <- %COMMA     SKIP*
  IDNUM     <- %IDNUM     SKIP*
  MATRIX    <- %MATRIX    SKIP*
  VECTOR    <- %VECTOR    SKIP*
]]

local grammar = re.compile(grammar, defs)


local function parse_string(str)
  local op, err, errpos = grammar:match(str)
  if op then return op end
  return false
end

local M = {}
local primitives = { 'Matrix', 'Vector' }
for _,v in pairs(primitives) do
  M['is_'..v] = function(obj,k)
    if obj._tag and obj._tag == 'Decl.'..v then return true end
    return false
  end
end

local function gen_accessor(obj)
  local mt  = {}
  if M.is_Matrix(obj) then
    mt = {
      __index = function(t,key)return '('..key[1]..','..key[2]..')'
      end
    }
    obj.gen = function(self,i,j,prefix)
      local prefix = prefix or ''
      return prefix..self[{i,j}]
     end
  end
  
  if M.is_Vector(obj) then
    mt = {
      __index = function(t,key) return '('..key..')'
      end
    }
    obj.gen = function(self,i)
      return self[i]
     end
  end
  
  setmetatable(obj,mt)
  return obj
end

M.parse_string = parse_string
M.gen_accessor = gen_accessor
M.gen_obj      = function(str) return gen_accessor(parse_string(str)) end

return M


