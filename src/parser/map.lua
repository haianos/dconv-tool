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

local lexer   = {}
local defs    = {}

local symbols = {
  COMMA  = ',',
  LCURLY = '{',
  RCURLY = '}',
  DOT    = '.',
  ASSIGN = '='
}

for i,v in pairs(symbols) do
  lexer[i] = lpeg.P(v)
end

local fieldtype= typedecl("Field", {
    Decl = {
      Name        = {'name'},
      Value       = {'value'},
      Pair        = {'value1','value2'},
      DotNotation = {'sequence'}
    }
})

lexer.SPACE = lpeg.S(" \t\n\v\f\r")^1
lexer.IDNUM = lpeg.C(lpeg.R("09")^1)

local idstart = lpeg.P("_") + lpeg.R("AZ", "az")
local idrest  = lpeg.P("_") + lpeg.P("-") + lpeg.R("AZ", "az", "09")
local possiblename = idstart * idrest^0
lexer.NAME = possiblename

local comment_start = lpeg.P("//")
local short_comment = comment_start * (lpeg.P(1) - lpeg.P("\n"))^0 * lpeg.P("\n")^-1
lexer.COMMENT = short_comment

--[[ LEXER BUILDING       --]]
for tokname, tokpat in pairs(lexer) do 
  defs[tokname] = tokpat
end

--[[ DECLARATION BUILDING --]]
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

defs['Decl_Test'] = function(...)
 local args={...}
 print(inspect(args))
 return args
end

--[[ GRAMMAR BUILDING    --]]
local gdef =
[[
  map       <- SKIP*  {| (mapdef ((COMMA)? mapdef)* ) |} !.
  mapdef    <- {| (mapel ASSIGN mapel) |}
                 
  mapel     <- ( drdotnot
                 / namefield 
                 / valuefield
                 / pairfield)                                                           
                    
  -- definition of dot notation in dr, to allow tree traversal of inner datastructs
  drdotnot        <- ( (NAME /IDNUM) (DOT (NAME /IDNUM))^1 (DOT (NAME /IDNUM))* )       ->Decl_DotNotationField
  namefield       <- NAME                                                               ->Decl_NameField
  pairfield       <- (LCURLY IDNUM (COMMA IDNUM)^1 RCURLY)                              ->Decl_PairField
  valuefield      <- (IDNUM / LCURLY IDNUM RCURLY)                                      ->Decl_ValueField
  
  SKIP     <- (%SPACE / %COMMENT)
  
  NAME                  <- %NAME         SKIP*
  LCURLY                <- %LCURLY       SKIP*
  RCURLY                <- %RCURLY       SKIP*
  COMMA                 <- %COMMA        SKIP*
  ASSIGN                <- %ASSIGN       SKIP*
  DOT                   <- %DOT          SKIP*
  IDNUM                 <- %IDNUM        SKIP*
]]

local grammar = re.compile(gdef,defs)

-- content = "m=0,m=1,m=2"

-- op,err = grammar:match(content)
-- print(inspect(op))

return grammar
