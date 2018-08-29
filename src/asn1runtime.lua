--[[
    ASN1 Runtime for ASN1 Simple parser
   Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
   License: MIT
--]]


local asnparser = require('parser.asn1')
local utils     = require('utils')

--[[ Utility Functions --]]
--- Fiters for each operation in ASN1 ASTdata
local function op_constant(v,k)
  if v._tag and v._tag == 'Op.Constant' then return true end
  return false
end

local function op_basic(v,k)
  if v._tag and
      (v._tag == 'Op.Integer' or v._tag == 'Op.Boolean' or v._tag == 'Op.Real') then
    return true
  end
  return false
end

local function op_enumerated(v,k)
  if v._tag and v._tag == 'Op.Enumerated' then return true end
  return false
end

local function op_import(v,k)
  if v._tag and v._tag == 'Op.Import' then return true end
  return false
end

local function op_alias(v,k)
  if v._tag and v._tag == 'Op.Alias' then return true end
  return false
end

local function op_octet(v,k)
  if v._tag and v._tag == 'Op.Octet' then return true end
  return false
end

local function op_sequence(v,k)
  if v._tag and v._tag == 'Op.Sequence' then return true end
  return false
end

local function op_fieldname(v,k)
  if v._tag and v._tag == 'Op.FieldName' then return true end
  return false
end

local function op_seqof(v,k)
  if v._tag and v._tag == 'Op.FieldSeqOf' then return true end
  return false
end

local function op_custom(v,k)
  if op_sequence(v,k) or op_alias(v,k) then return true end
  return false
end

local op_filters = {
  enumerated = op_enumerated,
  basic      = op_basic,
  constant   = op_constant,
  import     = op_import,
  alias      = op_alias,
  octet      = op_octet,
  sequence   = op_sequence,
  custom     = op_custom,
  seqof      = op_seqof
}

-- generic get function on tags
local function get_withtag(defs,typeof)
  return utils.filter(op_filters[typeof],defs)
end

-- Initialise the context(s) from ASN1 file models

--[[ Note.
     There is no file resolution in the ASN1 compiler.
   file name may mismatch with the name of the module.
   The compiler takes a list of ASN1 models, and parse them
   all before to process the input -- We do the same
--]]
local function init_runtime(asn_files,ctx)
  --[[ Parser state --]]
  local pres    = {}            -- parsed results
  local ctx     = ctx or {}     -- ASN1 context(s) models
  
  for _,file in ipairs(asn_files) do
    local ret, err = asnparser.parse_file(file)
    if err then
      return false, 'error by parsing '..file
    end
    -- Indexing by module name
    local modulename = ret.name
    if not pres[modulename] then 
      pres[modulename] = ret.defs
    else 
      print('Warning (ASN1): '..modulename..'parsed already')
    end
  end
  
  -- Indexing in Runtime context
  --[[
    * gets the parsed results into tables indexed by name
    * create contexts for each ASN1 module
  --]]
  for nmodule,p in pairs(pres) do
    ctx[nmodule] = {}
    for i,v in pairs(p) do
      if v.name then ctx[nmodule][v.name] = v end
      if op_sequence(v) and v.fields then
        local ftab   = {}
        local fparam = {}
        for _,field in ipairs(v.fields) do
          if field._tag == "Op.FieldName" or field._tag == "Op.FieldSeqOf" then
            ftab[field.name] = field
          elseif field._tag == "Op.ParamList" then
            v.param = fparam
          end
        end
        v.fields = ftab
      end
    end
  end

  
  -- Importing external definitions in context(s)
  --[[ ASN1 has IMPORTS keyword for referencing
     local names to definitions expressed somewhere else.
   This avoid name collisions when multiple files/ASN1 models
   are loaded, if a definition having same name is defined
   in multiple places.
   Lookup for a definition should always go through namespaces
  ]]--
  for nmodule,p in pairs(pres) do
    local impt = get_withtag(p,'import')
    for _, v in ipairs(impt) do
      local from = ctx[v.from]
      local list = v.list
      for i,l in ipairs(list) do
        if not from[l] then
          print('Warning (ASN1): definition "'..l..'" not found in "'..v.from..'" -- needed by "'..nmodule..'"')
        end
        ctx[nmodule][l] = from[l]
      end
    end
  end
  
  return ctx
end

--[[ Utility functions to query the context --]]

-- Get list of available ASN1 modules
local function get_modules(ctx)
  local t = {}
  for i,v in pairs(ctx) do t[#t+1] =i end
  return t
end

-- Get list of definitions in namespace
local function get_defs_ns(ctx,cname)
  local t = {}
  for i,v in pairs(ctx[cname]) do
    t[#t+1] = i
  end
  return t
end

-- Get a ASN1 definition
local function get_def(ctx,cname,name)
  if not ctx[cname] then return false end
  return ctx[cname][name]
end

local function solve_field(ctx,ns,defname,hints,fieldname)
  local solve_fieldname = function(d)
    if d.fields then -- if sequence, only one field must be there
      local onlyone = false
      local fieldname
      for i,v in pairs(d.fields) do 
        if not onlyone then 
          onlyone = true
          fieldname=i
        else 
          onlyone = false
          break;
        end
      end
      if onlyone then return fieldname, true end
      local namelist = {}
      for i,v in pairs(d.fields) do
        namelist[#namelist+1] = i
      end
      return namelist, false
    end
  end
  
  -- Start here
  local hints = hints or {}
  local def = get_def(ctx,ns,defname)
  if not def then return false, 'definition not found' end
  
  -- (recursive) ending condition
  if op_basic(def) then return true end
  
  local fieldname = fieldname
  if not fieldname then
    fieldname, ok  = solve_fieldname(def)
    if not ok then
      local hint = hints[1]
      if not hint then
        return ok,  'Ambiguity in "'..defname..'", possible fields are "'..table.concat(fieldname,'", "')..'"'
      end
      hints = {unpack(hints,2)}
      fieldname = hint
    end
  end
  
  local field = def.fields[fieldname]
  if not field then return false, fieldname..': field not found in "'..defname end
  
  local ires, err = solve_field(ctx,ns,field.typeof,hints)

  if ires == false then return false, err end -- propagate error
  if ires == true then -- leaf obtained
    if op_seqof(field) then
      return {fieldname, function(idx) return 'arr['..tostring(idx)..']' end}
    end
    return {field.name} -- in this case, it should be equal to the dr value
  elseif type(ires) == 'table' then
    return {fieldname, unpack(ires)}
  end

  return false,'error'
end

-- Configurable: hook to a given ctx table or expose full API (more verbose)
return function(ctx)
  local M = {}
  if ctx then
    M.get_def     = function(cname,name) return get_def(ctx,cname,name) end
    M.get_def_ns  = function(cname) return get_def_ns(ctx,cname) end
    M.get_modules = function() return get_modules(ctx) end
    M.init_runtime= function(asn_files) return init_runtime(asn_files,ctx) end
    M.solve_field = function(ns,defname,hints,fieldname) return solve_field(ctx,ns,defname,hints,fieldname) end
  else
    M.get_def      = get_def
    M.get_def_ns   = get_def_ns
    M.get_modules  = get_modules
    M.init_runtime = init_runtime
    M.solve_field  = solve_field
  end
  for i,v in pairs(op_filters) do
    M['is_'..i] = v
  end
  return M
end