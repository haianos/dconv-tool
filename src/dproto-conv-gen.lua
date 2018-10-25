--[[
  == Dproto Conversion Tool ==
    This is part of MFCF framework by Enea Scioni, KU Leuven
   Shipped and customised for the ESROCOS project
   Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
   License: MIT
--]]

local utils   = require('utils')
local dparser = require('parser.dproto') --needed for utils
local eigen   = require('parser.eigen')
local asnctx = {}
local asnr   = require('asn1runtime')(asnctx)
local smdsdctx = {_defined={}}

--[[ Internal state of the tool --]]
-- Accessor provided by utility functions
local liblist    = {}        -- DB of loaded modules
local dproto     = {}        -- DB of dproto defs
local fproto     = {}        -- DB of fproto defs (conversion functions)
local algebraic  = {}        -- DB algebraic defs
local view       = {}        -- DB view defs
local alias      = {}        -- DB alias defs
local conversion = {}        -- DB conversion defs
local ddrdb      = {}        -- DB ddr definitions
local unitsdb    = {}        -- DB units definitions

--[[ Utility funciton accessors to internal state --]]
local function find_alias(n1,n2)
  for _, a in pairs(alias) do
    if a.lhs == n1 and a.rhs == n2 then
      return a.relation
    end
    if a.rhs == n1 and a.lhs == n2 then --swap
      local t = {}
      for i,r in pairs(a.relation) do
        t[i] = {}
        t[i][1] = r[2]
        t[i][2] = r[1]
      end
      return t
    end
  end
  return false
end

-- @param from name dproto 
-- @param to   name dproto
-- @return conversion fproto name
local function find_conversion(from,to)
  for i,v in pairs(conversion) do
    if from == v.from and to == v.to then
      return v.fproto
    end
  end
  return false, 'No conversion found for '..from..' -> '..to
end

local function find_fproto(name)
  return fproto[name]
end

local function find_dproto(name)
  return dproto[name]
end

local function find_algebraic(name)
  return algebraic[name]
end

local function find_ddr(name)
  return ddrdb[name]
end

local find_generic = {
  ddr       = find_ddr,
  algebraic = find_algebraic,
  dproto    = find_dproto,
  fproto    = find_fproto
}

local cast_anonym_list = {
  ddr = function(def)
    local tab = {
      _tag='ddr',
      mmid  = def.domain[1],
      mid = def.content
    }
    return tab
  end
}

local function cast_anonymdef(def,select)
  return cast_anonym_list[select](def)
end

local function todef(tab,opsel)
  if tab and tab._tag then
    if tab._tag == opsel then return tab, true end  --is already opsel
    if tab._tag == 'reference' then
      local ret = find_generic[opsel](tab.symname)
      return ret
    end
    if tab._tag == 'anonymdef' then --(has content and domain)
      return cast_anonymdef(tab,opsel)
    end
  end
  return false
end

--[[ Mapping functions ddr-algebraic --]]

-- Check out dproto parser 
--  * lhs ==> is Field.Name
--  * rhs ==> cover all fieldtype cases
local function scalar_map_a2ddr(dp)
  local dr  = dp.dr
  local m = {}
  for i,v in pairs(dr) do
    local key = v[2].name or tonumber(v[2].value)
    if dparser.is_Name(v[1]) then
      m[key] = v[1].name
    elseif dparser.is_Value(v[1]) then
      m[key] = tonumber(v[1].value)
    elseif dparser.is_Pair(v[1]) then
      m[key] = {tonumber(v[1].value1),tonumber(v[1].value2)}
    else  -- Must be DotNotation
      m[key] = v[1].sequence
    end
  end
  return m
end

local function vector_map_a2ddr(dp)
  local dr = dp.dr
  local m  = {}
  for i,v in pairs(dr) do
    local ddr_idx       = v[1]
    local algebraic_idx = v[2]
    if not dparser.is_Value(algebraic_idx) then
      -- TODO: Handle error message better
      print('Error in parsing, Value expected in ddr')
    end
    if dparser.is_Value(ddr_idx) then
      m[tonumber(algebraic_idx.value)] = tonumber(ddr_idx.value);
    elseif dparser.is_Pair(ddr_idx) then
      m[tonumber(algebraic_idx.value)] = {tonumber(ddr_idx.value1),tonumber(ddr_idx.value2)};
    else
      m[tonumber(algebraic_idx.value)] = ddr_idx.name -- must be a Name
    end
  end
  return m
end

local function matrix_map_a2ddr(dp)
  -- my dr model
  local dr = dp.dr
  -- indexing map
  local m = {}
  for i,v in pairs(dr) do
    local lhs = v[1]
    local rhs = v[2]
    if not dparser.is_Pair(rhs) then
      --TODO: handle this error better
      print('Error in parsing, Pair expected in ddr')
    end
    local i_idx = tonumber(rhs.value1)
    local j_idx = tonumber(rhs.value2)
    if not m[i_idx] then m[i_idx] = {} end
    if dparser.is_Value(lhs) then
     m[i_idx][j_idx] = tonumber(lhs.value)
    else  -- then it should be Pair
     m[i_idx][j_idx] = {tonumber(lhs.value1), tonumber(lhs.value2)}
    end
  end
  return m
end


--[[ utility accessors for DDBLX metatable (index/newindex) --]]

-- TODO: separation indexing map generation with the get
local function matrix_get(p,i,j)
  if not p.dproto._map_a2ddr then    -- generate mapping if not done already
    p.dproto._map_a2ddr = matrix_map_a2ddr(p.dproto)
  end
  local idx = p.dproto._map_a2ddr[i][j]
  if type(idx) == 'table' then
    return p.dblx[idx[1]][idx[2]]
  end
  return p.dblx[idx]
end

local function vector_get(p,idx)
  if not p.dproto._map_a2ddr then
    p.dproto._map_a2ddr = vector_map_a2ddr(p.dproto)
  end
--   print(idx,p.dproto._map_a2ddr[idx],inspect(p.dproto._map_a2ddr))
  return p.dblx[p.dproto._map_a2ddr[idx]]
end

local function vector_set(p,idx,value)
  if not p.dproto._map_a2ddr then
    p.dproto._map_a2ddr = vector_map_a2ddr(p.dproto)
  end
  p.dblx[p.dproto._map_a2ddr[idx]] = value
end

local function scalar_get(p,name)
  if not p.dproto._map_a2ddr then 
    p.dproto._map_a2ddr = scalar_map_a2ddr(p.dproto)
  end
  return p.dblx[p.dproto._map_a2ddr[name]]
end

local function matrix_set(p,i,j,value)
  if not p.dproto._map_a2ddr then
    p.dproto._map_a2ddr = matrix_map_a2ddr(p.dproto)
  end
  local idx = p.dproto._map_a2ddr[i][j]
  if type(idx) == 'table' then
    p.dblx[idx[1]][idx[2]] = value
    return
  end
  p.dblx[idx] = value
end

local function vector_set(p,idx,value)
  if not p.dproto._map_a2ddr then
    p.dproto._map_a2ddr = vector_map_a2ddr(p.dproto)
  end
  p.dblx[p.dproto._map_a2ddr[idx]] = value
end

local function scalar_set(p,name,value)
  p.dblx[p.dproto._map_a2ddr[name]] = value
end

-- Initialise the runtime of the Dproto Conversion Tool
-- @param: parsed content of .dproto model(s)
local function init_runtime(dprotofile,ddr_models)
  local parsed, err = dparser.parse_file(dprotofile)
  if not parsed then
    return false,  err
  end
  
  if ddr_models.asn_files then
    ok, err = asnr.init_runtime(ddr_models.asn_files)
    if not ok then return false, err end
  end
  
  if ddr_models.ros2 then              --load generator module (require ffi), for DDBLX
    rgen = require('ros.rclc-gen')
  end
  
  if ddr_models.smdsd_files then
     smdsd = require('smdsdruntime')(smdsdctx)
     ok, err = smdsd.init_runtime(ddr_models.smdsd_files)
     if not ok then return false, err end
     smdsd.enableffi(true)
  end
  
  for i,v in pairs(parsed) do                                       --DEVNOTE: make it structural
--   print(i,v)
    if v._tag == 'algebraic' then algebraic[v.name] = v end
    if v._tag == 'alias' then alias[#alias+1] = v end
    if v._tag == 'dproto' or v._tag == 'dproto_comp' then dproto[v.name] = v end
    if v._tag == 'fproto' then
      local tab  = {}
      local args = {}
      for _, arg in ipairs(v.args) do
        args[arg[1]] = { dir=arg[1], typeof = arg[2]}
      end
      tab.name    = v.name
      tab.library = v.library[1]
      tab.fname   = v.fname[1]
      fproto[v.name] = tab
    end
    if v._tag == 'load' then liblist[v.name] = false end
    if v._tag == 'view' then
      local lhs = v.lhs
      if type(lhs)=='table' and lhs._tag =='dotnotation' then
       lhs = table.concat(lhs.sequence,'.')
      end
      local rhs = v.rhs
      if type(lhs)=='table' and lhs._tag =='dotnotation' then
        rhs = table.concat(rhs.sequence,'.')
      end
      view[lhs..'->'..rhs] = v 
    end
    if v._tag == 'conversion' then conversion[#conversion+1] = v end
    if v._tag == 'ddr'   then ddrdb[v.name] = v end
    if v._tag == 'units' then unitsdb[v.name] = v end
  end
  
  -- defines internal maps
  --- from tags to fields
  for i,field in pairs(dproto) do
    for a,value in ipairs(field) do
      if value.sval then 
        field[value._tag] = value.sval
      else
        field[value._tag] = value 
      end
      field[a] = nil
    end
  end
  
  --DEVNOTE: make it structural
  for i,v in pairs(dproto) do
    if v.algebraic then
      v.algebraic = v.algebraic.symname -- HOTFIX (to be removed)
--       local alg = todef(v.algebraic,'algebraic')
--       if not alg then  -- should fails only if v.algebraic is 'reference' and not found
--         return false, 'Error in dproto model: using undefined <'..v.algebraic.symname..'> in "'..v.name..'" definition'
--       end
    
      if not algebraic[v.algebraic] then
        return false, 'Error in dproto model: using undefined <'..v.algebraic..'> in "'..v.name..'" definition'
      end
      local class = algebraic[v.algebraic].class
--       local class = alg.class
      if class == 'Scalar' then
        v._map_a2ddr = scalar_map_a2ddr(v)
      elseif class == 'Vector' then
        v._map_a2ddr = vector_map_a2ddr(v)
      elseif class == 'Matrix' then
        v._map_a2ddr = matrix_map_a2ddr(v)
      end
    end
    if v.composes then
      local comp = {}  -- remove TAG _tag=v.composes._tag}
      for _,b in ipairs(v.composes) do
        comp[b[1]] = b[2]
      end
      v.composes = comp
    end
    if v.ddr then --translate all definitions
      local ddr = todef(v.ddr,'ddr')
      v.ddr     = ddr
    end
  end
  
  -- enable DDBLX (dynamic)
  if next(liblist) ~= nil then 
    ffi=require('ffi') 
    for name,flag in pairs(liblist) do
      local ok, data = pcall(ffi.load,name)
      if not ok then error('Error in initialization: '..data..'\n (hint: is the library in the search path?)') end
      liblist[name] = data -- TODO PUT HERE RELATIVE CDEF TO LOAD
    end
  end
  
  --Everything went fine!
  return true
end


--[[ MMID  specific utility functions -- DEV: structural, another module --]]
--[[ ASN1 Specific --]]
--[[ Set getter and setters for uniform API on property --]]
local function asn1_api_getset(p)
  local address    = utils.split(tostring(p.dblx),': ')[2]
  local __tostring = function(self)
    return 'dblx<'..p.dproto.name..'>: ASN1 '..tostring(p.dblx)
  end
  
  local get_asntype = function(mid)
    local args = utils.split(mid,'%.')
    return asnr.get_def(args[1],args[2])
  end
  
  if p.dproto.algebraic then
    class = algebraic[p.dproto.algebraic].class
    if class == 'Scalar' then
       local mt = {
         __index = function(t,key) return t.accessor(key) end,
         __tostring = __tostring
       }
       setmetatable(p,mt)
    elseif  class == 'Vector' then
      print('WARNING, ASN1::Vector NYI')
    elseif  class == 'Matrix' then
      local mt = { __tostring = __tostring }
      local mid = p.dproto.ddr.mid
      local asntype = get_asntype(mid)
      if asnr.is_sequence(asntype) then
        mt.__call    = function(t,k1,k2)
          return '['..p.dproto._map_a2ddr[k1][k2]..']'
         end
      end --other cases NYI
      setmetatable(p,mt)
    else
      print('ERROR, ASN1:: UNKNOWN algebraic!')
    end
  end
end

--[[ Eigen Specific --]]

--[[ c99 Specific --]]
-- c99_dyn_api_getset DEPRECATED, use set_generic_dyn_api
local function c99_dyn_api_getset(p)
  local address    = utils.split(tostring(p.dblx),': ')[2]
  local __tostring = function(self)
    return 'dblx<'..p.dproto.name..'>: c99('..address..')'
  end
  
  if p.dproto.algebraic then
    if not algebraic[p.dproto.algebraic] then
      print('WARNING: is algebraic "'..p.dproto.algebraic..'" not defined?')
      return false
    end
    class = algebraic[p.dproto.algebraic].class
    if class == 'Scalar' then
      local mt = {
        __index = function(t,key) return scalar_get(t,key) end,
        __newindex = function(t,k,v) scalar_set(t,k,v) end,
        __tostring = __tostring
      }
      setmetatable(p,mt)
    elseif class == 'Vector' then
      local mt = {
        __index = function(t,key) return vector_get(t,key) end,
        __newindex = function(t,k,v) vector_set(t,k,v) end,
        __tostring = __tostring
      }
      setmetatable(p,mt)
    elseif class == 'Matrix' then
      local mt = {
        __index = function(t,k) return matrix_get(t,k[1],k[2]) end,
        __newindex = function(t,k,v) matrix_set(t,k[1],k[2],v) end,
        __call     = function(t,k1,k2) return matrix_get(t,k1,k2) end,
        __tostring = __tostring
      }
      setmetatable(p,mt)
    end
  end
  
  if p.dproto.composes then
    local mt = {
      __index = function(t,k) return rawget(t,'dblx')[k] end,
      __tostring = function(self)
        return 'dblx<'..p.dproto.name..'>: auto('..address..')'
     end
    }
    setmetatable(p,mt)
  end
end

--[[ Utils for handling dot notation]]--
local function extract_asn_mid(mid)
  local tab = utils.split(mid,'%.')
  return tab[1], tab[2]
end

--this should be replaced with dproto
local function get_drkey(dr,value)
  local key
  for i,v in pairs(dr) do
    if v == value then key = i; break; end
  end
  return key
end



local function split_names_index(key)
  local tab = utils.split(key,"%.")
  local names = {}
  local idx   = {}
  for _,v in ipairs(tab) do
    local n = tonumber(v)
    if n then idx[#idx+1] = n
    else names[#names+1]  = v
    end
  end
  return names, idx
end

local function place_indexes(qres,idx)
  local count = 1
  for i,v in pairs(qres) do
    if type(v) == 'function' then
      qres[i]=v(idx[count])
      count = count + 1
    end
  end
end

local function getdrkey(d,key)
  return d._map_a2ddr[key]
end

local accessor_symbol = {
  inner = {
    Eigen = {composite='', simple=''},
    c99   = {composite='.', simple='.'},
    ASN1  = {composite='.', simple='.'},
  },
  outer = {
    Eigen = {composite='', simple=''},
    c99   = {composite='.',simple=''},
    ASN1  = {composite='.', simple='.'}
  }
}

local function select_inner_access(ddr)
  return accessor_symbol.inner[ddr]
end

local function select_outer_access(ddr)
  return accessor_symbol.outer[ddr]
end


-- SDBLX == Static dblx instance generator
-- @param: (parsed) dproto definition, or its name (ID)
-- @return valid instance with accessor (code generator)
--- generic accessor + API that depends on algebraic
-- * Scalar: var.<name>
-- * Vector: var[idx]
-- * Matrix: var(i,j)
--- NOTE: this includes specifics for each MMID supported
local function SDBLX(dproto)
  local p = { _tag = 'sdblx'}
  local dproto = dproto
  if type(dproto) == 'string' then
    dproto = find_dproto(dproto)
  end
  p.dproto = dproto
  
  local ddr  = p.dproto.ddr
  local alg  = p.dproto.alg
  local dr   = p.dproto.dr
  local comp = p.dproto.composes
--   print('ddr in sdblx',ddr,inspect(ddr))
  ------ Check if composition
  if not ddr and comp then
    p.dblx = {}
    for _,v in pairs(dr) do
      local accessor_name = v[1].name
      local sname         = v[2].name
      local dname         = comp[sname]
      p.dblx[accessor_name] = SDBLX(dname)
    end
    -- Accessor for this composite
    p.accessor = function(k1,prefix)
      local prefix = prefix or ''
      local rval = {}
      local sub  = p.dblx[k1]
      local mt   = getmetatable(sub)
      local asym = '.'
      if sub.dproto.ddr then asym = select_inner_access(sub.dproto.ddr.mmid).composite or '' end
      mt.__index = function(t,k)
        return sub.accessor(k,prefix..k1..asym)
--         return sub.accessor(k,prefix..k1..'.')
      end
      setmetatable(rval,mt)
      return rval
    end
    
    local mt = { __index = function(t,k)
      local sub = p.dblx[k]
      local mti  = {}
      local rval = {}
      local asym = '.'
      if sub.dproto.ddr then asym = select_outer_access(sub.dproto.ddr.mmid).composite end
      mti.__index = function(tt,kval)
        return sub.accessor(kval,k..asym)
     -- return sub.accessor(kval,k..'.')
      end
      setmetatable(rval,mti)
      return rval
    end
    }
    
    setmetatable(p,mt)
    return p
  end
  
  ------------- ASN1
  if ddr.mmid == 'ASN1' then
    if not asnr.is_init() then
      return false, 'ASN1 runtime has not been initialised (probably no ASN1 models has been provided as input)'
    end
    local module, def = extract_asn_mid(ddr.mid)
    p.dblx = def
    local afnc = function(key,prefix)
      local prefix = prefix or ''
      local drkey = getdrkey(p.dproto,key)
      if not drkey then
        return false, 'key not valid'
      end
      
      -- DEVNOTE: This IMPL is a bit messy, due to
      -- original impl. of split_names_index
      -- if it is a (Field.)Seuqnce a bit stupid to recompose a string here: 
      -- FIXME reimpl split_names_index
      local names, idx
      if type(drkey) == 'string' then
        names, idx = split_names_index(drkey)
      elseif type(drkey) == 'number' then
        names = nil
        idx = {drkey}
      else
        names, idx = split_names_index(table.concat(drkey,'.')) 
      end
      --------- End Workaround
      ret, err = asnr.solve_field(module,def,names)
      if not ret then
        return false, err
      end
      place_indexes(ret,idx)
      return prefix..table.concat(ret,'.')
    end
    p.accessor = afnc
    asn1_api_getset(p)
  end
  
  ------------- Eigen
  if ddr.mmid == 'Eigen' then
    local obj = eigen.gen_obj(ddr.mid)
    local class = algebraic[p.dproto.algebraic].class
    if class == 'Scalar' or class == 'Vector' then
      p.accessor = function(key,prefix)
       local prefix = prefix or ''
       return prefix..obj[p.dproto._map_a2ddr[key]]
      end
      mt = {
        __index = function(t,k)
          return t.accessor(k)
        end
      }
      setmetatable(p,mt)
    else -- class == 'Matrix'
      mt = {
        __call = function(t,k1,k2)
          return obj:gen(k1,k2)
        end
      }
      p.accessor = function(k1,k2,prefix)
        return obj:gen(k1,k2,prefix)
      end
--       p.accessor = function(...)
--       print('acc',inspect({...}))
--        return obj:gen(...)
--       end
      setmetatable(p,mt)
    end
  end
  
  ------------- C99
  if ddr.mmid == 'c99' then
    -- A full c99 parser is not provided yet, ad-hoc discriminating of the
    -- useful definitions
    local index_vector =  function(key) return '['..key..']' end --for c99vectors
    local known_mid = {
      ['double[3]'] = index_vector,
      ['double[4]'] = index_vector,
      -- {_tag='Vector', size = 4},
      ['double[9]'] = index_vector
      -- {_tag='Vector', size = 9}
    }
    local _mid = known_mid[ddr.mid]
    if not _mid then
      print('c99 parser does not support the following mid: '..ddr.mid);
      return false
    end
    local class = algebraic[p.dproto.algebraic].class
    local mt
    if  class == 'Scalar' or class == 'Vector' then
      mt = {
        __index = function(t,k)
          return _mid(p.dproto._map_a2ddr[k])
        end
      }
    else
      mt = {
         __call = function(t,k1,k2)
           return _mid(p.dproto._map_a2ddr[k1][k2])
         end
      }
    end
    mt.__tostring = function(self)
      return 'dblx<'..p.dproto.name..'>: c99 '..tostring(p.dproto.ddr.mid)
    end
    setmetatable(p,mt)
  end
  return p
end


local function set_generic_dyn_api(p,tagname)
  local tagname = tagname or 'unknown'
  local mt = {
    __tostring = function(self)
        return 'dblx<'..p.dproto.name..'>: '..tagname..'('..tostring(p.dproto.ddr.mid)..')'
    end
  }
  
  if p.dproto.algebraic then
    if not algebraic[p.dproto.algebraic] then
      print('WARNING: is algebraic "'..tostring(p.dproto.algebraic)..'" defined?')
      return false
    end
    local class = algebraic[p.dproto.algebraic].class
    if class == 'Scalar' then
      mt.__index    = function(self,key) return scalar_get(self,key)   end
      mt.__newindex = function(self,key,v)      scalar_set(self,key,v) end
    elseif class == 'Vector' then
      mt.__index    = function(self,key) return vector_get(self,key)   end
      mt.__newindex = function(self,key,v)      vector_set(self,key,v) end
    elseif class == 'Matrix' then
      mt.__index = function(self,key) return matrix_get(self,key[1],key[2])   end
      mt.__newindex = function(self,key,v)   matrix_set(self,key[1],key[2],v) end
      mt.__call     = function(self,k1,k2) return matrix_get(self,k1,k2)      end
    end
  elseif p.dproto.usingddr then
    -- NOTE: after refactoring, this is (likely) never used!
    -- Passing to internal dblx directly
    -- TODO: this is valid only if dproto model matches! (dr is mapping 1:1)
    -- make proper check or raise an issue
    mt.__index = function(self,key) return p.dblx[key] end
    mt.__newindex = function(self,key,v) p.dblx[key] = v end
  elseif p.dproto.composes then
    local address = utils.split(tostring(p.dblx),': ')[2]
    mt.__index = function(t,k) return rawget(t,'dblx')[k] end
    mt.__tostring = function(self)
        return 'dblx<'..p.dproto.name..'>: auto('..address..')'
     end
  else
    print('WARNING: set_generic_dyn_api -- some corner cases not yet handled')
    return
  end
  
  setmetatable(p,mt)
end

local function set_mmid_c99(p)
  -- generate dblx (dproto instance)
  local nametype = p.dproto.ddr.mid
  -- MADE IT SAFE: FIXME proper error handling
  local ok, data = pcall(ffi.new,nametype,init)
  if not ok then error('allocation issues: '..data) end
  
  p.dblx = data
  -- register algebraic API
  set_generic_dyn_api(p,'c99')
end

local function set_mmid_ros(p)
  local ddr = p.dproto.ddr
  -- register dproto (if not done before)
--   rgen.register_and_load(p.dproto.ddr.mid)
  rgen.register_and_load(ddr.mid)
  -- generate dblx (dproto instance)
--   p.dblx = rgen.create_instance(p.dproto.ddr.mid)
  p.dblx = rgen.create_instance(ddr.mid)

  --- register algebraic API
  set_generic_dyn_api(p,'ROS')
end

local function set_mmid_smdsd(p)
  local nametype   =  utils.split(p.dproto.ddr.mid,'%.')
  local repo       = nametype[1]
  local objname    = nametype[2]
  smdsd.gen_all_c99(repo,objname)
  local ok, data = pcall(ffi.new,'SMDSD_'..objname,init)
  if not ok then error('allocation issues: '..data) end
  p.dblx = data
  set_generic_dyn_api(p,'SMDSD')
end

local gen_ctypename = {
  ROS = function(...) return rgen.gen_typename(...) end,
  c99 = function(mid) return mid end
}

local gen_ctypepointer = {
  ROS = function(...) return gen_ctypename.ROS(...)..'*' end,
  c99 = function(mid)
    local name = gen_ctypename.c99(mid)  
    reflect = require'reflect'
    local r = reflect(ffi.typeof(mid))
    if r.what == 'array' then
      local ret = r.element_type
      for i=1,r.pointer do
        ret = ret..'*'
      end
      return ret
    end
    --fallback version   
    return name
  end
}

local function get_ctype(ddr)
  local  name = gen_ctypepointer[ddr.mmid](ddr.mid)
  return name
end

local function attach_dproto(data,name)
  local tab = {_tag='dblx'}
  local d = find_dproto(name)
  if d then
    tab.dproto = d
    tab.dblx   = data
    set_generic_dyn_api(tab,'external')
    return tab
  end
  return false
end

local function create_reference(dpid, init)
  local dp    = find_dproto(dpid)
  local ddr   = dp.ddr
  local ctype = get_ctype(ddr)
  if ddr.mmid == 'ROS' then  rgen.register_and_load(ddr.mid) end
  local data = init
  if not init then data = ffi.new(ctype) end
  if init and type(init) == 'userdata' then
    data = ffi.cast(ctype,init)
  end
  return attach_dproto(data,dpid)
end

local function getraw(d)
  return d.dblx
end

local ddblx_mmid_support = {
  c99   = set_mmid_c99,
  ROS   = set_mmid_ros,
  SMDSD = set_mmid_smdsd
}

local function DDBLX(dproto)
  local p = { _tag = 'dblx'}
  local dproto = dproto
  if type(dproto) == 'string' then
    local  dprotoname = dproto
    dproto = find_dproto(dprotoname)
    if not dproto then
      print('Error, not possible to create instance of <'..dprotoname..'>: definition not found')
      return
    end
  end
  -- Set proto on return data
  p.dproto = dproto
  
  local ddr  = dproto.ddr --todef(dproto.ddr,'ddr')
  local alg  = p.dproto.alg
  local dr   = p.dproto.dr
  local comp = p.dproto.composes
  -- override on todef solution
--   p.dproto.ddr = ddr
  
  ------ Check if composition
  if not ddr and comp then
    if p.dproto.usingddr then
      ----------- FIXME (this could be done in init_runtime
      local ddr = {_tag='ddr'}
      for _,mdef in ipairs(p.dproto.usingddr) do
        ddr[mdef[1]] = mdef[2]
      end
      -----------------------------------------------------
      p.dproto.ddr = ddr
--       p.dproto.usingddr = nil
      p.dblx = DDBLX(p.dproto)
    else
      p.dblx = {}
      for _,v in pairs(dr) do
        local accessor_name = v[1].name
        local sname         = v[2].name
        local dname         = comp[sname]
        p.dblx[accessor_name] = DDBLX(dname)
      end
    end
    -- Accessor for this composite
--     c99_dyn_api_getset(p)
    set_generic_dyn_api(p,'auto')
    return p
  end
  
  ------------- ASN1
--   if ddr.mmid == 'ASN1' then
--     if not asnr.is_init() then
--       return false, 'ASN1 runtime has not been initialised (probably no ASN1 models has been provided as input)'
--     end
--     local module, def = extract_asn_mid(ddr.mid)
--     p.dblx = def
--     local afnc = function(key,prefix)
--       local prefix = prefix or ''
--       local drkey = getdrkey(p.dproto,key)
--       if not drkey then
--         return false, 'key not valid'
--       end
--       
--       -- DEVNOTE: This IMPL is a bit messy, due to
--       -- original impl. of split_names_index
--       -- if it is a (Field.)Seuqnce a bit stupid to recompose a string here: 
--       -- FIXME reimpl split_names_index
--       local names, idx
--       if type(drkey) == 'string' then
--         names, idx = split_names_index(drkey)
--       elseif type(drkey) == 'number' then
--         names = nil
--         idx = {drkey}
--       else
--         names, idx = split_names_index(table.concat(drkey,'.')) 
--       end
--       --------- End Workaround
--       ret, err = asnr.solve_field(module,def,names)
--       if not ret then
--         return false, err
--       end
--       place_indexes(ret,idx)
--       return prefix..table.concat(ret,'.')
--     end
--     p.accessor = afnc
--     asn1_api_getset(p)
--   end
  
  ------------- Eigen
--   if ddr.mmid == 'Eigen' then
--     local obj = eigen.gen_obj(ddr.mid)
--     local class = algebraic[p.dproto.algebraic].class
--     if class == 'Scalar' or class == 'Vector' then
--       p.accessor = function(key,prefix)
--        local prefix = prefix or ''
--        return prefix..obj[p.dproto._map_a2ddr[key]]
--       end
--       mt = {
--         __index = function(t,k)
--           return t.accessor(k)
--         end
--       }
--       setmetatable(p,mt)
--     else -- class == 'Matrix'
--       mt = {
--         __call = function(t,k1,k2)
--           return obj:gen(k1,k2)
--         end
--       }
--       p.accessor = function(k1,k2,prefix)
--         return obj:gen(k1,k2,prefix)
--       end
-- --       p.accessor = function(...)
-- --       print('acc',inspect({...}))
-- --        return obj:gen(...)
-- --       end
--       setmetatable(p,mt)
--     end
--   end

  if ddblx_mmid_support[ddr.mmid] then
    ddblx_mmid_support[ddr.mmid](p)
  end
  
  return p
end

local function CaptureOutput()
  local str = {value=''}
  str.write = function(me,value)
    me.value = me.value..value
  end
  local mt = {
    __tostring = function(t)
      return t.value
    end
 }
 setmetatable(str,mt)
 return str
end

local single_assignment = function(rhs,lhs,fd,rhsname,lhsname,idx)
  local fd  = fd or io.stdout
  local idx = idx or 0
  local ok, res = utils.preproc([[
$(space)$(lhsname)$(lhs) = $(rhsname)$(rhs);
]],
  {table=table, 
    space=utils.gen_spaces('\t',idx),
    rhsname = rhsname,
    lhsname = lhsname,
    lhs  = lhs,
    rhs = rhs
    })
  if not ok then error(res) end
  fd:write(res)
end

-- generate call biop
local fblx_call = function(fnc,fd,rhsname,lhsname,idx)
  local fd  = fd or io.stdout
  local idx = idx or 0
  --remove accessor to last if present
  local rhsname = rhsname
  if rhsname:sub(-1) == '.' or rhsname:sub(-1) == '>' then
    rhsname = rhsname:sub(1,-2)
  end
  local lhsname = lhsname
  if lhsname:sub(-1) == '.' or lhsname:sub(-1) == '>' then
    lhsname = lhsname:sub(1,-2)
  end
  local ok, res = utils.preproc([[
$(space)$(fnc)($(rhsname),$(lhsname));
]],
  {table=table, 
    space=utils.gen_spaces('\t',idx),
    rhsname = rhsname,
    lhsname = lhsname,
    fnc     = fnc
    })
  if not ok then error(res) end
  fd:write(res)
end

-- check if obj is a composite model (geometric)
local function is_composition(m)
  local obj = m.dproto or m
  if obj._tag == 'dproto_comp' then return true end
  return false
end

local function check_semantics(m1,m2)
  local lhs = m1.dproto or m1
  local rhs = m2.dproto or m2
  if lhs.semantic == rhs.semantic then return true end
  return false
end

local function check_semantics_composition(m1,m2)
  --NOTE: impl not fast...
  local obj1 = m1.dproto.composes or m1
  local obj2 = m2.dproto.composes or m2
  for i,v in pairs(obj1) do
    if not obj2[i] then return false end
  end
  --cross-check
  for i,v in pairs(obj2) do
    if not obj1[i] then return false end
  end
  return true
end

local function same_coord(m1,m2)
  local lhs = m1.dproto or m1
  local rhs = m2.dproto or m2
  if lhs.coord == rhs.coord then return true end
  return false
end

local function same_algebraic(m1,m2)
  local lhs = m1.dproto or m1
  local rhs = m2.dproto or m2
  if lhs.algebraic == rhs.algebraic then return true end
  return false
end

-- check if dproto pairs is directly convertible;
-- should reflect checks in 'convert' function
-- NOTE: unidirectional relationship (e.g., by conv function)
local function check_convertibility(d1,d2)
  if not check_semantics(d1,d2) then return false end
  
  if not same_coord(d1,d2) then
    local fblx_name = find_conversion(d1.name,d2.name)
    if not fblx_name then return false end
    return true
  end
  
  if not same_algebraic(d1,d2) then
    --find alias
    local ar = find_alias(d1.algebraic,d2.algebraic)
    if not ar then return false end
  end

  return true
end

-- Find affinity conversion a dproto with other dprotos
-- @return list of dproto names
local function find_affinity(name)
  local tgt = find_dproto(name)
--   local t   = utils.filter(function(v,k)
--     if check_convertibility(tgt,v) then return true end
--   end, dproto)
  local t   = utils.map(function(v,k)
    if check_convertibility(tgt,v) then return v.name end
  end, dproto)
  return t
end

-- "brute-force" heuristic to find
-- indirect path conversions
local function search_conv_path(src,tgt)
  local found = function(tab,n)
    for i,v in pairs(tab) do
      if n == v then return true end
    end
  end
  
  local allcheck = function(tab)
    for i,v in pairs(tab) do
      if not v then return false end
    end
  end
  
  -- Init
  local checked = {}
  for i,v in pairs(dproto) do
    checked[v.name] = false
  end
  checked[src] = true
  
  search_step = function(s,tgt)
    local af = find_affinity(s)
--     print(s,inspect(af))
    checked[s] = true
    -- set empty, not found
    if #af == 0 then
--       print('affinity not found')
      return false
    end
    
    -- found?
    if found(af,tgt) then
--       print('found!')
      return tgt
    end
    
    --checked them all?
    if allcheck(checked) then
--       print('all checked, not found')
      return false
    end
    
    -- remove checked
    for i,v in pairs(af) do
      if checked[v]  then af[i] = nil end
    end
    
    -- continue to search
    for i,v in pairs(af) do
      local res = search_step(v,tgt)
      if res then
        if type(res) == 'string' then return {v,res} end
        return {v, unpack(res)}
      end
    end
  end
  
  return search_step(src,tgt)
end

local function compute_constructor(d,n)
  -------- c99
  if d.ddr.mmid == 'c99' then
    -- A full c99 parser is not provided yet, ad-hoc discriminating of the
    -- useful definitions
    local known_mid = {
      ['double[3]'] = {_tag='Vector', typeof='double', size = 3},
      ['double[4]'] = {_tag='Vector', typeof='double', size = 4},
      ['double[9]'] = {_tag='Vector', typeof='double', size = 9}
    }
    local _mid = known_mid[d.ddr.mid]
    if not _mid then
      print('c99 parser does not support the following mid: '..ddr.mid);
      return false
    end
    return _mid.typeof..' '..n..'['.._mid.size..'];'
  end
  
  return false, 'not implemented'
end

local function compute_tmpvars(path)
  local cstr  = {}
  local names = {}
  for i=2,#path-1 do -- not the extremes
    names[i] = path[i]..'_tmp_'..i
    cstr[i]  = compute_constructor(find_dproto(path[i]),names[i])
  end
  return cstr, names
end

-- @return:
--  * conversion fails <false,errmsg>
--  * conversion succeed <fnc,state>
local function convert_sdblx(m1name,m2name)
  local p1 = dproto[m1name] or false
  local p2 = dproto[m2name]
  if (not p1) or (not p2) then
    return false, 'data model not found, request ('..tostring(m1name)..', '..tostring(m2name)..')'
  end
  
  local m1, err = SDBLX(p1)
  if not m1 then 
    print('Error on "'..m1name..'": '..err); return;
  end
  local m2, err = SDBLX(p2)
  if not m2 then 
    print('Error on "'..m1name..'": '..err); return;
  end

  
  -- First, check semantics
  if not check_semantics(m1,m2) then
    local n1 = m1.dproto.name
    local n2 = m2.dproto.name
    local s1 = m1.dproto.semantic
    local s2 = m2.dproto.semantic
    return false, 'Semantics of "'..n1..'" and "'..n2..'" are not compatible ("'..s1..'","'..s2..'")'
  end
  
  -- is composition (both must be composite)
  if is_composition(m1) and is_composition(m2) then
    --composition must match
    local c1 = m1.dproto.composes
    local c2 = m2.dproto.composes

    if not check_semantics_composition(m1,m2) then
      return false, 'semantic of the composition not compatible for conversion between '..m1.dproto.name..' and '..m2.dproto.name
    end
    
    
    return function(fd,rhsname,lhsname,idx)
      local fd = fd or io.stdout
      local rhsname = rhsname or ''
      local lhsname = lhsname or ''
      local find_keyname = function(d,value)
        for i,v in pairs(d) do
          if value == v[2].name then return v[1].name end
        end
      end
--       for s,dname in pairs(c1) do
      for s,dname in utils.pairs_order(c1) do  --Make it ordered, so the generated output is deterministic
        local d1 = find_dproto(dname)
        if not d1.composes then
          local n1 = find_keyname(m1.dproto.dr,s)
          local n2 = find_keyname(m2.dproto.dr,s)
          local d1mmid = d1.ddr.mmid
          local d2mmid = find_dproto(c2[s]).ddr.mmid
          local lhssym = accessor_symbol.inner[d1mmid].simple or '.'
          local rhssym = accessor_symbol.inner[d2mmid].simple or '.'
          local gen, err = convert_sdblx(dname,c2[s])
          gen(fd,rhsname..n1..lhssym,lhsname..n2..rhssym)
        else
          local gen, err = convert_sdblx(dname,c2[s])
          local n1 = find_keyname(m1.dproto.dr,s)
          local n2 = find_keyname(m2.dproto.dr,s)
          gen(fd,rhsname..n1..'.',lhsname..n2..'.')
        end
      end
    end
  end
  
  
  -- Check Coordinate representation
  if not same_coord(m1,m2) then
    local n1 = m1.dproto.name
    local n2 = m2.dproto.name
    local fblx_name = find_conversion(n1,n2)
    if fblx_name then
      --       return false, 'Conversion function exists, but generator not implemented yet'
      return function(fd,rhsname,lhsname,idx)
        return fblx_call(fblx_name,fd,rhsname,lhsname,idx)
      end
    end
    local path = search_conv_path(n1,n2)
    if path then
      path = {n1, unpack(path)}
      cstr, names = compute_tmpvars(path)
      local conres = {}
      for cnum=1,#path-1 do
        conres[cnum] = convert_sdblx(path[cnum],path[cnum+1])
      end
      return function(fd,rhsname,lhsname,idx)
        local fd = fd or io.stdout
        local sn = {lhsname}
        for i,v in pairs(names) do
          sn[#sn+1] = v
        end
        sn[#sn+1] = rhsname
--         print(inspect(sn))
        for i,v in pairs(cstr) do
          fd:write(v..'\n')
        end
        fd:write('\n')
        for i,v in pairs(conres) do
          v(fd,sn[i],sn[i+1],idx)
          fd:write('\n')
        end
      end
--       return false, 'Conversion possible by indirection "'..table.concat(path,'.')..'", but not implemented yet'
    end

    local n1 = m1.dproto.name
    local n2 = m2.dproto.name
    local c1 = m1.dproto.coord
    local c2 = m2.dproto.coord
    return false, 'Coordinate representation <coord> of "'..n1..'" and "'..n2..'" are not compatible ("'..c1..'","'..c2..'")'
  end

  -- Algebraic: the same or an alias exists
  if same_algebraic(m1,m2) then
    local alg = algebraic[m1.dproto.algebraic]
    if alg.class == 'Scalar' then
      return function(fd,rhsname,lhsname,idx)
        for _,e in pairs(alg.elements) do
          single_assignment(m1[e],m2[e],fd,rhsname,lhsname,idx)
        end
      end
    elseif alg.class == 'Matrix' then
--                         inspect=require('inspect')
--                         print(inspect(m1))
      return function(fd,rhsname,lhsname,idx)
        for i=0,alg.elements[2]-1 do
          for j=0,alg.elements[2]-1 do
            single_assignment(m1(i,j),m2(i,j),fd,rhsname,lhsname,idx)
          end
        end
      end
    end
  else -- Non-trivial, check for alias
    local n1 = m1.dproto.name
    local n2 = m2.dproto.name
    local a1 = m1.dproto.algebraic
    local a2 = m2.dproto.algebraic
    local ar = find_alias(a1,a2)
    if not ar then
      return false, 
          'Algebraic of "'..n1..'" and "'..n2..'" are not compatible or alias not defined for ("'..a1..'", "'..a2..'")'
    end
    return function(fd,rhsname,lhsname,idx)
      for _,keys in pairs(ar) do
--         print(keys[1]._tag,keys[2]._tag)
        -- FIXME: Pair case not implemented yet
        if dparser.is_Pair(keys[1]._tag) or dparser.is_Pair(keys[1]._tag) then
          return false, 'Alias with PAIRS not implemented yet'
        end
        local k1 = keys[1].name or tonumber(keys[1].value)
        local k2 = keys[2].name or tonumber(keys[2].value)
        single_assignment(m1[k1],m2[k2],fd,rhsname,lhsname,idx)
      end
    end
    

  end
  
  return false, 'Some corner cases not handled yet in this implementation'
end

local function convert_ddblx(src,tgt)

  -- First, check semantics
  if not check_semantics(src,tgt) then
    local n1 = src.dproto.name
    local n2 = tgt.dproto.name
    local s1 = src.dproto.semantic
    local s2 = tgt.dproto.semantic
    return false, 'Semantics of "'..n1..'" and "'..n2..'" are not compatible ("'..s1..'","'..s2..'")'
  end
  
  -- is composition (both must be composite)
  if is_composition(src) and is_composition(tgt) then
    --composition must match
    local c1 = src.dproto.composes
    local c2 = tgt.dproto.composes
    
    if not check_semantics_composition(src,tgt) then
      return false, 'semantic of the composition not compatible for conversion between '..src.dproto.name..' and '..tgt.dproto.name
    end

    -------------------- CLEAN THIS, clone if usingddr -------------------
    -- create clone
    local adblx = {}
    adblx.dproto = find_dproto('ros_position')
    adblx.dblx = src.position
--     adblx.dblx = src.position.dblx
    set_generic_dyn_api(adblx)
    local res, err = convert_ddblx(adblx,tgt.position)
    if not res then error(err) end

    adblx = {}
    adblx.dproto = find_dproto('ros_quaternion')
    adblx.dblx   = src.orientation
--     adblx.dblx = src.orientation.dblx
    set_generic_dyn_api(adblx)
    local res, err = convert_ddblx(adblx,tgt.orientation)
    if not res then error(err) end
    -------------------- CLEAN THIS, clone if usingddr -------------------
        
    return true
  end
  
  -- Check Coordinate representation
  if not same_coord(src,tgt) then
    local n1 = src.dproto.name
    local n2 = tgt.dproto.name
    local fblx_name = find_conversion(n1,n2)
    --------------------------------------------------------clean-me up: TODO FIXME
    ffi.cdef('void quat2rot(geometry_msgs__msg__Quaternion* quat, double rot[9]);')
    -------------------------------------------------------------------------------
    if fblx_name then
      local fblx = find_fproto(fblx_name)
      --TODO check args
      local ok, res = pcall(liblist[fblx.library][fblx.fname],src.dblx,tgt.dblx)
      if not ok then 
        return false, 'Conversion call of '..tostring(fblx.fname)..' failed: '..res
      end
      return true
    end
    
    return false, 'coord conversion, -- NYI INDIRECT'
  end
  
  -- Algebraic: the same or an alias exists
  if same_algebraic(src,tgt) then
--     print('PLAIN',src.dproto.name,tgt.dproto.name)
    local alg = algebraic[src.dproto.algebraic]
    if alg.class == 'Scalar' then
      for _,e in pairs(alg.elements) do
        tgt[e] = src[e]                  --copying...
      end
      return true
    elseif alg.class == 'Matrix' then
      for i=0,alg.elements[2]-1 do
        for j=0,alg.elements[2]-1 do
          tgt[{i,j}] = src(i,j)
        end
      end
      return true
    elseif alg.class == 'Vector' then
      for i=0,tonumber(alg.elements[1])-1 do
        tgt[i] = src[i]
      end
      return true
    end
    return false, 'same algebraic, unknown error during conversion'
  else                                                                -- Non-trivial, check for aliases
--   print('ALIAS',src.dproto.name,tgt.dproto.name)
    local a1 = src.dproto.algebraic
    local a2 = tgt.dproto.algebraic
    local ar = find_alias(a1,a2)
    if not ar then
      return false, 'algebraic not found'
    end
    for _, keys in pairs(ar) do
      local k1 = keys[1].name or tonumber(keys[1].value)
      local k2 = keys[2].name or tonumber(keys[2].value)
      tgt[k2] = src[k1]
    end
    return true
  end
    
  print('Dynamic conversion, TODO')
  
  return false, 'Some corner cases not handled yet in this implementation'
end
                        

--[[ Dispatcher for converting functions --]]
local function convert(src,tgt)
  if type(src) == 'string' and type(tgt) == 'string' then
    return convert_sdblx(src,tgt)
  end
  
  if src._tag == 'dblx' and tgt._tag == 'dblx' then
    return convert_ddblx(src,tgt)
  end
  print('Conversion Failure: wrong arguments')
end

local M = {}

M.init_runtime      = init_runtime
M.place_indexes     = place_indexes
M.extract_asn_mid   = extract_asn_mid
M.split_names_index = split_names_index
M.get_drkey         = get_drkey           --DEPRECATED
M.SDBLX             = SDBLX
M.DDBLX             = DDBLX
M.attach_dproto     = attach_dproto
M.getraw            = getraw
M.convert           = convert
M.CaptureOutput     = CaptureOutput
M.create_reference  = create_reference
-- M.check_convertibility = check_convertibility --debug only
-- M.find_conversion =find_conversion            --debug only
M.get_ddrdb = function() return ddrdb end        --dev only
M.get_unitsdb = function() return unitsdb end    --dev only
M.get_dproto = find_dproto                       --dev only
M.find_alias = find_alias                        --dev only
M.get_ctype  = get_ctype
return M
