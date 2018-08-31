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

--[[ Internal state of the tool --]]
-- Accessor provided by utility functions
local dproto     = {}        -- DB of dproto defs
local algebraic  = {}        -- DB algebraic defs
local view       = {}        -- DB view defs
local alias      = {}        -- DB alias defs
local conversion = {}        -- DB conversion defs

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

local function find_dproto(name)
  return dproto[name]
end

local function find_algebraic(name)
  return algebraic[name]
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
  
  for i,v in pairs(parsed) do                                       --DEVNOTE: make it structural
    if v._tag == 'Op.Algebraic' then algebraic[v.name] = v end
    if v._tag == 'Op.Alias' then alias[#alias+1] = v end
    if v._tag == 'Op.Dproto' or v._tag == 'Op.Dproto_comp' then dproto[v.name] = v end
    if v._tag == 'Op.View' then
      local lhs = v.lhs
      if type(lhs)=='table' and lhs._tag =='Op.DotNotation' then
       lhs = table.concat(lhs.sequence,'.')
      end
      local rhs = v.rhs
      if type(lhs)=='table' and lhs._tag =='Op.DotNotation' then
        rhs = table.concat(rhs.sequence,'.')
      end
      view[lhs..'->'..rhs] = v 
    end
    if v._tag == 'Op.Conversion' then conversion[#conversion+1] = v end
  end
  
  -- defines internal maps
  --DEVNOTE: make it structural
  for i,v in pairs(dproto) do
    if v.algebraic then
      if not algebraic[v.algebraic] then
        return false, 'Error in dproto model: using undefined <'..v.algebraic..'> in "'..v.name..'" definition'
      end
      local class = algebraic[v.algebraic].class
      if class == 'Scalar' then
        v._map_a2ddr = scalar_map_a2ddr(v)
      elseif class == 'Vector' then
        v._map_a2ddr = vector_map_a2ddr(v)
      elseif class == 'Matrix' then
        v._map_a2ddr = matrix_map_a2ddr(v)
      end
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
  
  if p.dproto.algebraic then
    class = algebraic[p.dproto.algebraic].class
    if class == 'Scalar' then
       local mt = {
         __index = function(t,key) return t.accessor(key) end,
         __tostring = __tostring
       }
       setmetatable(p,mt)
    end
  end
end

--[[ Eigen Specific --]]

--[[ c99 Specific --]]

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
  local p = { _tag = 'dblx'}
  local dproto = dproto
  if type(dproto) == 'string' then
    dproto = find_dproto(dproto)
  end
  p.dproto = dproto
  
  local ddr  = p.dproto.ddr
  local alg  = p.dproto.alg
  local dr   = p.dproto.dr
  local comp = p.dproto.composes
  
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
    setmetatable(p,mt)
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
  if obj._tag == 'Op.Dproto_comp' then return true end
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
local function convert(m1name,m2name)
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
          local gen, err = convert(dname,c2[s])
          gen(fd,rhsname..n1..lhssym,lhsname..n2..rhssym)
        else
          local gen, err = convert(dname,c2[s])
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
        conres[cnum] = convert(path[cnum],path[cnum+1])
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

local M = {}

M.init_runtime      = init_runtime
M.place_indexes     = place_indexes
M.extract_asn_mid   = extract_asn_mid
M.split_names_index = split_names_index
M.get_drkey         = get_drkey           --DEPRECATED
M.SDBLX             = SDBLX
M.convert           = convert
M.CaptureOutput     = CaptureOutput
-- M.check_convertibility = check_convertibility --debug only
-- M.find_conversion =find_conversion            --debug only
return M
