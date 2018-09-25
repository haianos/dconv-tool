--[[ 
    Lua FFI reflection for luaffi  Library 
  Implements reflection for ffi ctypes by using ffi itself.
  Useful if reflection is not implemented natively in the ffi library
   
  Inspired by reflect.lua lbrary for LuaJIT by Peter Cawley, <lua@corsix.org>.
  Author: Enea Scioni, <enea.scioni@kuleuven.be>
--]]

local ffi = require("ffi")

--[[ The following definitions are in "ffi.h" ]]--
local dtypes = {
  'invalid',
  'void',
  'float',
  'double',
  'long_double',
  'complex_float',
  'complex_double',
  'complex_long_double',
  'bool',
  'int8',
  'int16',
  'int32',
  'int64',
  'intptr',
  'enum',
  'union',
  'struct',
  'function',
  'functionptr'
}

--  WIP
-- ffi.cdef[[
--   struct _ffi_ctype { /* renaming to reserve ffi namespace, avoid future conflicts (original is 'struct ctype') */
--     size_t base_size; /* size of the base type in bytes */
-- 
--     union {
--         /* valid if is_bitfield */
--         struct {
--             /* size of bitfield in bits */
--             unsigned bit_size : 7;
--             /* offset within the current byte between 0-63 */
--             unsigned bit_offset : 6;
--         };
--         /* Valid if is_array */
--         struct {
--           size_t array_size;
--           size_t fix_mem[3];
--         };
--         /* Valid for is_variable_struct or is_variable_array. If
--          * variable_size_known (only used for is_variable_struct) then this is
--          * the total increment otherwise this is the per element increment.
--          */
--         size_t variable_increment;
--     };
--     size_t offset;
--     unsigned align_mask          : 4; /* as (align bytes - 1) eg 7 gives 8 byte alignment */
--     unsigned pointers            : 2; /* number of dereferences to get to the base type including +1 for arrays */
--     unsigned const_mask          : 4; //POINTER_MAX + 1; /* const pointer mask, LSB is current pointer, +1 for the whether the base type is const */
--     unsigned type                : 5; /* value given by type enum above */
--     unsigned is_reference        : 1;
--     unsigned is_array            : 1;
--     unsigned is_defined          : 1;
--     unsigned is_null             : 1;
--     unsigned has_member_name     : 1;
--     unsigned calling_convention  : 2;
--     unsigned has_var_arg         : 1;
--     unsigned is_variable_array   : 1; /* set for variable array types where we don't know the variable size yet */
--     unsigned is_variable_struct  : 1;
--     unsigned variable_size_known : 1; /* used for variable structs after we know the variable size */
--     unsigned is_bitfield         : 1;
--     unsigned has_bitfield        : 1;
--     unsigned is_jitted           : 1;
--     unsigned is_packed           : 1;
--     unsigned is_unsigned         : 1;
-- };
-- ]]

ffi.cdef[[
  struct _ffi_ctype { /* renaming to reserve ffi namespace, avoid future conflicts (original is 'struct ctype') */
    size_t base_size; /* size of the base type in bytes */

    union {
        /* valid if is_bitfield */
        struct {
            /* size of bitfield in bits */
            unsigned bit_size : 7;
            /* offset within the current byte between 0-63 */
            unsigned bit_offset : 6;
        };
        /* Valid if is_array */
        size_t array_size;
        /* Valid for is_variable_struct or is_variable_array. If
         * variable_size_known (only used for is_variable_struct) then this is
         * the total increment otherwise this is the per element increment.
         */
        size_t variable_increment;
    };
    size_t offset;
    unsigned align_mask          : 4; /* as (align bytes - 1) eg 7 gives 8 byte alignment */
    unsigned pointers            : 2; /* number of dereferences to get to the base type including +1 for arrays */
    unsigned const_mask          : 4; //POINTER_MAX + 1; /* const pointer mask, LSB is current pointer, +1 for the whether the base type is const */
    unsigned type                : 5; /* value given by type enum above */
    unsigned is_reference        : 1;
    unsigned is_array            : 1;
    unsigned is_defined          : 1;
    unsigned is_null             : 1;
    unsigned has_member_name     : 1;
    unsigned calling_convention  : 2;
    unsigned has_var_arg         : 1;
    unsigned is_variable_array   : 1; /* set for variable array types where we don't know the variable size yet */
    unsigned is_variable_struct  : 1;
    unsigned variable_size_known : 1; /* used for variable structs after we know the variable size */
    unsigned is_bitfield         : 1;
    unsigned has_bitfield        : 1;
    unsigned is_jitted           : 1;
    unsigned is_packed           : 1;
    unsigned is_unsigned         : 1;
};
]]

local maybe_bool = function(value)
 if not value or value == 0 then return end
 if value == 1 then return true end
end

local maybe_number = function(value)
  if not value then return end
  if value == 0 then return end
  return tonumber(value)
end

reflect = function(x)
    arrayof = function(ct,pidx)
--     print(pidx, pidx-2)
    if pidx == 1 then
      local ret = {}
--       ret.dim  = ct.fix_mem[pidx-2]
      ret.what = dtypes[ct.type+1]
      ret.size = tonumber(ct.base_size)
      return ret
    end
    local ret = {}
--     ret.dim  = ct.fix_mem[pidx-2]
    ret.what = 'array'
    ret.element_type = arrayof(ct,pidx-1)
    return ret
  end
-- print(x)
  local t = ffi.typeof(x)
  local ct = ffi.cast('struct _ffi_ctype*',t)
  local ret = {}
  if ct.type == 1 then -- void
    ret.what      = dtypes[ct.type+1]
    ret.size      = tonumber(ct.base_size)
    ret.reference = ct.is_reference
    ret.defined   = ct.is_defined
    ret.is_null   = ct.is_null
    return ret
  end
  if ct.is_variable_array == 1 then
--     print('todo')
    return ret
  end
  if ct.is_array == 1 then
    ret.what    = 'array'
    ret.dim     = tonumber(ct.array_size)
    ret.size    = tonumber(ct.base_size)*tonumber(ret.dim)
    ret.pointer = maybe_number(ct.pointers)
    if ret.pointer and ret.pointer > 1 then
--       print(ret.pointer)
      ret.element_type = arrayof(ct,ret.pointer-1)
    else
      ret.element_type = dtypes[ct.type+1]
    end
--     ret.element_type = {
--       size = tonumber(ct.base_size),
--       what = dtypes[ct.type+1]
--     }
--     if ret.pointer and ret.pointer > 1 then
--       ret.element_type.dim = ct.fix_mem[ret.pointer-2]
--     end
    ---
    ret.reference = maybe_bool(ct.is_reference)
    ret.defined   = maybe_bool(ct.is_defined)
    ret.is_null   = maybe_bool(ct.is_null)
    return ret
  end
  ret.what      = dtypes[ct.type+1]
  if ct.type == 16 then -- struct
    local m = ffi.members(t)
    ret.fields = {}
    for i,v in pairs(m) do 
      ret.fields[i] = reflect(v) 
    end
--     print('has_member_name',ct.has_member_name)
--     print('offset',ct.offset)
--     print('variable_increment',ct.variable_increment)
--     print('variable?',ct.is_variable_struct)
--     print('variable_size_known',ct.variable_size_known)
  end
  
  ret.size      = tonumber(ct.base_size)
  ret.reference = maybe_bool(ct.is_reference)
  ret.defined   = maybe_bool(ct.is_defined)
  ret.pointer   = maybe_number(ct.pointers)
  ret.is_null   = maybe_bool(ct.is_null)
  
  return ret
end

-- if not ffi.reflect then ffi.reflect = reflect end

-- testing
-- inspect=require('inspect')
-- -- 
-- ffi.cdef[[typedef struct pos_s {double x,y,z;} pos_t;]]
-- ffi.cdef[[typedef struct pose_s {pos_t pos; double x,y,z,w;} pose_t;]]
-- d=ffi.new('pos_t')
-- meta=reflect(d)
-- print(inspect(meta))
-- 
-- d=ffi.new('pose_t')
-- meta=reflect(d)
-- print(inspect(meta))

return reflect
