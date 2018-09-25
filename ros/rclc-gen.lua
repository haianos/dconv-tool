--[[
   Generator of POD C datatypes
   and ffi loader for ROS2/rclc
   messages
   Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
   License: MIT
--]]
--[[
    This implementation exploits the existing
   `rosidl_parser`.
   Integration not directly, but by results
   passed on a file. This is SLOW -- FIXME
--]]
--[[
  TODO: separate registration with loading;
   loading is ffi/dependent
--]]

utils   = require('utils')
json    = require('json')
ffi     = require('ffi')

local primitives_c = {
  float64 = 'double',
  float32 = 'float',
  int32   = 'int',
  uint32  = 'unsigned integer',
  int16   = 'short int',
  uint16  = 'unsigned short int',
  int8    = 'char',
  uint8   = 'unsigned char',
  -- FILL ME ALL
  -- BUILT-IN
  string  = 'rosidl_generator_c__String',
  time    = 'builtin_interfaces__msg__Time'
}

--Buildin primitives_c
ffi.cdef[[
typedef struct rosidl_generator_c__String
{
  char * data;
  size_t size;
  size_t capacity;
} rosidl_generator_c__String;
typedef struct builtin_interfaces__msg__Time
{
  int32_t sec;
  uint32_t nanosec;
} builtin_interfaces__msg__Time;
]]

local loaded = {
  ['builtin_interfaces/Time']=true
}

local function gen_ros_typedef(spec)
  local todo = {}
  local str = 'typedef struct '..spec.name..' {'
  for _,field in ipairs(spec.fields) do
    local typename = ''
    -- Plain struct generation
    if field.typeof.is_primitive then
      typename = primitives_c[field.typeof.type]     
    else
      todo[#todo+1] = {pkg=field.typeof.pkg, name=field.typeof.type}
      typename = field.typeof.pkg..'__msg__'..field.typeof.type
      if field.typeof.is_array then typename = typename .. '__Array' end
    end
    str = str..'\n  '..typename..' '..field.name..';'
  end
  str = str..'\n} '..spec.name..';\n'
  
  -- support for Array version
  local ok, res = utils.preproc([[
typedef struct $(name)__Array {
  $(name)* data;
  size_t size;
  size_t capacity;
} $(name)__Array;
]],{table=table,name=spec.name})
  if not ok then error(res) end
  str = str..res
  return str, todo
end

local function register_and_load(pkg,name)
  if not name then -- Assumption: pkg/name as first argument, help Enea's poor memory
    args = utils.split(pkg,'%/')
    pkg = args[1]
    name = args[2]
  end
  -- get path
  os.execute('rosidl-parser-json path '..pkg..' '..name)
  fd = io.open('/tmp/ros2msg_path','r')
  path = fd:read('*all')
  fd:close()
  
  -- get def
  os.execute('rosidl-parser-json spec '..pkg..' '..path)
  fd = io.open('/tmp/ros2msg_spec','r')
  spec = json.decode(fd:read('*all'))
  fd:close()
  
  local def, todo = gen_ros_typedef(spec)
  for _,v in ipairs(todo) do
    if not loaded[v.pkg..'/'..v.name] then
      register_and_load(v.pkg,v.name)
    end
  end
  if not loaded[pkg..'/'..name] then
    ffi.cdef(def)
    loaded[pkg..'/'..name] = true
  end
end

local function create_instance(pkgname,pointer)
  local _p = ''
  if pointer then _p = '*' end
  if not loaded[pkgname] then
    return false, 'type unknown'
  end
  
  local sep = utils.split(pkgname,'%/')
  local mangled_name = sep[1]..'__msg__'..sep[2]
  return ffi.new(mangled_name.._p)
end

local function gen_typename(pkg,name)
  if not name then -- Assumption: pkg/name as first argument, help Enea's poor memory
    args = utils.split(pkg,'%/')
    pkg  = args[1]
    name = args[2]
  end
  return pkg..'__msg__'..name
end

local M = {}

M.register_and_load  = register_and_load
M.create_instance    = create_instance
M.gen_typename       = gen_typename
    
return M