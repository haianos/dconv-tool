--[[
    Test `test2` on `dconv` tool, a model-based, semantic complete
   datatype automatic generation.
   
   Test created using (Lua) Busted Framework.
   
   This test does sanity checks for the basic output of the tool.
   Full description in `description.md`
   
   Main Author: Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
--]]

require 'busted.runner'()

--  Load Models subject to test
dprotofile =  'busted/test2/dproto/test2.dproto'
asn_files = {
  'busted/test2/asn/taste-types.asn',
  'busted/test2/asn/taste-extended.asn',
  'busted/test2/asn/userdefs-base.asn',
  'busted/test2/asn/mybase.asn'
}

--[[ Load all necessary stuff --]]
utils      = require('utils')
ansicolors = require('ansicolors')

dconv  = require('dproto-conv-gen')

ddr_models = {asn_files = asn_files}
dconv.init_runtime(dprotofile,ddr_models)

-- Aliases
SDBLX = dconv.SDBLX

describe('simple conversions test', function()
  it('convert ASN1-Eigen position',function()
    local convlist = {
      {'Base_Position', 'kul_position'},
      {'Base_Position', 'kul_position2'},
    }
    c   = {}
    err = {}
    assert.has_no.errors(function()
      for i=1,#convlist do
        c[i], err[i] = dconv.convert(convlist[i][1],convlist[i][2])
      end
    end)
    
    for i,v in pairs(c) do
      if not v then print(err[i]) end
    end
    
    for i,v in pairs(c) do
      local str = dconv.CaptureOutput()
      v(str,'lhs.','rhs')
      assert.equal([[rhs(0,0) = lhs.data.arr[0];
rhs(1,0) = lhs.data.arr[1];
rhs(2,0) = lhs.data.arr[2];
]],tostring(str))
    end
  end)
  
  it('convert ASN1-Eigen position (reversed)',function()
      local convlist = {
      {'kul_position', 'Base_Position'},
      {'kul_position2', 'Base_Position'},
    }
    c   = {}
    err = {}
    assert.has_no.errors(function()
      for i=1,#convlist do
        c[i], err[i] = dconv.convert(convlist[i][1],convlist[i][2])
      end
    end)
    
    for i,v in pairs(c) do
      if not v then print(err[i]) end
    end
    
    for i,v in pairs(c) do
      local str = dconv.CaptureOutput()
      v(str,'lhs','rhs.')
      assert.equal([[rhs.data.arr[0] = lhs(0,0);
rhs.data.arr[1] = lhs(1,0);
rhs.data.arr[2] = lhs(2,0);
]],tostring(str))
    end
  end)
  
  it('convert ASN1-Eigen quaternion (reversed)',function()
        local convlist = {
      {'kul_quaternion', 'Base_Quaternion'},
      {'kul_quaternion2', 'Base_Quaternion'},
    }
    c   = {}
    err = {}
    assert.has_no.errors(function()
      for i=1,#convlist do
        c[i], err[i] = dconv.convert(convlist[i][1],convlist[i][2])
      end
    end)
    
    for i,v in pairs(c) do
      if not v then print(err[i]) end
    end
    
    for i,v in pairs(c) do
      local str = dconv.CaptureOutput()
      v(str,'lhs','rhs.')
      assert.equal([[rhs.im.arr[0] = lhs(0);
rhs.im.arr[1] = lhs(1);
rhs.im.arr[2] = lhs(2);
rhs.re = lhs(3);
]],tostring(str))
    end
  end)

end)

describe('composite conversion test', function()
  it('convert Base_Pose to kul_pose2',function()
    assert.has.no_errors(function()
      gen, err = dconv.convert('Base_Pose','kul_pose2')
      if not gen then return error(err); end
      str = dconv.CaptureOutput()
      gen(str,'rhs.','lhs.')
    end)
    assert.equal([[
lhs.orientation(0) = rhs.orientation.im.arr[0];
lhs.orientation(1) = rhs.orientation.im.arr[1];
lhs.orientation(2) = rhs.orientation.im.arr[2];
lhs.orientation(3) = rhs.orientation.re;
lhs.position(0,0) = rhs.position.data.arr[0];
lhs.position(1,0) = rhs.position.data.arr[1];
lhs.position(2,0) = rhs.position.data.arr[2];
]],tostring(str))
  end)
  
  it('convert kul_pose2 to Base_Pose (inverse)',function()
      assert.has.no_errors(function()
      gen, err = dconv.convert('kul_pose2','Base_Pose')
      if not gen then return error(err); end
      str = dconv.CaptureOutput()
      gen(str,'rhs.','lhs.')
    end)
    assert.equal([[
lhs.orientation.im.arr[0] = rhs.orientation(0);
lhs.orientation.im.arr[1] = rhs.orientation(1);
lhs.orientation.im.arr[2] = rhs.orientation(2);
lhs.orientation.re = rhs.orientation(3);
lhs.position.data.arr[0] = rhs.position(0,0);
lhs.position.data.arr[1] = rhs.position(1,0);
lhs.position.data.arr[2] = rhs.position(2,0);
]],tostring(str))
  end)
  
  it('convert SuperPose to kul_superpose', function()
      assert.has.no_errors(function()
      gen, err = dconv.convert('SuperPose','kul_superpose')
      if not gen then return error(err); end
      str = dconv.CaptureOutput()
      gen(str,'rhs.','lhs.')
    end)
    assert.equal([[
lhs.o1.orientation(0) = rhs.bar.orientation.im.arr[0];
lhs.o1.orientation(1) = rhs.bar.orientation.im.arr[1];
lhs.o1.orientation(2) = rhs.bar.orientation.im.arr[2];
lhs.o1.orientation(3) = rhs.bar.orientation.re;
lhs.o1.position(0,0) = rhs.bar.position.data.arr[0];
lhs.o1.position(1,0) = rhs.bar.position.data.arr[1];
lhs.o1.position(2,0) = rhs.bar.position.data.arr[2];
lhs.o2.orientation(0) = rhs.foo.orientation.im.arr[0];
lhs.o2.orientation(1) = rhs.foo.orientation.im.arr[1];
lhs.o2.orientation(2) = rhs.foo.orientation.im.arr[2];
lhs.o2.orientation(3) = rhs.foo.orientation.re;
lhs.o2.position(0,0) = rhs.foo.position.data.arr[0];
lhs.o2.position(1,0) = rhs.foo.position.data.arr[1];
lhs.o2.position(2,0) = rhs.foo.position.data.arr[2];
]],tostring(str))
  end)
  
  it('convert kul_superpose to SuperPose (inverse)', function()
      assert.has.no_errors(function()
        gen, err = dconv.convert('kul_superpose','SuperPose')
        if not gen then return error(err); end
        str = dconv.CaptureOutput()
        gen(str,'rhs.','lhs.')
      end)
      assert.equal([[
lhs.bar.orientation.im.arr[0] = rhs.o1.orientation(0);
lhs.bar.orientation.im.arr[1] = rhs.o1.orientation(1);
lhs.bar.orientation.im.arr[2] = rhs.o1.orientation(2);
lhs.bar.orientation.re = rhs.o1.orientation(3);
lhs.bar.position.data.arr[0] = rhs.o1.position(0,0);
lhs.bar.position.data.arr[1] = rhs.o1.position(1,0);
lhs.bar.position.data.arr[2] = rhs.o1.position(2,0);
lhs.foo.orientation.im.arr[0] = rhs.o2.orientation(0);
lhs.foo.orientation.im.arr[1] = rhs.o2.orientation(1);
lhs.foo.orientation.im.arr[2] = rhs.o2.orientation(2);
lhs.foo.orientation.re = rhs.o2.orientation(3);
lhs.foo.position.data.arr[0] = rhs.o2.position(0,0);
lhs.foo.position.data.arr[1] = rhs.o2.position(1,0);
lhs.foo.position.data.arr[2] = rhs.o2.position(2,0);
]],tostring(str))
  end)  
end)
