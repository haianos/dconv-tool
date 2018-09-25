--[[
    Test `test4` on `dconv` tool, a model-based, semantic complete
   datatype automatic generation.
   
   Test created using (Lua) Busted Framework.
   
   This test does sanity checks for the basic output of the tool.
   Testing DDBLX and convert_ddblx --> DYNAMIC CONVERSION
   Using ROS2/rclc
   
   Main Author: Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
--]]

require 'busted.runner'()

--  Load Models subject to test
dprotofile =  'busted/test4/dproto/ros-example.dproto'

--[[ Load all necessary stuff --]]
dconv  = require('dproto-conv-gen')


ddr_models = {ros2=true}
dconv.init_runtime(dprotofile,ddr_models)

-- Aliases
DDBLX = dconv.DDBLX

describe('creating DDBLX', function()
  it('plain c99 test',function()
    assert.has.no_errors(function()
      p = DDBLX('plain_position')
      o = DDBLX('plain_rotation')
      pose = DDBLX('plain_pose')
      p.x = 1
      p.y = 2
      p.z = 3
      o[{0,0}] = 1.0; o[{1,1}] = 1.0; o[{2,2}] = 1.0
      pose.position.x = 111
      pose.position.y = 222
      pose.position.z = 66.6
    end)
    assert.equal(1,p.x)
    assert.equal(2,p.y)
    assert.equal(3,p.z)
    assert.equal(1,o(0,0))
    assert.equal(1,o(1,1))
    assert.equal(1,o(2,2))
    assert.equal(111,pose.position.x)
    assert.equal(222,pose.position.y)
    assert.equal(66.6,pose.position.z)
  end)
  
  it('ROS2/rclc test', function()
    assert.has.no_error(function()
      p = DDBLX('ros_position')
      o = DDBLX('ros_quaternion')
      pose = DDBLX('ros_pose')
      p[0] = 24
      p[1] = -24
      p[2] = 5
    end)
    assert.equal(24,p[0])
    assert.equal(-24,p[1])
    assert.equal(5,p[2])
  end)
end)
  
describe('conversions tests', function()
  it('conversion plain_position->ros_position',function()
    assert.has.no_error(function()
      pp = DDBLX('plain_position')
      rp = DDBLX('ros_position')
      pp.x = 1
      pp.y = 2
      pp.z = 3
      dconv.convert(pp,rp)
    end)
    assert.equal(1,rp[0])
    assert.equal(2,rp[1])
    assert.equal(3,rp[2])
  end)
  
  it('conversion ros_quaternion->plain_rotation (uses conversion)',function()
    assert.has.no_error(function()
      op = DDBLX('plain_rotation')
      ro = DDBLX('ros_quaternion')
      ro.w = 1.0
      dconv.convert(ro,op)
    end)
    assert.equal(1.0,op(0,0))
    assert.equal(1.0,op(1,1))
    assert.equal(1.0,op(2,2))
    assert.equal(0.0,op(2,0))
    assert.equal(0.0,op(0,1))
    assert.equal(0.0,op(2,1))
  end)
  
  it('conversion ros_pose->plain_pose (uses conversion)',function()
  assert.has.no_error(function()
      ppose = DDBLX('plain_pose')
      rpose = DDBLX('ros_pose')
      rpose.position.x = 111
      rpose.position.z = 666
      rpose.orientation.w = 1
      dconv.convert(rpose,ppose)
    end)
    assert.equal(111,ppose.position.x)
    assert.equal(0,ppose.position.y)
    assert.equal(666,ppose.position.z)
    assert.equal(1.0,ppose.orientation(0,0))
    assert.equal(1.0,ppose.orientation(1,1))
    assert.equal(1.0,ppose.orientation(2,2))
    assert.equal(0.0,ppose.orientation(2,0))
    assert.equal(0.0,ppose.orientation(0,1))
    assert.equal(0.0,ppose.orientation(2,1))
  end)

end)