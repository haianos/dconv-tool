--[[
    Test `test1` on `dconv` tool, a model-based, semantic complete
   datatype automatic generation.
   
   Test created using (Lua) Busted Framework.
   
   This test does sanity checks for the basic output of the tool.
   Full description in `description.md`
   
   Main Author: Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
--]]

require 'busted.runner'()

--  Load Models subject to test
dprotofile =  'busted/test1/dproto/test1.dproto'
asn_files = {
  'busted/test1/asn/taste-types.asn',
  'busted/test1/asn/taste-extended.asn',
  'busted/test1/asn/userdefs-base.asn',
  'busted/test1/asn/mybase.asn'
}

--[[ Load all necessary stuff --]]
utils      = require('utils')
ansicolors = require('ansicolors')

dconv  = require('dproto-conv-gen')

ddr_models = {}
ddr_models.asn_files = asn_files

dconv.init_runtime(dprotofile,ddr_models)

-- Aliases
SDBLX = dconv.SDBLX

describe('dblx static gen creation', function()
  v  = SDBLX('kul_position2')    -- vector, API v[0]
  s  = SDBLX('Base_Quaternion')  -- scalar, API s.x
  m  = SDBLX('kul_rotation')     -- matrix, API m(0,0)
  s2 = SDBLX('kul_rotation2')    -- scalar, API s2.Xx
  
  it('create ASN1 types', function()
    assert.has_no.errors(function()
        p  = SDBLX('Base_Position')
        q  = SDBLX('Base_Quaternion')
    end)
    
    -- Base_Position has scalar API
    assert.equal(p.x,'data.arr[0]')
    assert.equal(p.y,'data.arr[1]')
    assert.equal(p.z,'data.arr[2]')
    -- Base_Quaternion has scalar API
    assert.equal(q.x,'im.arr[0]')
    assert.equal(q.y,'im.arr[1]')
    assert.equal(q.z,'im.arr[2]')
    assert.equal(q.w,'re')
  end)
  
  it('create Eigen types', function()
    assert.has_no.errors(function()
      p1   = SDBLX('kul_position')
      p2   = SDBLX('kul_position2')
      r1   = SDBLX('kul_rotation')
      r2   = SDBLX('kul_rotation2')
      pose = SDBLX('kul_pose')
    end)
    
    -- kul_position has scalar API
    assert.equal(p1.x,'(0,0)')
    assert.equal(p1.y,'(1,0)')
    assert.equal(p1.z,'(2,0)')
    -- kul_position2 has vector API
    assert.equal(p2[0],'(0,0)')
    assert.equal(p2[1],'(1,0)')
    assert.equal(p2[2],'(2,0)')
    -- kul_rotation has matrix api
    assert.equal(r1(0,0),'(0,0)')
    assert.equal(r1(1,0),'(1,0)')
    assert.equal(r1(2,0),'(2,0)')
    assert.equal(r1(1,0),'(1,0)')
    assert.equal(r1(1,1),'(1,1)')
    assert.equal(r1(1,2),'(1,2)')
    assert.equal(r1(2,0),'(2,0)')
    assert.equal(r1(2,1),'(2,1)')
    assert.equal(r1(2,2),'(2,2)')
    -- kul_rotation2 has scalar API
    assert.equal(r2.Xx,'(0,0)')
    assert.equal(r2.Yx,'(1,0)')
    assert.equal(r2.Zx,'(2,0)')
    assert.equal(r2.Xy,'(0,1)')
    assert.equal(r2.Yy,'(1,1)')
    assert.equal(r2.Zy,'(2,1)')
    assert.equal(r2.Xz,'(0,2)')
    assert.equal(r2.Yz,'(1,2)')
    assert.equal(r2.Zz,'(2,2)')
  end)
  
  it('create c99 types', function()
    assert.has_no.errors(function()
        r  = SDBLX('ROT')
        q  = SDBLX('QUAT')
    end)
    
    -- QUAT has scalar API
    assert.equal(q.x,'[0]')
    assert.equal(q.y,'[1]')
    assert.equal(q.z,'[2]')
    assert.equal(q.w,'[3]')
    -- rot has Matrix API
    assert.equal(r(0,0),'[0]')
    assert.equal(r(0,1),'[1]')
    assert.equal(r(0,2),'[2]')
    assert.equal(r(1,0),'[3]')
    assert.equal(r(1,1),'[4]')
    assert.equal(r(1,2),'[5]')
    assert.equal(r(2,0),'[6]')
    assert.equal(r(2,1),'[7]')
    assert.equal(r(2,2),'[8]')
  end)
  
  it('create composite dproto', function()
    assert.has_no.errors(function()
      pose  = SDBLX('Base_Pose')
      spose = SDBLX('SuperPose')
      kpose = SDBLX('kul_pose2') 
      kspose= SDBLX('kul_superpose')
    end)
    assert.equal('position.data.arr[0]',pose.position.x)
    assert.equal('position.data.arr[1]',pose.position.y)
    assert.equal('position.data.arr[2]',pose.position.z)
    assert.equal('orientation.im.arr[0]',pose.orientation.x)
    assert.equal('orientation.im.arr[1]',pose.orientation.y)
    assert.equal('orientation.im.arr[2]',pose.orientation.z)
    assert.equal('orientation.re',pose.orientation.w)
    
    assert.equal('bar.position.data.arr[0]',spose.bar.position.x)
    assert.equal('bar.position.data.arr[1]',spose.bar.position.y)
    assert.equal('bar.position.data.arr[2]',spose.bar.position.z)
    assert.equal('bar.orientation.im.arr[0]',spose.bar.orientation.x)
    assert.equal('bar.orientation.im.arr[1]',spose.bar.orientation.y)
    assert.equal('bar.orientation.im.arr[2]',spose.bar.orientation.z)
    assert.equal('bar.orientation.re',spose.bar.orientation.w)

    assert.equal('foo.position.data.arr[0]',spose.foo.position.x)
    assert.equal('foo.position.data.arr[1]',spose.foo.position.y)
    assert.equal('foo.position.data.arr[2]',spose.foo.position.z)
    assert.equal('foo.orientation.im.arr[0]',spose.foo.orientation.x)
    assert.equal('foo.orientation.im.arr[1]',spose.foo.orientation.y)
    assert.equal('foo.orientation.im.arr[2]',spose.foo.orientation.z)
    assert.equal('foo.orientation.re',spose.foo.orientation.w)

    assert.equal('position(0,0)',kpose.position.x)
    assert.equal('position(1,0)',kpose.position.y)
    assert.equal('position(2,0)',kpose.position.z)
    assert.equal('orientation(3)',kpose.orientation.w)
    assert.equal('orientation(0)',kpose.orientation.x)
    assert.equal('orientation(1)',kpose.orientation.y)
    assert.equal('orientation(2)',kpose.orientation.z)
    
    assert.equal('o1.position(2,0)',kspose.o1.position.z)
    assert.equal('o2.position(1,0)', kspose.o2.position.y)
    assert.equal('o2.orientation(1)',kspose.o2.orientation.y)    
  end)
end)