--[[
    Test `test3` on `dconv` tool, a model-based, semantic complete
   datatype automatic generation.
   
   Test created using (Lua) Busted Framework.
   
   This test does sanity checks for the basic output of the tool.
   Full description in `description.md`
   
   Main Author: Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
--]]

require 'busted.runner'()

--  Load Models subject to test
dprotofile =  'busted/test3/dproto/test3.dproto'
asn_files = {
  'busted/test3/asn/taste-types.asn',
  'busted/test3/asn/taste-extended.asn',
  'busted/test3/asn/userdefs-base.asn',
  'busted/test3/asn/mybase.asn'
}

--[[ Load all necessary stuff --]]
utils      = require('utils')
ansicolors = require('ansicolors')

dconv  = require('dproto-conv-gen')

ret, err =  dconv.init_runtime(dprotofile,asn_files)

if not ret then error(err); end

-- Aliases
SDBLX = dconv.SDBLX

describe('indirect conversions test', function()
  it('convert Base_Quaternion and ROT',function()
    assert.has_no.errors(function()
      c, e = dconv.convert('Base_Quaternion','ROT')
      if not c then error(e) end
      str = dconv.CaptureOutput()
      c(str,'lhs','rhs')
    end)
    assert.equal(tostring(str),[[double QUAT_tmp_2[4];

QUAT_tmp_2[0] = rhsim.arr[0];
QUAT_tmp_2[1] = rhsim.arr[1];
QUAT_tmp_2[2] = rhsim.arr[2];
QUAT_tmp_2[3] = rhsre;

quat2rot(QUAT_tmp_2,lhs);

]])
  end)
  
  
  it('convert ROT and kul_rotation',function()
    assert.has_no.errors(function()
      c, e = dconv.convert('ROT','kul_rotation')
      str = dconv.CaptureOutput()
      if not c then error(e) end
      c(str,'lhs','rhs')
    end)
    assert.equal(tostring(str),[[rhs(0,0) = lhs[0];
rhs(0,1) = lhs[1];
rhs(0,2) = lhs[2];
rhs(1,0) = lhs[3];
rhs(1,1) = lhs[4];
rhs(1,2) = lhs[5];
rhs(2,0) = lhs[6];
rhs(2,1) = lhs[7];
rhs(2,2) = lhs[8];
]])
  end)

  
  it('convert Base_Quaternion(ASN1) and kul_rotation',function()
    assert.has_no.errors(function()
      c, e = dconv.convert('Base_Quaternion','kul_rotation')
      str = dconv.CaptureOutput()
      if not c then error(e); end
      c(str,'lhs','rhs.')
    end)
    assert.equal(tostring(str),[[double QUAT_tmp_2[4];
double ROT_tmp_3[9];

QUAT_tmp_2[0] = rhs.im.arr[0];
QUAT_tmp_2[1] = rhs.im.arr[1];
QUAT_tmp_2[2] = rhs.im.arr[2];
QUAT_tmp_2[3] = rhs.re;

quat2rot(QUAT_tmp_2,ROT_tmp_3);

lhs(0,0) = ROT_tmp_3[0];
lhs(0,1) = ROT_tmp_3[1];
lhs(0,2) = ROT_tmp_3[2];
lhs(1,0) = ROT_tmp_3[3];
lhs(1,1) = ROT_tmp_3[4];
lhs(1,2) = ROT_tmp_3[5];
lhs(2,0) = ROT_tmp_3[6];
lhs(2,1) = ROT_tmp_3[7];
lhs(2,2) = ROT_tmp_3[8];

]])
  end)
  
end)