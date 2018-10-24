--[[
    Test `test6` on `dconv` tool, a model-based, semantic complete
   datatype automatic generation.
   
   Test created using (Lua) Busted Framework.
   
   This test does sanity checks for the basic output of the tool.
   Full description in `description.md`
   
   Main Author: Enea Scioni, <enea.scioni@kuleuven.be>
   2018, KU Leuven, Belgium
--]]

require 'busted.runner'()

--  Load Models subject to test
dprotofile =  'busted/test6/dproto/test6.dproto'
asn_files = {
  'busted/test6/asn/taste-types.asn',
  'busted/test6/asn/taste-extended.asn',
  'busted/test6/asn/userdefs-base.asn',
  'busted/test6/asn/mybase.asn'
}

--[[ Load all necessary stuff --]]
utils      = require('utils')
ansicolors = require('ansicolors')

dconv  = require('dproto-conv-gen')

ddr_models = {asn_files = asn_files}
dconv.init_runtime(dprotofile,ddr_models)

-- Aliases
SDBLX = dconv.SDBLX

-- inspect = require('inspect')
-- -- k = dconv.SDBLX('kul_rotation')
-- -- o = dconv.SDBLX('Base_Orientation')


describe('Conversions employed in ilk-compiler',function()
  it('From Base_Position to kul_position',function()
    c, err = dconv.convert('Base_Position','kul_position')
    if not c then print(err) end
    str = dconv.CaptureOutput()
    c(str,'lhs.','rhs')
    assert.equal([[rhs(0,0) = lhs.data.arr[0];
rhs(1,0) = lhs.data.arr[1];
rhs(2,0) = lhs.data.arr[2];
]],tostring(str))
  end)
  
  it('From Base_Rotation to kul_rotation',function()
    c, err = dconv.convert('Base_Rotation','kul_rotation')
    if not c then print(err) end
    str = dconv.CaptureOutput()
    c(str,'lhs','rhs')
    assert.equal([[rhs(0,0) = lhs[0];
rhs(0,1) = lhs[1];
rhs(0,2) = lhs[2];
rhs(1,0) = lhs[3];
rhs(1,1) = lhs[4];
rhs(1,2) = lhs[5];
rhs(2,0) = lhs[6];
rhs(2,1) = lhs[7];
rhs(2,2) = lhs[8];
]],tostring(str))
  end)
  
  it('From Base_Quaternion to kul_rotation',function()
    c, err = dconv.convert('Base_Quaternion','kul_rotation')
    if not c then print(err) end
    str = dconv.CaptureOutput()
    c(str,'lhs','rhs')
    assert.equal([[quat2rot(lhs,rhs);
]],tostring(str))
  end)
  
  it('From Base_Pose to kul_pose',function()
    c, err = dconv.convert('Base_Pose','kul_pose')
    if not c then print(err) end
    str = dconv.CaptureOutput()
    c(str,'lhs','rhs')
    assert.equal([[quat2rot(lhsorientation.,rhsorientation);
rhsposition(0,0) = lhsposition.data.arr[0];
rhsposition(1,0) = lhsposition.data.arr[1];
rhsposition(2,0) = lhsposition.data.arr[2];
]],tostring(str))
  end)

end)
