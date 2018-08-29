# DCONV -- TEST2 -- DIRECT DPROTO CONVERSION

This test focuses on *direct* dproto conversion,
that is, request of the body function generation that performs
conversions between digital data representation of semantic-compatible dproto models,
without the need to pass through the conversion of a third datatype.

## Content (of test2)

  * `asn`: folder with used ASN1 modules definitions:
    * `taste-types.asn`, `taste-extended.asn` and `userdefs-base.asn` are the ones used in ESROCOS project (cf. `types-base`)
    * `mybase.asn` is a reduced version of `base.asn` in `types-base`
    
  * `dproto`: folder with dproto models used in this test
  * `test2.lua`: the test, using busted framework (Lua).
  * this file

## Requirements

The same of the software tool.
Source all your paths (LUA_PATH, etc) properly before to run the test.

## Run the test
```
busted busted/test2/test2.lua
```
## Expected output result
```
●●●●●●●
7 successes / 0 failures / 0 errors / 0 pending : 0.024875 seconds
```

## Explanation

The first three tests are a set of *simple conversions*, that is,
all dproto models are *as-is*, and not modelled as *composition* of
other existing dproto models.
Different dproto are converted (back and forth), since convertibility is
bidirectional for the cases in example.
It is tested, within the same domain `geometric`:
  * conversion of dprotos having same semantics `semantic`, same coordinate representation `coord`
  and same `algebraic` representation, but different digital data representation `ddr`;
  
  *  conversion of dprotos having same semantics `semantic`, same coordinate representation `coord`
  but different `algebraic` representation *and* different digital data representation `ddr`;
  this conversion exploits the relation _digital_representation_ `dr` of the dproto model;
  
  * conversion of dprotos having same semantics `semantic`, but different coordinate representation `coord`
  is shown/tested in `test3`, for which the convertibility is not necessarily bidirectional.
  
The last four tests are a set of *composite conversions*, that is,
dproto models in subject are modelled as *composition* of other existing dproto models.
For the sake of the test, the two models `SuperPose` and `kul_superpose` are composition of composition,
thus testing the dconv tools also for more complex scenarios.