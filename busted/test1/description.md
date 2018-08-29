# DCONV -- TEST1

This test focus on the creation of DBLX instances for static code generation.
Mostly used for internal development. For released versions of this software, it should always succeed.

## Content (of test1)

  * `asn`: folder with used ASN1 modules definitions:
    * `taste-types.asn`, `taste-extended.asn` and `userdefs-base.asn` are the ones used in ESROCOS project (cf. `types-base`)
    * `mybase.asn` is a reduced version of `base.asn` in `types-base`
    
  * `dproto`: folder with dproto models used in this test
  * `test1.lua`: the test, using busted framework (Lua).
  * this file

## Requirements

The same of the software tool.
Source all your paths (LUA_PATH, etc) properly before to run the test.

## Run the test
```
busted busted/test1/test1.lua
```
## Expected output result
```
●●●●
4 successes / 0 failures / 0 errors / 0 pending : 0.01403 seconds
```