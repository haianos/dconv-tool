# DCONV -- TEST3

This test focus on indirect dproto conversion, by means of alias definitions and conversion
functions available.

## Content (of test3)

  * `asn`: folder with used ASN1 modules definitions:
    * `taste-types.asn`, `taste-extended.asn` and `userdefs-base.asn` are the ones used in ESROCOS project (cf. `types-base`)
    * `mybase.asn` is a reduced version of `base.asn` in `types-base`
    
  * `dproto`: folder with dproto models used in this test
  * `test3.lua`: the test, using busted framework (Lua).
  * this file

## Requirements

The same of the software tool.
Source all your paths (LUA_PATH, etc) properly before to run the test.

## Run the test
```
busted busted/test3/test3.lua
```
## Expected output result
```
●●●
3 successes / 0 failures / 0 errors / 0 pending : 0.012488 seconds
```

## Explanation

This test validate the indirect conversion capability of dconv tool.
Indirect conversion means that it is possible to realise datatype conversion
semantic-compatible dproto models, but with completely different coordinate representation,
by means of an explicit declaration of a conversion function (keyword `conversion`).
The generated code must be compiled together with the implementation of that function,
that should be shipped as companion source file.
The `conversion` facility can also be used to model (and delegate) the conversion
between two dproto models to a user-defined implementation, avoiding any possible Limitation
related to the concrete reference implementation, and to extend the dconv tool to corner cases 
not fully expressed as dproto model, for the time being, in the domain of reference.

In the example, three conversions are tested:
  1. From `Base_Quaternion` to `ROT`
  2. From `ROT` to `kul_rotation`
  3. from `Base_Quaternion` to `kul_rotation` (the sum of the previous two).
The latest case is an *indirect* conversion, since the conversion is possible only
by means of a third type (`ROT`, in this case). This requires the generation of
temporal variables, which must be allocated in a proper way.

## Current Limitations

Currently, the "path" solver implemented in this reference uses an heuristic that
does *not* guarantee that the solution is the shortest one.
This may be improved in future releases.