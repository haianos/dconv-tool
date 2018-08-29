# dconv-tool

Automatic datatype conversion generation tool based on dproto models.

## What it does

`dconv` is a tool for static `C` and `C++` code generation starting from
enriched datatypes models called `dproto`s.
A `dproto` model includes semantic information about a data structure,
independently from its *digital data representation`, that is, the model
used to express the datatype (usually an IDL to describe Communication Objects).

`dconv` exploits `dproto` models to generate C-code automatically to convert from one
datatype to another, if they are semantically equivalent, or if enough knowledge
has been placed into the models to perform so.

Please see folder `busted` and `examples` for further hints.

## Current supported IDLs:

  * `ASN1`
  * `c99`, c99 datatypes definitions (only partial for the time being);
  * `Eigen` datatypes 
  
## Coming up support:

  * `ROS-IDL`
  * `SmartSoft Communication Object`

## Prerequisites and installation

A `Lua` interpreter (tested on `Lua5.2`) and `luarocks` tool to ease 
process.

1. Install `lua5.2` and `lua5.2-dev` (**NOTE**: getting luarocks using
   `apt-get install` results in an incompatible installation on Ubuntu 16.04 as
   of 14 Jun 2018)

     ```sudo apt-get install lua5.2 liblua5.2-dev```

2. Download and install luarocks
    - download from `https://luarocks.org/` or using `wget`:

        ```wget http://luarocks.github.io/luarocks/releases/luarocks-2.4.4.tar.gz```
    - unpack the source archive:

        `tar zxvf luarocks-2.4.4.tar.gz`
    - build it:

        ```
        cd luarocks-2.4.4/
        ./configure --lua-version=5.2
        make build
        sudo make install
        cd ..
        ```

3. Download the required rockspecs with `curl` or `wget`:

```
wget https://people.mech.kuleuven.be/~u0072295/software/luarocks/lua-common-tools-0.1-2.rockspec
wget https://raw.githubusercontent.com/haianos/dconv-tool/master/dconv-tool-0.1-1.rockspec
```

4. Installing the:

    ```
    luarocks install --local lua-common-tools-0.1-2.rockspec
    luarocks install --local dconv-tool-0.1-1.rockspec
    ```

    
## Usage

### Usage as a library

The `dconv-tool` can be used directly as a `Lua` module:
```
dconv  = require('dproto-conv-gen')
ret, err =  dconv.init_runtime(dprotofile,asn_files)
if not ret then error(err); end
```
where `dprotofile` is a `.dproto` file model, and `asn_files` are a list (Lua-table)
of the files containing ASN1 module definitions.

`dconv` provides:
  * A common API based on the algebraic information of the datatype to link to
  the relative C-code generation;
  * `convert`, a conversion function that returns a function to generate
  statically C body functions for datatype conversion.

#### Example

The following
```
s = SDBLX('Base_Quaternion')
```
generates a code generation object for the `dproto` definition with `id` "Base_Quaternion", which
has been defined as
```
dproto Base_Quaternion :: geometric {
  semantic  = Orientation
  coord     = quaternion
  algebraic = quat_named
  ddr = {
    mid  = "Base-Types.Wrappers-Quaterniond"
    mmid = "ASN1"
  }
  dr = {re=w, im.0=x, im.1=y, im.2=z}
}
```
The model above enriches the *digital data representation* (`ddr`) used in the implementation;
in this case, a `Base-Types.Wrappers-Quaterniond` defined by means of ASN1 IDL.
The semantic of the dproto is `Orientation`, that is, the associate datatype represent an orientation in space,
within the domain of `geometric`.
The *coordinate representation* is of type `quaternion`, and its *algebraic* representation is `quat_named`
that has been defined as:
```
algebraic quat_named :: Scalar{x,y,z,w}
```
This means that, semantically speaking, the right accessor to this model is by referring to it's algebraic
representation: in this case, a list of scalars.
Therefore, we can access to the concrete accessor of the datatype by means of the values `x`,`y`,`z` and `w`.
For example, `s.w` refers to the concrete datatype `data.re`, while `s.x` refers to `s.im.arr[0]`, which are
the generated datatypes from the ASN1 description of `Base-Types.Wrappers-Quaterniond` as reported below:

*ASN1 Model*
```
Wrappers-Quaterniond ::= SEQUENCE
{
    im  SEQUENCE(SIZE(1 .. 3)) OF T-Double,
    re  T-Double
}
```

*Generated datatypes with ASN1 compiler*
```
typedef struct {    int nCount; 
    
    T_Double arr[3];
} Wrappers_Quaterniond_im;

typedef struct {
    Wrappers_Quaterniond_im im;
    T_Double re;
} Wrappers_Quaterniond;
```

Providing another `dproto` definition of **compatible semantics**, it is possible to use
the `convert` function to generate the body of the function
```
g, e = dconv.convert('Base_Quaternion','kul_quaternion')
```
where `e` is returning error message (if any), while `g` is a function that generates the body 
text and can be used in your application. For example:
```
g(nil,'lhs','rhs')
```
generates (print to `stdout`):
```
rhs(0) = lhs.im.arr[0];
rhs(1) = lhs.im.arr[1];
rhs(2) = lhs.im.arr[2];
rhs(3) = lhs.re;
```
Replacing `nil` with a file descriptor will write the output above there.
For commodity, it is possible to buffer the output with `dconv.CaptureOutput` as follows:
```
local out = dconv.CaptureOutput()
v(out,'lhs.','rhs')
```
`out` can be turn to a string, streaming out its content.

### Usage as Command-line tool

Coming Soon.

## Run the tests

All tests uses the Busted Framework and they are self-contained in the `busted` folder of this repository.
To try them out, download this repository and run them as:
```
busted busted/testX/testX.lua
```
where `X` is the test number to execute.

Description of the tests can be found in `description.md` in each relative test subfolder.

### DPROTO MODEL

## DPROTO

**TODO**, describe `dproto` model

  * `domain`
  * `semantic`
  * `coord`
  * `algebraic`
  * `ddr`
  * `dr`
  

## Author

  * Enea Scioni, <enea.scioni@kuleuven.be>
  * 2018, KU Leuven, Belgium

## License

Released under MIT license.

## Acknowledgement

 * peraspera/ESROCOS project
 * H2020 RobMoSys
