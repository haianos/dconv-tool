package = "dconv-tool"
version = "0.1-1"
source = {
  url    = "git://github.com/haianos/dconv-tool",
  branch = "master"
}
description = {
  summary  = "dconv-tool",
  detailed = [[
    An automatic datatypes conversion tools
    based on dproto models, with support for ASN1 and more.
  ]],
  license = "MIT"
}
dependencies = {
  "lua > 5.1",
  "lua-common-tools",
  "lpeg",
  "lpeglabel",
  "luautf8",
  "busted",  --for testing only
  "luafilesystem"      --command tool only
}
build = {
  type = "builtin",
  modules = {
    asn1runtime          = "src/asn1runtime.lua",
    location             = "src/location.lua",
    ['dproto-conv-gen']  = "src/dproto-conv-gen.lua",
    ['parser/asn1']      = "src/parser/asn1.lua",
    ['parser/eigen']     = "src/parser/eigen.lua",
    ['parser/dproto']    = "src/parser/dproto.lua"
  },
  install = {
    bin = {
      dconv = 'dconv'
    }
  }
}