require "CodeTypes"

local function LString_construct()
  local lString = LString:new("abc")
  assert("abc" == lString:toString())
end

local function Symbol_construct()
  local symbol = Symbol:new("abc")
  assert("true:abc" == symbol:toString())
end

function testCodeTypes()
  LString_construct()

  Symbol_construct()
end

