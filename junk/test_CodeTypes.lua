require "CodeTypes"

local function LString_construct()
  local lString = LString:new("abc")
  assert("abc" == lString:toString())
end

local function Symbol_construct()
  local symbol = Symbol:new("abc")
  assert("true:abc" == symbol:toString())
end

local function Expression_construct()
  local expression = Expression:new()
  assert("()" == expression:toString())
end

function testCodeTypes()
  LString_construct()

  Symbol_construct()

  Expression_construct()
end

