require "CodeTypes"

local function LString_construct()
  local lString = LString:new("abc")
  assert("abc" == lString:getValue())
end

function testCodeTypes()
  LString_construct()
end

