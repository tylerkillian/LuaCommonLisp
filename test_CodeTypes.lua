require "CodeTypes"

function LString_construct()
  local lString = LString:new("abc")
  assert("abc" == lString:getValue())
end

LString_construct()

