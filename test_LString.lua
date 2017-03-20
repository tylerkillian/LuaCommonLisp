require "LString"
require "test_utilities"

-- Begin unit tests

local test_LString = {}

function test_LString.construct()
  local hello = LString:new("hello")
  assert("hello" == hello:getValue())
end

function test_LString.represents()
  local hello = LString:new("hello")
  assert("string" == hello:represents())
end

-- End unit tests

function testLString()
  runTests("LString", test_LString)
end

