require "Symbol"
require "test_utilities"

-- Begin unit tests

local test_Symbol = {}

function test_Symbol.construct()
  local hello = Symbol:new("hello")
  assert("hello" == hello:getValue())
end

function test_Symbol.represents()
  local hello = Symbol:new("hello")
  assert("symbol" == hello:represents())
end

-- End unit tests

function testSymbol()
  runTests("Symbol", test_Symbol)
end

