require "SingleQuoteOperator"
require "test_Code"
require "test_utilities"

-- Begin unit tests

local test_SingleQuoteOperator = {}

function test_SingleQuoteOperator.construct()
  local operator = SingleQuoteOperator:new(FakeCode:new("a"))
  assert("'a" == operator:toString())
end

function test_SingleQuoteOperator.getValue()
  local operator = SingleQuoteOperator:new(FakeCode:new("a"))
  assert("a" == operator:getValue():toString())
end

-- End unit tests

function testSingleQuoteOperator()
  runTests("SingleQuoteOperator", test_SingleQuoteOperator)
end

