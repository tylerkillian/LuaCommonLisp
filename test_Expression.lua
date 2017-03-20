require "Expression"
require "test_Code"
require "test_utilities"

-- Begin unit tests

local test_Expression = {}

function test_Expression.construct()
  local emptyExpression = Expression:new()
  assert("()" == emptyExpression:toString())
end

function test_Expression.multipleEntries()
  local threeEntries = Expression:new()
  threeEntries:push(FakeCode:new("a"))
  threeEntries:push(FakeCode:new("b"))
  threeEntries:push(FakeCode:new("c"))
  assert("(a b c)" == threeEntries:toString())
end

function test_Expression.represents()
  local emptyExpression = Expression:new()
  assert("expression" == emptyExpression:represents())
end
  
-- End unit tests

function testExpression()
  runTests("Expression", test_Expression)
end

