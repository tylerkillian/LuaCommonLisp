require "Expression"
require "test_Code"
require "test_utilities"

FakeContext = {}

function FakeContext:new()
  local context = {
    numArgumentsPassedIn = 0,
  }
  setmetatable(context, self)
  self.__index = self

  return context
end

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

function test_Expression.lengthOfDefaultExpression()
  local emptyExpression = Expression:new()
  assert(0 == emptyExpression:getLength())
end

function test_Expression.lengthOf3()
  local threeEntries = Expression:new()
  threeEntries:push(FakeCode:new("a"))
  threeEntries:push(FakeCode:new("b"))
  threeEntries:push(FakeCode:new("c"))
  assert(3 == threeEntries:getLength())
end

function test_Expression.represents()
  local emptyExpression = Expression:new()
  assert("expression" == emptyExpression:represents())
end
  
-- End unit tests

function testExpression()
  runTests("Expression", test_Expression)
end

