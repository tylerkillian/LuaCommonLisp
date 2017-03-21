require "Expression"
require "test_Code"
require "test_utilities"

FakeContext = {}

function FakeContext:new()
  local context = {
    history = "",
    mode = "readyToBegin",
  }
  setmetatable(context, self)
  self.__index = self

  return context
end

function FakeContext:beginEvaluateExpression()
  assert(self.mode == "readyToBegin")
  self.history = self.history .. "("
end

function FakeContext:receiveSymbol(symbol)
  assert(self.mode == "readyForSymbol")
  self.history = self.history .. symbol:toString()
end

function FakeContext:receiveArgument(argument)
  assert(self.mode == "readyForArgument")
  self.history = self.history .. " " .. argument:toString()
end

function FakeContext:endEvaluateExpression()
  assert(self.mode == "readyForArgument")
  self.history = self.history .. ")"
end

function FakeContext:toString()
  return self.history
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

function test_Expression.evaluate()
  local threeEntries = Expression:new()
  threeEntries:push(FakeCode:new("a"))
  threeEntries:push(FakeCode:new("b"))
  threeEntries:push(FakeCode:new("c"))
  local context = FakeContext:new()
  threeEntries:evaluate(context)
  assert("(a b c)" == context:toString())
end
  
-- End unit tests

function testExpression()
  runTests("Expression", test_Expression)
end

