require "ExpressionReader"
require "test_Code"
require "test_Reader"
require "test_utilities"

-- Begin unit tests

local test_ExpressionReader = {}

function test_ExpressionReader.startsWithParentheses()
  assert(ExpressionReader:new('('))
end

function test_ExpressionReader.doesNotStartWithAlphabeticCharacter()
  assert(not ExpressionReader:new('a'))
end

function test_ExpressionReader.doesNotStartWithNumber()
  assert(not ExpressionReader:new('1'))
end

function test_ExpressionReader.doesNotStartWithDoubleQuote()
  assert(not ExpressionReader:new('"'))
end

function test_ExpressionReader.construct()
  local expressionReader = ExpressionReader:new("(")
  assert("():false", expressionReader:toString())
end

function test_ExpressionReader.terminate()
  local expressionReader = ExpressionReader:new("(", createFakeReaderFunctor{
    FakeReader:new(FakeCode:new("ab")),
    FakeReader:new(FakeCode:new("cd")),
  })
  feedCharactersOneAtATime(expressionReader, 'xx)')
  assert("(ab cd)" == expressionReader:readCharacter(" "):toString())
end

function test_ExpressionReader.nested()
  local expressionReader = ExpressionReader:new("(", createFakeReaderFunctor{
    FakeReader:new(FakeCode:new("a")),
    ExpressionReader:new("(", createFakeReaderFunctor{
      FakeReader:new(FakeCode:new("b")),
      FakeReader:new(FakeCode:new("c")),
    }),
  })
  feedCharactersOneAtATime(expressionReader, 'x(xx))')
  assert("(a (b c))" == expressionReader:readCharacter(" "):toString())
end

-- End unit tests

function testExpressionReader()
  runTests("ExpressionReader", test_ExpressionReader)
end

