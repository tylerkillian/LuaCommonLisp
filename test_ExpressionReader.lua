require "ExpressionReader"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

FakeReader = {}

function FakeReader:new(returnValue)
  local reader = {
    returnValue = returnValue
  }
  setmetatable(reader, self)
  self.__index = self

  return reader
end

function FakeReader:readCharacter(character)
  return self.returnValue
end

function createFakeReaderFunctor(readers)
  local index = 0
  return function(firstCharacter)
      index = index + 1
      return readers[index]
  end
end

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
    FakeReader:new("ab"),
    FakeReader:new("cd"),
  })
  feedCharactersOneAtATime(expressionReader, 'xx)')
  assert("(ab cd)" == expressionReader:readCharacter(" "))
end

function test_ExpressionReader.nested()
  local expressionReader = ExpressionReader:new("(", createFakeReaderFunctor{
    FakeReader:new("a"),
    ExpressionReader:new("(", createFakeReaderFunctor{
      FakeReader:new("b"),
      FakeReader:new("c"),
    }),
  })
  feedCharactersOneAtATime(expressionReader, 'x(xx))')
print(expressionReader:readCharacter(" "))
  assert("(a (b c))" == expressionReader:readCharacter(" "))
end

-- End unit tests

local function runTests(testCategory, tests)
  for testName, theTest in pairs(tests) do
    print("Running " .. testCategory .. " " .. testName)
    theTest()
  end
end

function testExpressionReader()
  runTests("ExpressionReader", test_ExpressionReader)
end

