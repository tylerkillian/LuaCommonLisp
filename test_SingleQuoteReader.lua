require "SingleQuoteReader"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

--[[
function convertExpressionToString(expression)
  if #expression == 0 then
    return "()"
  end

  local result = ""
  for _, current in ipairs(expression) do
    if type(current) == "table" then
      result = result .. " " .. convertExpressionToString(current)
    else
      result = result .. " " .. current
    end
  end
  return "(" .. string.sub(result, 2) .. ")"
end
--]]

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

test_SingleQuoteReader = {}

function test_SingleQuoteReader.startsWithSingleQuote()
  assert(SingleQuoteReader.startsWith("'"))
end

function test_SingleQuoteReader.quoteSymbol()
  local quoteReader = SingleQuoteReader.startsWith("'")
  quoteReader:readCharacter("a")
  assert("'a" == quoteReader:readCharacter(" "))
end

function test_SingleQuoteReader.quoteString()
  local quoteReader = SingleQuoteReader.startsWith("'")
  feedCharactersOneAtATime(quoteReader, '"a"')
  assert("'a" == quoteReader:readCharacter(" "))
end

function test_SingleQuoteReader.quoteExpression()
  local quoteReader = SingleQuoteReader.startsWith("'")
  feedCharactersOneAtATime(quoteReader, '("a" b)')
  assert("'(a b)" == quoteReader:readCharacter(" "))
end

function test_SingleQuoteReader.quoteQuoteSymbol()
  local quoteReader = SingleQuoteReader.startsWith("'")
  feedCharactersOneAtATime(quoteReader, "'a")
  assert("''a" == quoteReader:readCharacter(" "))
end

function test_SingleQuoteReader.nested()
  local quoteReader = SingleQuoteReader.startsWith("'")
  feedCharactersOneAtATime(quoteReader, "(a '(b 'c))")
  assert("'(a '(b 'c))" == quoteReader:readCharacter(" "))
end

-- End unit tests

local function runTests(testCategory, tests)
  for testName, theTest in pairs(tests) do
    print("Running " .. testCategory .. " " .. testName)
    theTest()
  end
end

function testSingleQuoteReader()
  runTests("SingleQuoteReader", test_SingleQuoteReader)
end
