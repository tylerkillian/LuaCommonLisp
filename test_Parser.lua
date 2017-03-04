require "Parser"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

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

-- Begin unit tests

local test_Scanner = {}

function test_Scanner.startsWithSpace()
  assert(Scanner.startsWith(" "))
end

function test_Scanner.doesNotStartWithNonWhitespace()
  assert(not Scanner.startsWith("a"))
end

function test_Scanner.readSpace()
  local scanner = Scanner:new()
  local result = scanner:readCharacter(" ")
  assert(not result)
end

function test_Scanner.readNonWhitespace()
  local scanner = Scanner:new()
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

function test_Scanner.multipleWhitespaceCharacters()
  local scanner = Scanner:new()
  feedCharactersOneAtATime(scanner, "   ")
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

test_StringReader = {}

function test_StringReader.startsWithDoubleQuote()
  assert(StringReader.startsWith('"'))
end

function test_StringReader.doesNotStartWithNonDoubleQuote()
  assert(not StringReader.startsWith("a"))
end

function test_StringReader.addCharacter()
  local stringReader = StringReader:new()
  stringReader:readCharacter("a")
  assert("false:a" == stringReader:toString())
end

function test_StringReader.addTwoCharacters()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, "ab")
  assert("false:ab" == stringReader:toString())
end

function test_StringReader.returnOnNextCharacterAfterEndOfString()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, 'ab"')
  assert("ab" == stringReader:readCharacter('a'))
end

function test_StringReader.isDoneAfterEndOfString()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, 'ab"')
  assert("true:ab" == stringReader:toString())
end

function test_StringReader.doesNotReturnAtEndOfString()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, 'ab')
  assert(not stringReader:readCharacter('"'))
end

local test_SymbolReader = {}

function test_SymbolReader.startsWithAlphabeticCharacter()
  assert(SymbolReader.startsWith("a"))
end

function test_SymbolReader.startsWithNumber()
  assert(SymbolReader.startsWith("1"))
end

function test_SymbolReader.doesNotStartWithDoubleQuote()
  assert(not SymbolReader.startsWith('"'))
end

function test_SymbolReader.doesNotStartWithParentheses()
  assert(not SymbolReader.startsWith('('))
end

function test_SymbolReader.addSingleCharacter()
  local symbolReader = SymbolReader:new()
  symbolReader:readCharacter("a")
  assert("a" == symbolReader:toString())
end

function test_SymbolReader.addTwoCharacters()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:toString())
end

function test_SymbolReader.terminateWithSpace()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:readCharacter(" "))
end

local test_ExpressionReader = {}

function test_SymbolReader.startsWithParentheses()
  assert(ExpressionReader.startsWith('('))
end

function test_SymbolReader.doesNotStartWithAlphabeticCharacter()
  assert(not ExpressionReader.startsWith('a'))
end

function test_SymbolReader.doesNotStartWithNumber()
  assert(not ExpressionReader.startsWith('1'))
end

function test_SymbolReader.doesNotStartWithDoubleQuote()
  assert(not ExpressionReader.startsWith('"'))
end

function test_ExpressionReader.construct()
  local expressionReader = ExpressionReader:new()
  assert("():false", expressionReader:toString())
end

function test_ExpressionReader.switchFromScanToSymbol()
  local expressionReader = ExpressionReader:new()
  expressionReader:readCharacter("a")
  assert("():false" == expressionReader:toString())
end

function test_ExpressionReader.addSymbolToExpressionWhenReachSpace()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, "ab ")
  assert("(ab):false" == expressionReader:toString())
end

function test_ExpressionReader.addSymbolToExpressionWhenReachString()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab"')
  assert("(ab):false" == expressionReader:toString())
end

function test_ExpressionReader.startStringWhenReachInitialQuotationMark()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"')
  assert("():false" == expressionReader:toString())
end

function test_ExpressionReader.returnStringWhenReachEndQuotationMark()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"ab" ')
  assert("(ab):false" == expressionReader:toString())
end

function test_ExpressionReader.terminate()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab cd)')
  assert("(ab cd)" == convertExpressionToString(expressionReader:readCharacter(" ")))
end

function test_ExpressionReader.nested()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'a (b c))')
  assert("(a (b c))" == convertExpressionToString(expressionReader:readCharacter(" ")))
end

-- End unit tests

local function runTests(testCategory, tests)
  for testName, theTest in pairs(tests) do
    print("Running " .. testCategory .. " " .. testName)
    theTest()
  end
end

function testParser()
  runTests("Scanner", test_Scanner)
  runTests("StringReader", test_StringReader)
  runTests("SymbolReader", test_SymbolReader)
  runTests("ExpressionReader", test_ExpressionReader)
end

