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

local test_update_Scanner = {}

function test_update_Scanner.startsWithSpace()
  assert(update_Scanner.startsWith(" "))
end

function test_update_Scanner.doesNotStartWithNonWhitespace()
  assert(not update_Scanner.startsWith("a"))
end

function test_update_Scanner.readSpace()
  local scanner = update_Scanner:new()
  local result = scanner:readCharacter(" ")
  assert(not result)
end

function test_update_Scanner.readNonWhitespace()
  local scanner = update_Scanner:new()
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

function test_update_Scanner.multipleWhitespaceCharacters()
  local scanner = update_Scanner:new()
  feedCharactersOneAtATime(scanner, "   ")
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

test_update_StringReader = {}

function test_update_StringReader.startsWithDoubleQuote()
  assert(update_StringReader.startsWith('"'))
end

function test_update_StringReader.doesNotStartWithNonDoubleQuote()
  assert(not update_StringReader.startsWith("a"))
end

function test_update_StringReader.addCharacter()
  local stringReader = update_StringReader:new()
  stringReader:readCharacter("a")
  assert("false:a" == stringReader:toString())
end

function test_update_StringReader.addTwoCharacters()
  local stringReader = update_StringReader:new()
  feedCharactersOneAtATime(stringReader, "ab")
  assert("false:ab" == stringReader:toString())
end

function test_update_StringReader.returnOnNextCharacterAfterEndOfString()
  local stringReader = update_StringReader:new()
  feedCharactersOneAtATime(stringReader, 'ab"')
  assert("ab" == stringReader:readCharacter('a'))
end

function test_update_StringReader.isDoneAfterEndOfString()
  local stringReader = update_StringReader:new()
  feedCharactersOneAtATime(stringReader, 'ab"')
  assert("true:ab" == stringReader:toString())
end

function test_update_StringReader.doesNotReturnAtEndOfString()
  local stringReader = update_StringReader:new()
  feedCharactersOneAtATime(stringReader, 'ab')
  assert(not stringReader:readCharacter('"'))
end

local test_update_SymbolReader = {}

function test_update_SymbolReader.startsWithAlphabeticCharacter()
  assert(update_SymbolReader.startsWith("a"))
end

function test_update_SymbolReader.startsWithNumber()
  assert(update_SymbolReader.startsWith("1"))
end

function test_update_SymbolReader.doesNotStartWithDoubleQuote()
  assert(not update_SymbolReader.startsWith('"'))
end

function test_update_SymbolReader.doesNotStartWithParentheses()
  assert(not update_SymbolReader.startsWith('('))
end

function test_update_SymbolReader.addSingleCharacter()
  local symbolReader = update_SymbolReader:new()
  symbolReader:readCharacter("a")
  assert("a" == symbolReader:toString())
end

function test_update_SymbolReader.addTwoCharacters()
  local symbolReader = update_SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:toString())
end

function test_update_SymbolReader.terminateWithSpace()
  local symbolReader = update_SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:readCharacter(" "))
end

local test_update_ExpressionReader = {}

function test_update_SymbolReader.startsWithParentheses()
  assert(update_ExpressionReader.startsWith('('))
end

function test_update_SymbolReader.doesNotStartWithAlphabeticCharacter()
  assert(not update_ExpressionReader.startsWith('a'))
end

function test_update_SymbolReader.doesNotStartWithNumber()
  assert(not update_ExpressionReader.startsWith('1'))
end

function test_update_SymbolReader.doesNotStartWithDoubleQuote()
  assert(not update_ExpressionReader.startsWith('"'))
end

function test_update_ExpressionReader.construct()
  local expressionReader = update_ExpressionReader:new()
  assert("():false", expressionReader:toString())
end

function test_update_ExpressionReader.switchFromScanToSymbol()
  local expressionReader = update_ExpressionReader:new()
  expressionReader:readCharacter("a")
  assert("():false" == expressionReader:toString())
end

function test_update_ExpressionReader.addSymbolToExpressionWhenReachSpace()
  local expressionReader = update_ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, "ab ")
  assert("(ab):false" == expressionReader:toString())
end

function test_update_ExpressionReader.addSymbolToExpressionWhenReachString()
  local expressionReader = update_ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab"')
  assert("(ab):false" == expressionReader:toString())
end

function test_update_ExpressionReader.startStringWhenReachInitialQuotationMark()
  local expressionReader = update_ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"')
  assert("():false" == expressionReader:toString())
end

function test_update_ExpressionReader.returnStringWhenReachEndQuotationMark()
  local expressionReader = update_ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"ab" ')
  assert("(ab):false" == expressionReader:toString())
end

function test_update_ExpressionReader.terminate()
  local expressionReader = update_ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab cd)')
  assert("(ab cd)" == convertExpressionToString(expressionReader:readCharacter(" ")))
end

function test_update_ExpressionReader.nested()
  local expressionReader = update_ExpressionReader:new()
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
  runTests("update_Scanner", test_update_Scanner)
  runTests("update_StringReader", test_update_StringReader)
  runTests("update_SymbolReader", test_update_SymbolReader)
  runTests("update_ExpressionReader", test_update_ExpressionReader)
end

