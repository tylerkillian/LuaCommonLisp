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

function test_Scanner.readSpace()
  local scanner = Scanner:new()
  assert(not scanner:readCharacter(" "))
end

local test_SymbolReader = {}

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

function test_SymbolReader.resetAfterTerminate()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab ")
  assert("" == symbolReader:toString())
end

test_StringReader = {}

function test_StringReader.addCharacter()
  local stringReader = StringReader:new()
  stringReader:readCharacter("a")
  assert("a" == stringReader:toString())
end

function test_StringReader.addTwoCharacters()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, "ab")
  assert("ab" == stringReader:toString())
end

function test_StringReader.terminate()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, "ab")
  assert("ab" == stringReader:readCharacter("\""))
end

function test_StringReader.resetAfterTerminate()
  local stringReader = StringReader:new()
  feedCharactersOneAtATime(stringReader, "ab\"")
  assert("" == stringReader:toString())
end

local test_ExpressionReader = {}

function test_ExpressionReader.construct()
  local expressionReader = ExpressionReader:new()
  assert("()", expressionReader:toString())
end

function test_ExpressionReader.switchFromScanToSymbol()
  local expressionReader = ExpressionReader:new()
  expressionReader:readCharacter("a")
  assert("():symbol" == expressionReader:toString())
end

function test_ExpressionReader.addSymbolToExpressionWhenReachSpace()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, "ab ")
  assert("(ab):scan" == expressionReader:toString())
end

function test_ExpressionReader.addSymbolToExpressionWhenReachString()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab"')
  assert("(ab):string" == expressionReader:toString())
end

function test_ExpressionReader.startStringWhenReachInitialQuotationMark()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"')
  assert("():string" == expressionReader:toString())
end

function test_ExpressionReader.returnStringWhenReachEndQuotationMark()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"ab"')
  assert("(ab):scan" == expressionReader:toString())
end

function test_ExpressionReader.switchFromStringToScan()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, '"ab"')
  assert("(ab):scan" == expressionReader:toString())
end

function test_ExpressionReader.switchFromSymbolToString()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab"')
  assert("(ab):string" == expressionReader:toString())
end

function test_ExpressionReader.terminate()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'ab cd')
  assert("(ab cd)" == convertExpressionToString(expressionReader:readCharacter(")")))
end

function test_ExpressionReader.nested()
  local expressionReader = ExpressionReader:new()
  feedCharactersOneAtATime(expressionReader, 'a (b c)')
  assert("(a (b c))" == convertExpressionToString(expressionReader:readCharacter(")")))
end

function test_ExpressionReader.construct()
  local defaultExpressionReader = ExpressionReader:new("element")
  assert("scan" == defaultExpressionReader:toString())
end

function test_ExpressionReader.switchFromScanToSymbol()
  local expressionReader = ExpressionReader:new("element")
  expressionReader:readCharacter("a")
  assert("symbol" == expressionReader:toString())
end

function test_ExpressionReader.switchFromSymbolToScan()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, "ab ")
  assert("scan" == expressionReader:toString())
end

function test_ExpressionReader.returnSymbolWhenReachSpace()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, "ab")
  assert("ab" == expressionReader:readCharacter(" "))
end

function test_ExpressionReader.returnSymbolWhenReachString()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, 'ab')
  assert("ab" == expressionReader:readCharacter('"'))
end

function test_ExpressionReader.startStringWhenReachInitialQuotationMark()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, '"')
  assert("string" == expressionReader:toString())
end

function test_ExpressionReader.returnStringWhenReachEndQuotationMark()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, '"ab')
  assert("ab" == expressionReader:readCharacter('"'))
end

function test_ExpressionReader.switchFromStringToScan()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, '"ab"')
  assert("scan" == expressionReader:toString())
end

function test_ExpressionReader.switchFromSymbolToString()
  local expressionReader = ExpressionReader:new("element")
  feedCharactersOneAtATime(expressionReader, 'ab"')
  assert("string" == expressionReader:toString())
end

local test_update_Scanner = {}

function test_update_Scanner.readSpace()
  local scanner = update_Scanner:new()
  scanner:readCharacter(" ")
  assert(not scanner:hasTerminated())
end

function test_update_Scanner.readNonWhitespace()
  local scanner = update_Scanner:new()
  scanner:readCharacter("a")
  assert(scanner:hasTerminated())
end

function test_update_Scanner.doesNotUseTerminalCharacter()
  local scanner = update_Scanner:new()
  scanner:readCharacter("a")
  assert(not scanner:usedTerminalCharacter())
end

function test_update_Scanner.resultIsNil()
  local scanner = update_Scanner:new()
  scanner:readCharacter("a")
  assert(not scanner:getResult())
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
  runTests("SymbolReader", test_SymbolReader)
  runTests("StringReader", test_StringReader)
  runTests("ExpressionReader", test_ExpressionReader)

  runTests("update_Scanner", test_update_Scanner)
end

