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
    result = result .. " " .. current
  end
  return "(" .. string.sub(result, 2) .. "):" .. self.state
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

local test_Parser = {}

function test_Parser.construct()
  local defaultParser = Parser:new()
  assert("scan" == defaultParser:toString())
end

function test_Parser.switchFromScanToSymbol()
  local parser = Parser:new()
  parser:readCharacter("a")
  assert("symbol" == parser:toString())
end

function test_Parser.switchFromSymbolToScan()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, "ab ")
  assert("scan" == parser:toString())
end

function test_Parser.returnSymbolWhenReachSpace()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, "ab")
  assert("ab" == parser:readCharacter(" "))
end

function test_Parser.returnSymbolWhenReachString()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, 'ab')
  assert("ab" == parser:readCharacter('"'))
end

function test_Parser.startStringWhenReachInitialQuotationMark()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, '"')
  assert("string" == parser:toString())
end

function test_Parser.returnStringWhenReachEndQuotationMark()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, '"ab')
  assert("ab" == parser:readCharacter('"'))
end

function test_Parser.switchFromStringToScan()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, '"ab"')
  assert("scan" == parser:toString())
end

function test_Parser.switchFromSymbolToString()
  local parser = Parser:new()
  feedCharactersOneAtATime(parser, 'ab"')
  assert("string" == parser:toString())
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
  runTests("Parser", test_Parser)
end

