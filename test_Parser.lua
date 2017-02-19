require "Parser"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
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

-- End unit tests

local function runTests(tests)
  for testName, theTest in pairs(tests) do
    print("Running " .. testName)
    theTest()
  end
end

function testParser()
  runTests(test_Scanner)
  runTests(test_SymbolReader)
  runTests(test_Parser)
end

