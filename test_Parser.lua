require "Parser"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

-- Begin unit tests

local test_Parser = {}

function test_Parser.Scanner_readSpace()
  local scanner = Scanner:new()
  assert(not scanner:readCharacter(" "))
end

function test_Parser.SymbolReader_addSingleCharacter()
  local symbolReader = SymbolReader:new()
  symbolReader:readCharacter("a")
  assert("a" == symbolReader:toString())
end

function test_Parser.SymbolReader_addTwoCharacters()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:toString())
end

function test_Parser.SymbolReader_terminateWithSpace()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:readCharacter(" "))
end

function test_Parser.SymbolReader_resetAfterTerminate()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab ")
  assert("" == symbolReader:toString())
end

function test_Parser.Parser_construct()
  local defaultParser = Parser:new()
  assert("scan" == defaultParser:toString())
end

function test_Parser.Parser_switchFromScanToSymbol()
  local parser = Parser:new()
  parser:readCharacter("a")
  assert("symbol" == parser:toString())
end

function testParser()
  for testName, theTest in pairs(test_Parser) do
    print("Running " .. testName)
    theTest()
  end
end

