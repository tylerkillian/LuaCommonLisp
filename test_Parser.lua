require "Parser"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

-- Begin unit tests

local function Scanner_readSpace()
  local scanner = Scanner:new()
  assert(not scanner:readCharacter(" "))
end

local function SymbolReader_addSingleCharacter()
  local symbolReader = SymbolReader:new()
  symbolReader:readCharacter("a")
  assert("a" == symbolReader:toString())
end

local function SymbolReader_addTwoCharacters()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:toString())
end

local function SymbolReader_terminateWithSpace()
  local symbolReader = SymbolReader:new()
  feedCharactersOneAtATime(symbolReader, "ab")
  assert("ab" == symbolReader:readCharacter(" "))
end

local function Parser_construct()
  local defaultParser = Parser:new()
  assert("scan" == defaultParser:toString())
end

local function Parser_switchFromScanToSymbol()
  local parser = Parser:new()
  parser:readCharacter("a")
  assert("symbol" == parser:toString())
end

function testParser()
  local Parser_tests = {
    Scanner_readSpace,
    SymbolReader_addSingleCharacter,
    SymbolReader_addTwoCharacters,
    SymbolReader_terminateWithSpace,
    Parser_construct,
    Parser_switchFromScanToSymbol,
  }

  for testName, theTest in pairs(Parser_tests) do
    print("Running " .. testName)
    theTest()
  end
end

