require "SymbolReader"
require "test_Reader"
require "test_utilities"

-- Begin unit tests

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

-- End unit tests

function testSymbolReader()
  runTests("SymbolReader", test_SymbolReader)
end

