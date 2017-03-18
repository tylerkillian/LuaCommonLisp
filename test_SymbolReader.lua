require "SymbolReader"
require "test_Reader"
require "test_utilities"

-- Begin unit tests

local test_SymbolReader = {}

function test_SymbolReader.startsWithAlphabeticCharacter()
  assert(SymbolReader:new("a"))
end

function test_SymbolReader.startsWithNumber()
  assert(SymbolReader:new("1"))
end

function test_SymbolReader.doesNotStartWithDoubleQuote()
  assert(not SymbolReader:new('"'))
end

function test_SymbolReader.doesNotStartWithParentheses()
  assert(not SymbolReader:new('('))
end

function test_SymbolReader.addSingleCharacter()
  local symbolReader = SymbolReader:new("a")
  assert("a" == symbolReader:toString())
end

function test_SymbolReader.addTwoCharacters()
  local symbolReader = SymbolReader:new("a")
  symbolReader:readCharacter("b")
  assert("ab" == symbolReader:toString())
end

function test_SymbolReader.terminateWithSpace()
  local symbolReader = SymbolReader:new("a")
  symbolReader:readCharacter("b")
  assert("ab" == symbolReader:readCharacter(" "):getValue())
end

-- End unit tests

function testSymbolReader()
  runTests("SymbolReader", test_SymbolReader)
end

