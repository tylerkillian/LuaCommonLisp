require "StringReader"
require "test_Reader"
require "test_utilities"

-- Begin unit tests

local test_StringReader = {}

function test_StringReader.doesNotStartWithAlphabeticCharacter()
  assert(not StringReader:new("a"))
end

function test_StringReader.doesNotStartWithNumber()
  assert(not StringReader:new("1"))
end

function test_StringReader.doesNotStartWithParentheses()
  assert(not StringReader:new('('))
end

function test_StringReader.startsWithDoubleQuote()
  assert(StringReader:new('"'))
end

function test_StringReader.addSingleCharacter()
  local stringReader = StringReader:new('"')
  stringReader:readCharacter("a")
  assert("a" == stringReader:toString())
end

function test_StringReader.addTwoCharacters()
  local stringReader = StringReader:new('"')
  stringReader:readCharacter("a")
  stringReader:readCharacter("b")
  assert("ab" == stringReader:toString())
end

function test_StringReader.terminateWithSpace()
  local stringReader = StringReader:new('"')
  stringReader:readCharacter("a")
  stringReader:readCharacter("b")
  stringReader:readCharacter('"')
  assert("ab" == stringReader:readCharacter(" "))
end

-- End unit tests

function testStringReader()
  runTests("StringReader", test_StringReader)
end

