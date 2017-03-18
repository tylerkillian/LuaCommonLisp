require "WhitespaceReader"
require "test_Reader"
require "test_utilities"

-- Begin unit tests

local test_WhitespaceReader = {}

function test_WhitespaceReader.startsWithSpace()
  assert(WhitespaceReader.startsWith(" "))
end

function test_WhitespaceReader.doesNotStartWithNonWhitespace()
  assert(not WhitespaceReader.startsWith("a"))
end

function test_WhitespaceReader.readSpace()
  local scanner = WhitespaceReader:new()
  local result = scanner:readCharacter(" ")
  assert(not result)
end

function test_WhitespaceReader.readNonWhitespace()
  local scanner = WhitespaceReader:new()
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

function test_WhitespaceReader.multipleWhitespaceCharacters()
  local scanner = WhitespaceReader:new()
  feedCharactersOneAtATime(scanner, "   ")
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

-- End unit tests

function testWhitespaceReader()
  runTests("WhitespaceReader", test_WhitespaceReader)
end

