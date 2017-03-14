require "Scanner"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

-- Begin unit tests

local test_Scanner = {}

function test_Scanner.startsWithSpace()
  assert(Scanner.startsWith(" "))
end

function test_Scanner.doesNotStartWithNonWhitespace()
  assert(not Scanner.startsWith("a"))
end

function test_Scanner.readSpace()
  local scanner = Scanner:new()
  local result = scanner:readCharacter(" ")
  assert(not result)
end

function test_Scanner.readNonWhitespace()
  local scanner = Scanner:new()
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end

function test_Scanner.multipleWhitespaceCharacters()
  local scanner = Scanner:new()
  feedCharactersOneAtATime(scanner, "   ")
  local result = scanner:readCharacter("a")
  assert(result == Code.NULL)
end


local function runTests(testCategory, tests)
  for testName, theTest in pairs(tests) do
    print("Running " .. testCategory .. " " .. testName)
    theTest()
  end
end

function testScanner()
  runTests("Scanner", test_Scanner)
end

