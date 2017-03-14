require "SymbolReader"

local function feedCharactersOneAtATime(reader, characters)
  for index = 1, string.len(characters) do
    local nextCharacter = string.sub(characters, index, index)
    reader:readCharacter(nextCharacter)
  end
end

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

local function runTests(testCategory, tests)
  for testName, theTest in pairs(tests) do
    print("Running " .. testCategory .. " " .. testName)
    theTest()
  end
end

function testSymbolReader()
  runTests("SymbolReader", test_SymbolReader)
end

