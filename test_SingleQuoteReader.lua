require "SingleQuoteReader"
require "test_Reader"
require "test_utilities"

-- Begin unit tests

test_SingleQuoteReader = {}

function test_SingleQuoteReader.startsWithSingleQuote()
  assert(SingleQuoteReader:new("'"))
end

function test_SingleQuoteReader.readCodeAfterQuote()
  local quoteReader = SingleQuoteReader:new("'", createFakeReaderFunctor{
    FakeReader:new(createFakeCode("a")),
  })
  feedCharactersOneAtATime(quoteReader, "x")
  assert("'a" == quoteReader:readCharacter(" "))
end

function test_SingleQuoteReader.skipWhitespace()
  local quoteReader = SingleQuoteReader:new("'", createFakeReaderFunctor{
    FakeReader:new(Code.NULL),
    FakeReader:new(createFakeCode("a")),
  })
  feedCharactersOneAtATime(quoteReader, " x")
  assert("'a" == quoteReader:readCharacter(" "))
end

-- End unit tests

function testSingleQuoteReader()
  runTests("SingleQuoteReader", test_SingleQuoteReader)
end

