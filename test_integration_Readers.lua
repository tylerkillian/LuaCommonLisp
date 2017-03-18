require "ExpressionReader"
require "SingleQuoteReader"
require "StringReader"
require "SymbolReader"
require "test_utilities"

-- Begin integration tests

test_integrateReaders = {}

function test_integrateReaders.validCharacterOnlyRecognizedByOneReader()
  local validCharacters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\"('"

  for characterIndex = 1, string.len(validCharacters) do
    local nextCharacter = string.sub(validCharacters, characterIndex, characterIndex)

    local numberOfRecognitions = 0;

    if Scanner.startsWith(nextCharacter) then
      numberOfRecognitions = numberOfRecognitions + 1
    end

    if SymbolReader.startsWith(nextCharacter) then
      numberOfRecognitions = numberOfRecognitions + 1
    end

    if StringReader.startsWith(nextCharacter) then
      numberOfRecognitions = numberOfRecognitions + 1
    end

    if ExpressionReader.startsWith(nextCharacter) then
      numberOfRecognitions = numberOfRecognitions + 1
    end

    if SingleQuoteReader.startsWith(nextCharacter) then
      numberOfRecognitions = numberOfRecognitions + 1
    end

    assert(numberOfRecognitions == 1)
  end
end

-- End integration tests

function testReaders()
  runTests("integrate Readers", test_integrateReaders)
end

