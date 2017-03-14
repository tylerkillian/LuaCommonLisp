require "Code"

SingleQuoteReader = {}

function SingleQuoteReader.startsWith(character)
  if character == "'" then
    return SingleQuoteReader:new()
  end
end

function SingleQuoteReader:new()
  local reader = {
    nextLink = Scanner:new(),
  }
  setmetatable(reader, self)
  self.__index = self

  return reader
end

function SingleQuoteReader:readCharacter(character)
  local linkResult = self.nextLink:readCharacter(character)
  if not linkResult then
    return
  end

  if linkResult ~= Code.NULL then
    return "'" .. linkResult
  end

  self.nextLink = getNewReaderUsingInitialCharacter(character)
end

