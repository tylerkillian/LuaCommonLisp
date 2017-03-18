require "Code"

SingleQuoteReader = {}

function SingleQuoteReader:new(initialCharacter, readerFunctor)
  if initialCharacter ~= "'" then
    return
  end

  local reader = {
    nextLink = nil,
    readerFunctor = readerFunctor,
  }
  setmetatable(reader, self)
  self.__index = self

  return reader
end

function SingleQuoteReader:readCharacter(character)
  if not self.nextLink then
    self.nextLink = self.readerFunctor(character)
    return
  end

  local linkResult = self.nextLink:readCharacter(character)
  if not linkResult then
    return
  end

  if linkResult ~= Code.NULL then
    return "'" .. linkResult:toString()
  end

  self.nextLink = self.readerFunctor(character)
end

