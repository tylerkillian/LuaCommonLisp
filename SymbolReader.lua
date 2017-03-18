SymbolReader = {}

function SymbolReader:new(initialCharacter)
  if initialCharacter == '"' or
    initialCharacter == "(" or
    initialCharacter == " " or
    initialCharacter == "\n" or
    initialCharacter == "'" then
    return
  end

  local symbolReader = {
    queue = "",
  }
  setmetatable(symbolReader, self)
  self.__index = self

  symbolReader:readCharacter(initialCharacter)

  return symbolReader
end

function SymbolReader:readCharacter(character)
  if character == " " or character == '"' or character == ")" then
    return self.queue
  else
    self.queue = self.queue .. character
    return
  end
end

function SymbolReader:toString()
  return self.queue
end

