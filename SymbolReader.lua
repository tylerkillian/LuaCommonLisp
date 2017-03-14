SymbolReader = {}

function SymbolReader.startsWith(character)
  if character == '"' or
    character == "(" or
    character == " " or
    character == "\n" or
    character == "'" then
    return
  end

  local result = SymbolReader:new()
  result:readCharacter(character)
  return result
end

function SymbolReader:new()
  local symbolReader = {
    queue = "",
  }
  setmetatable(symbolReader, self)
  self.__index = self

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

