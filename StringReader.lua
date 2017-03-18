require "LString"

StringReader = {}

function StringReader:new(initialCharacter)
  if initialCharacter ~= '"' then
    return
  end

  local stringReader = {
    queue = "",
    isDone = false,
  }
  setmetatable(stringReader, self)
  self.__index = self

  return stringReader
end

function StringReader:readCharacter(character)
  if self.isDone then
    return LString(self.queue)
  end

  if character == '"' then
    self.isDone = true
    return
  end

  self.queue = self.queue .. character
end

function StringReader:toString()
  return self.queue
end

