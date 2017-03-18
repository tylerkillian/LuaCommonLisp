require "Code"

WhitespaceReader = {}

function WhitespaceReader:new(initialCharacter)
  if initialCharacter ~= " " and initialCharacter ~= "\n" then
    return
  end

  local reader = {}
  setmetatable(reader, self)
  self.__index = self

  return reader
end

function WhitespaceReader:readCharacter(character)
  if character ~= " " and character ~= "\n" then
    return Code.NULL
  else
    return
  end
end

