require "Code"

WhitespaceReader = {}

function WhitespaceReader.startsWith(character)
  if character == " " or character == "\n" then
    return WhitespaceReader:new()
  end
end

function WhitespaceReader:new()
  local scanner = {}
  setmetatable(scanner, self)
  self.__index = self

  return scanner
end

function WhitespaceReader:readCharacter(character)
  if character ~= " " and character ~= "\n" then
    return Code.NULL
  else
    return
  end
end

