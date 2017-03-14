Code = {
  NULL = {},
}

Scanner = {}

function Scanner.startsWith(character)
  if character == " " or character == "\n" then
    return Scanner:new()
  end
end

function Scanner:new()
  local scanner = {}
  setmetatable(scanner, self)
  self.__index = self

  return scanner
end

function Scanner:readCharacter(character)
  if character ~= " " and character ~= "\n" then
    return Code.NULL
  else
    return
  end
end

