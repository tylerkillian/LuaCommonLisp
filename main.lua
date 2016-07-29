Parser = {}

function Parser:new()
  local parser = {
    mode = "scan",
  }
  setmetatable(parser, self)
  self.__index = self

  return parser
end

function Parser:readCharacter_scan(character)
end

function Parser:readCharacter(character)
  if mode == "scan" then
    return self:readCharacter(character)
end

local parser = Parser:new()
local code = [[(format t "hello~t")]]
for codeIdx = 1, string.len(code) do
  local nextCharacter = string.sub(code, codeIdx, codeIdx)
  parser:readCharacter(nextCharacter)
end
