Parser = {}

function Parser:new()
  local parser = {}
  setmetatable(parser, self)
  self.__index = self

  return parser
end

function Parser:readCharacter(character)
end

local parser = Parser:new()
local code = [[(format t "hello~t")]]
parser:readCharacter
