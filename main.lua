Parser = {}

function Parser:new()
  local parser = {
    mode = "scan",
    buffer = "",
    code = nil,
  }
  setmetatable(parser, self)
  self.__index = self

  return parser
end

function Parser:readCharacter_scan(character)
  if character == "(" then
    if not self.code then
      self.code = {}
      self.currentTable = self.code
    else
      table.insert(self.code, {})
      self.currentTable = self.code[#self.code]
    end
  elseif character ~= " " then
    self.mode = "symbol"
    self.buffer = character
  end
end

function Parser:readCharacter(character)
  if self.mode == "scan" then
    return self:readCharacter_scan(character)
  end
end

local parser = Parser:new()
local code = [[(format t "hello~t")]]
for codeIdx = 1, string.len(code) do
  local nextCharacter = string.sub(code, codeIdx, codeIdx)
  parser:readCharacter(nextCharacter)
end
