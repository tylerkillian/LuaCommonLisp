Parser = {}

function Parser:new()
  local parser = {
    mode = "scan",
    buffer = "",
    code = nil,
    tableStack = {},
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
      table.insert(self.tableStack, self.currentTable)
      table.insert(self.code, {})
      self.currentTable = self.code[#self.code]
    end
  elseif character == ")" then
    if #self.tableStack == 0 then
      local code = self.code
      self.code = nil
      self.currentTable = nil
      return code
    else
      self.currentTable = self.tableStack[#self.tableStack]
      table.remove(self.tableStack)
    end
  elseif character ~= " " then
    self.mode = "symbol"
    self.buffer = character
  end
end

function Parser:readCharacter_symbol(character)
  if character == " " then
    table.insert(self.currentTable, self.buffer)
    self.buffer = ""
    self.mode = "scan"
  elseif character == ")" then
    table.insert(self.currentTable, self.buffer)
    self.buffer = ""
    self.mode = "scan"
    if #self.tableStack == 0 then
      local code = self.code
      self.code = nil
      self.currentTable = nil
      return code
    else
      self.currentTable = self.tableStack[#self.tableStack]
      table.remove(self.tableStack)
    end
  else
    self.buffer = self.buffer .. character
  end
end

function Parser:readCharacter(character)
  if self.mode == "scan" then
    return self:readCharacter_scan(character)
  elseif self.mode == "symbol" then
    return self:readCharacter_symbol(character)
  end
end

Environment = {}

function Environment_format(theString)
  local formattedString = string.gsub(theString, "~%%", "\n")
  io.write(formattedString)
end

function Environment:new()
  local environment = {
  }
  setmetatable(environment, self)
  self.__index = self

  return environment
end

function Environment:eval(code)
  if type(code) == "string" then
    return code
  end
  if code[1] == "format" then
    Environment_format(self:eval(code[3]))
  end
  if code[1] == "concatenate" then
    return string.sub(code[3], 2, -2) .. string.sub(code[4], 2, -2)
  end
end

local parser = Parser:new()
local codeString = [[(format t (concatenate 'string "hel" "lo~%"))]]
local code
local environment = Environment:new()
for codeStringIdx = 1, string.len(codeString) do
  local nextCharacter = string.sub(codeString, codeStringIdx, codeStringIdx)
  code = parser:readCharacter(nextCharacter)
  if code then
    environment:eval(code)
  end
end
assert(type(code) == "table")
assert(#code == 3)
assert(code[1] == "format")
assert(code[2] == "t")
assert(type(code[3]) == "table")
assert(#code[3] == 4)
assert(code[3][1] == "concatenate")
assert(code[3][2] == "'string")
assert(code[3][3] == "\"hel\"")
assert(code[3][4] == "\"lo~%\"")

local input = io.open("allTests.cl")

