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
print("got right parenthesis")
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
print("got " .. self.buffer)
    table.insert(self.currentTable, self.buffer)
    self.buffer = ""
    self.mode = "scan"
  elseif character == ")" then
print(self.buffer)
    table.insert(self.currentTable, self.buffer)
    self.buffer = ""
    self.mode = "scan"
print("got right parenthesis")
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
  io.write(theString)
end

function Environment:new()
  local environment = {
  }
  setmetatable(environment, self)
  self.__index = self

  return environment
end

function Environment:eval(code)
  if code[1] == "format" then
    Environment_format(code[3])
  end
end

local parser = Parser:new()
local codeString = [[(format t (concatenate 'string "hel" "lo~t"))]]
local returnCode
for codeIdx = 1, string.len(codeString) do
  local nextCharacter = string.sub(codeString, codeIdx, codeIdx)
  returnCode = parser:readCharacter(nextCharacter)
  if returnCode then
    environment:eval(returnCode)
  end
end
assert(type(returnCode) == "table")
assert(#returnCode == 3)
assert(returnCode[1] == "format")
assert(returnCode[2] == "t")
assert(type(returnCode[3]) == "table")
assert(#returnCode[3] == 4)
assert(returnCode[3][1] == "concatenate")
assert(returnCode[3][2] == "'string")
assert(returnCode[3][3] == "\"hel\"")
assert(returnCode[3][4] == "\"lo~t\"")

