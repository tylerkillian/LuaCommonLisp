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
