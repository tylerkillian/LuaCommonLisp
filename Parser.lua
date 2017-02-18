Parser2 = {}

function Parser2:new()
  local parser = {
    mode = "scan",
    buffer = "",
    tableStack = {},
    currentTable = nil,
  }
  setmetatable(parser, self)
  self.__index = self

  return parser
end

function Parser2:readCharacter_scan(character)
  if character == "(" then
    if not self.currentTable then
      self.currentTable = {}
    else
      table.insert(self.tableStack, self.currentTable)
      table.insert(self.currentTable, {})
      self.currentTable = self.currentTable[#self.currentTable]
    end
  elseif character == ")" then
    if #self.tableStack == 0 then
      local code = self.currentTable
      self.currentTable = nil
      return code
    else
      self.currentTable = self.tableStack[#self.tableStack]
      table.remove(self.tableStack)
    end
  elseif character ~= " " and character ~= "\n" then
    self.mode = "symbol"
    self.buffer = character
  end
end

function Parser2:readCharacter_symbol(character)
  if character == " " then
    table.insert(self.currentTable, self.buffer)
    self.buffer = ""
    self.mode = "scan"
  elseif character == ")" then
    table.insert(self.currentTable, self.buffer)
    self.buffer = ""
    self.mode = "scan"
    if #self.tableStack == 0 then
      local code = self.currentTable
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

function Parser2:readCharacter(character)
  if self.mode == "scan" then
    return self:readCharacter_scan(character)
  elseif self.mode == "symbol" then
    return self:readCharacter_symbol(character)
  end
end

Scanner = {}

function Scanner:new()
  local scanner = {
  }
  setmetatable(scanner, self)
  self.__index = self

  return scanner
end

Parser = {}

function Parser:new()
  local parser = {
    nextLink = Scanner:new(),
    operatorQueue = {},
  }
  setmetatable(parser, self)
  self.__index = self

  return parser
end

function Parser:nextCharacter(character)
  local result = self.nextLink(character)

  if not self.nextLink:isDone() then
    return
  end
  local result
  if isDone

  if not result then
    return
  end

  if character == " then
    self.nextLink = StringReader:new()
  end

  return result
end

