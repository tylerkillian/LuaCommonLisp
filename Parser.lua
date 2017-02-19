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

function Scanner:readCharacter(character)
  if character == " " then
    return
  else
    return "DONE"
  end
end

SymbolReader = {}

function SymbolReader:new()
  local symbolReader = {
    queue = "",
  }
  setmetatable(symbolReader, self)
  self.__index = self

  return symbolReader
end

function SymbolReader:readCharacter(character)
  if character == " " then
    local result = self.queue
    self.queue = ""
    return result
  else
    self.queue = self.queue .. character
    return
  end
end

function SymbolReader:toString()
  return self.queue
end

Parser = {
}

function Parser:new()
  local parser = {
    nextLink = Scanner:new(),
    operatorQueue = {},
    state = "scan",
  }
  setmetatable(parser, self)
  self.__index = self

  return parser
end

local function getNewState(currentState, terminalCharacter)
  if currentState == "scan" then

    if terminalCharacter == "\"" then
      return "string"
    elseif terminalCharacter == "(" then
      return "expression"
    else
      return "symbol"
    end

  elseif currentState == "symbol" then

    if terminalCharacter == "\"" then
      return "string"
    elseif terminalCharacter == "(" then
      return "expression"
    else
      return "scan"
    end

  elseif currentState == "string" then

    if terminalCharacter == "\"" then
      return "scan"
    end

  end
end

local function isOperator()
  return false
end

function Parser:readCharacter(character)
  local result = self.nextLink:readCharacter(character)
print(result)

  if not result then
    return
  end

  if isOperator(character) then
  else
    local newState = getNewState(self.state, character)

    if newState == "symbol" then
      self.nextLink = SymbolReader:new()
      self.state = "symbol"
    elseif newState == "scan" then
      self.nextLink = Scanner:new()
      self.state = "scan"
    end

  end

  return result
end

function Parser:toString()
  return self.state
end
