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
  if character == " " or character == '"' or character == ")" then
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

StringReader = {}

function StringReader:new()
  local stringReader = {
    queue = "",
  }
  setmetatable(stringReader, self)
  self.__index = self

  return stringReader
end

function StringReader:readCharacter(character)
  if character == "\"" then
    local result = self.queue
    self.queue = ""
    return result
  else
    self.queue = self.queue .. character
    return
  end
end

function StringReader:toString()
  return self.queue
end

ExpressionReader = {}

function ExpressionReader:new(returnBy,name)
  local expressionReader = {
    nextLink = Scanner:new(),
    operatorQueue = {},
    state = "scan",
    expression = {},
    returnBy = returnBy or "collection",
name = name or "base",
  }
print("new with " .. expressionReader.name)
  setmetatable(expressionReader, self)
  self.__index = self

  return expressionReader
end

function ExpressionReader:reset()
  self.nextLink = Scanner:new()
  self.operatorQueue = {}
  self.state = "scan"
  self.expression = {}
end

local function getNewState(currentState, terminalCharacter, self)
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

  elseif currentState == "expression" then

     if terminalCharacter == ")" then
       return "scan"
     end

  end

print(self.name .. " " .. currentState)
print(self.name .. " " .. terminalCharacter)
  assert(false)
end

local function isOperator()
  return false
end

function ExpressionReader:changeState(currentStateTerminalCharacter)
print(self.name .. " old state = " .. self.state)
    self.state = getNewState(self.state, currentStateTerminalCharacter, self)

    if self.state == "scan" then
      self.nextLink = Scanner:new()
    elseif self.state == "symbol" then
      self.nextLink = SymbolReader:new()
      self.nextLink:readCharacter(currentStateTerminalCharacter)
    elseif self.state == "string" then
      self.nextLink = StringReader:new()
    elseif self.state == "expression" then
print("new expression")
      self.nextLink = ExpressionReader:new(_, "child")
    end
print(self.name .. " new state = " .. self.state)
end

function ExpressionReader:callNextLink(character)
  local linkResult = self.nextLink:readCharacter(character)

  if linkResult and self.state ~= "scan" and self.returnBy == "collection" then
print(self.name .. " storing " .. linkResult)
    table.insert(self.expression, linkResult)
  end

  return linkResult
end

function ExpressionReader:returningExpression(character)
  if self.returnBy == "collection" and self.state ~= "expression" and character == ")" then
    return true
  else
    return false
  end
end

function ExpressionReader:getReturnValue(linkResult, character)
  if not linkResult or self.state == "scan" then
    return
  end

  if self:returningExpression(character) then
    return self.expression
  end

  return linkResult
end

function ExpressionReader:prepareForNextCharacter(linkResult, character)
  if not linkResult then
    return
  end

  if self:returningExpression(character) then
    self:reset()
  else
    self:changeState(character)
  end
end

function ExpressionReader:readCharacter(character)
if false then
  local result = self.nextLink:readCharacter(character)

  if not result then
    return
  end

  if self.state ~= "scan" and self.returnBy == "collection" then
    table.insert(self.expression, result)
  end

  if self.state ~= "expression" and character == ")" then
    assert(self.returnBy == "collection")
    local expressionToReturn = self.expression
    self:reset()
    return expressionToReturn
  end

  if isOperator(character) then
  else
    self:changeState(character)
  end

  if self.returnBy == "element" then
    return result
  end
else
  local linkResult = self:callNextLink(character)

  local result = self:getReturnValue(linkResult, character)

  self:prepareForNextCharacter(linkResult, character)

  return result
end
end

function ExpressionReader:toString()
  if self.returnBy == "element" then
    return self.state
  end

  if #self.expression == 0 then
    return "():" .. self.state
  end

  local result = ""
  for _, current in ipairs(self.expression) do
    result = result .. " " .. current
  end
  return "(" .. string.sub(result, 2) .. "):" .. self.state
end

