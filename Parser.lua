Code = {
  NULL = {},
}

update_Scanner = {}

function update_Scanner.startsWith(character)
  if character == " " or character == "\n" then
    return update_Scanner:new()
  end
end

function update_Scanner:new()
  local scanner = {}
  setmetatable(scanner, self)
  self.__index = self

  return scanner
end

function update_Scanner:readCharacter(character)
  if character ~= " " and character ~= "\n" then
    return Code.NULL
  else
    return
  end
end

update_StringReader = {}

function update_StringReader.startsWith(character)
  if character == '"' then
    return update_StringReader:new()
  end
end

function update_StringReader:new()
  local stringReader = {
    isDone = false,
    queue = "",
  }
  setmetatable(stringReader, self)
  self.__index = self

  return stringReader
end

function update_StringReader:readCharacter(character)
  if self.isDone then
    return self.queue
  end

  if character == "\"" then
    self.isDone = true
  else
    self.queue = self.queue .. character
  end
end

function update_StringReader:toString()
  local isDoneString = "false"
  if self.isDone then
    isDoneString = "true"
  end

  return isDoneString .. ":" .. self.queue
end

update_SymbolReader = {}

function update_SymbolReader.startsWith(character)
  if character == '"' or character == "(" or character == " " or character == "\n" then
    return
  end

  local result = update_SymbolReader:new()
  result:readCharacter(character)
  return result
end

function update_SymbolReader:new()
  local symbolReader = {
    queue = "",
  }
  setmetatable(symbolReader, self)
  self.__index = self

  return symbolReader
end

function update_SymbolReader:readCharacter(character)
  if character == " " or character == '"' or character == ")" then
    return self.queue
  else
    self.queue = self.queue .. character
    return
  end
end

function update_SymbolReader:toString()
  return self.queue
end

update_ExpressionReader = {}

function update_ExpressionReader.startsWith(character)
  if character ~= "(" then
    return
  end

  return update_ExpressionReader:new()
end

function update_ExpressionReader:new()
  local expressionReader = {
    nextLink = update_Scanner:new(),
    expression = {},
    isDone = false,
  }
  setmetatable(expressionReader, self)
  self.__index = self

  return expressionReader
end

function update_ExpressionReader:readCharacter(character)
  if self.isDone then
    return self.expression
  end

  local linkResult = self.nextLink:readCharacter(character)
  if not linkResult then
    return
  end

  if linkResult ~= Code.NULL then
    table.insert(self.expression, linkResult)
  end

  if character == ")" then
    self.isDone = true
    return
  end

  self.nextLink = update_Scanner.startsWith(character) or
    update_StringReader.startsWith(character) or
    update_SymbolReader.startsWith(character) or
    update_ExpressionReader.startsWith(character)
end

function update_ExpressionReader:toString()
  local isDoneString = "false"
  if self.isDone then
    isDoneString = "true"
  end

  if #self.expression == 0 then
    return "():" .. isDoneString
  end

  local result = ""
  for _, current in ipairs(self.expression) do
    result = result .. " " .. current
  end
  return "(" .. string.sub(result, 2) .. "):" .. isDoneString
end
