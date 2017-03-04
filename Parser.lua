Code = {
  NULL = {},
}

Scanner = {}

function Scanner.startsWith(character)
  if character == " " or character == "\n" then
    return Scanner:new()
  end
end

function Scanner:new()
  local scanner = {}
  setmetatable(scanner, self)
  self.__index = self

  return scanner
end

function Scanner:readCharacter(character)
  if character ~= " " and character ~= "\n" then
    return Code.NULL
  else
    return
  end
end

StringReader = {}

function StringReader.startsWith(character)
  if character == '"' then
    return StringReader:new()
  end
end

function StringReader:new()
  local stringReader = {
    isDone = false,
    queue = "",
  }
  setmetatable(stringReader, self)
  self.__index = self

  return stringReader
end

function StringReader:readCharacter(character)
  if self.isDone then
    return self.queue
  end

  if character == "\"" then
    self.isDone = true
  else
    self.queue = self.queue .. character
  end
end

function StringReader:toString()
  local isDoneString = "false"
  if self.isDone then
    isDoneString = "true"
  end

  return isDoneString .. ":" .. self.queue
end

SymbolReader = {}

function SymbolReader.startsWith(character)
  if character == '"' or character == "(" or character == " " or character == "\n" then
    return
  end

  local result = SymbolReader:new()
  result:readCharacter(character)
  return result
end

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
    return self.queue
  else
    self.queue = self.queue .. character
    return
  end
end

function SymbolReader:toString()
  return self.queue
end

ExpressionReader = {}

function ExpressionReader.startsWith(character)
  if character ~= "(" then
    return
  end

  return ExpressionReader:new()
end

function ExpressionReader:new()
  local expressionReader = {
    nextLink = Scanner:new(),
    expression = {},
    isDone = false,
  }
  setmetatable(expressionReader, self)
  self.__index = self

  return expressionReader
end

function ExpressionReader:readCharacter(character)
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

  self.nextLink = Scanner.startsWith(character) or
    StringReader.startsWith(character) or
    SymbolReader.startsWith(character) or
    ExpressionReader.startsWith(character)
end

function ExpressionReader:toString()
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
