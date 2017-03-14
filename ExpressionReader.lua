require "Code"

ExpressionReader = {}

function ExpressionReader.startsWith(character)
  if character ~= "(" then
    return
  end

  return ExpressionReader:new()
end

function ExpressionReader:new(readerFunctor)
  local expressionReader = {
    nextLink = nil,
    expression = {},
    isDone = false,
    readerFunctor = readerFunctor,
  }
  setmetatable(expressionReader, self)
  self.__index = self

  return expressionReader
end

local function convertExpressionToString(expression)
  if #expression == 0 then
    return "()"
  end

  local result = ""
  for _, current in ipairs(expression) do
    result = result .. " " .. current
  end
  return "(" .. string.sub(result, 2) .. ")"
end

function ExpressionReader:readCharacter(character)
  if not self.nextLink then
    if character == ")" then
      self.isDone = true
    else
      self.nextLink = self.readerFunctor(character)
    end

    return
  end

  if self.isDone then
    return convertExpressionToString(self.expression)
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

  self.nextLink = self.readerFunctor(character)
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

