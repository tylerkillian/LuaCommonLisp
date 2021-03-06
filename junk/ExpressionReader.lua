require "Code"
require "Expression"

ExpressionReader = {}

function ExpressionReader:new(initialCharacter, readerFunctor)
  if initialCharacter ~= "(" then
    return
  end

  local expressionReader = {
    nextLink = nil,
    expression = Expression:new(),
    isDone = false,
    readerFunctor = readerFunctor,
  }
  setmetatable(expressionReader, self)
  self.__index = self

  return expressionReader
end

function ExpressionReader:doneReading()
  return self.isDone
end

function ExpressionReader:getFullExpression()
    return self.expression
end

function ExpressionReader:linkIsReading()
  if self.nextLink then
    return true
  else
    return false
  end
end

function ExpressionReader:passToLink(character)
  local linkResult = self.nextLink:readCharacter(character)
  if not linkResult then
    return
  end

  if linkResult ~= Code.NULL then
    self.expression:push(linkResult)
  end

  self.nextLink = nil
end

function ExpressionReader:readyForNewLink()
  if self.nextLink then
    return false
  else
    return true
  end
end

function ExpressionReader:terminate()
  self.isDone = true
end

function ExpressionReader:createNewLink(character)
  self.nextLink = self.readerFunctor(character, self.readerFunctor)
end

function ExpressionReader:readCharacter(character)
  if self:doneReading() then
    return self:getFullExpression()
  end

  if self:linkIsReading() then
    self:passToLink(character)
  end

  if self:readyForNewLink() then
    if character == ")" then
      self:terminate()
    else
      self:createNewLink(character)
    end
  end
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
    result = result .. " " .. current:toString()
  end
  return "(" .. string.sub(result, 2) .. "):" .. isDoneString
end

