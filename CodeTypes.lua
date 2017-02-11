LString = {}

function LString:new(value)
  local lString = {
    data = value
  }
  setmetatable(lString, self)
  self.__index = self

  return lString
end

Symbol = {}

function Symbol:new(value)
  local symbol = {
    data = value
  }
  setmetatable(symbol, self)
  self.__index = self

  return symbol
end

Expression = {}

function Expression:new()
  local expression = {
  }
  setmetatable(expression, self)
  self.__index = self

  return expression
end

