LString = {}

function LString:new(value)
  local lString = {
    data = value
  }
  setmetatable(lString, self)
  self.__index = self

  return lString
end

function LString:toString()
  return self.data
end

Symbol = {}

function Symbol:new(value)
  local symbol = {
    evaluate = true,
    data = value or ""
  }
  setmetatable(symbol, self)
  self.__index = self

  return symbol
end

function Symbol:toString()
  if self.evaluate then
    return "true:" .. self.data
  else
    return "false:" .. self.data
  end
end

Expression = {}

function Expression:new()
  local expression = {
  }
  setmetatable(expression, self)
  self.__index = self

  return expression
end

function Expression:toString()
  return "()"
end


