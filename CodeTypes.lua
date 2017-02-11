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
  setmetatable(Symbol, self)
  self.__index = self

  return symbol
end
