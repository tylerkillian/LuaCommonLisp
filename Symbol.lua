Symbol = {}

function Symbol:new(value)
  local lString = {
    data = value
  }
  setmetatable(lString, self)
  self.__index = self

  return lString
end

function Symbol:getValue()
  return self.data
end

function Symbol:toString()
  return self.data
end

