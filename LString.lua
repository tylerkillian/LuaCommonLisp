LString = {}

function LString:new(value)
  local lString = {
    data = value
  }
  setmetatable(lString, self)
  self.__index = self

  return lString
end

function LString:getValue()
  return self.data
end

function LString:toString()
  return self.data
end

