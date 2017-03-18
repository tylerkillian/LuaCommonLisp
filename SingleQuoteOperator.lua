SingleQuoteOperator = {}

function SingleQuoteOperator:new(value)
  assert(value)

  local operator = {
    data = value
  }
  setmetatable(operator, self)
  self.__index = self

  return operator
end

function SingleQuoteOperator:getArgument()
  return self.data
end

function SingleQuoteOperator:toString()
  return "'" .. self.data:toString
end

