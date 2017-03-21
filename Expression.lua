Expression = {}

function Expression:new(value)
  local expression = {
    data = {}
  }
  setmetatable(expression, self)
  self.__index = self

  return expression
end

function Expression:push(value)
  assert(value)

  table.insert(self.data, value)
end

function Expression:get(index)
  assert(index >= 1 and index <= #self.data)

  return self.data[index]
end

function Expression:getLength()
  return #self.data
end

function Expression:represents()
  return "expression"
end

function Expression:evaluate(context)
  assert(self:getLength() > 0)
  context:evaluateExpression()
  
end

function Expression:toString()
  if #self.data == 0 then
    return "()"
  end

  local result = ""
  for _, current in ipairs(self.data) do
    result = result .. " " .. current:toString()
  end
  return "(" .. string.sub(result, 2) .. ")"
end

