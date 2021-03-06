FakeCode = {}

function FakeCode:new(valueFromToString)
  assert(valueFromToString)

  local fakeCode = {
    valueFromToString = valueFromToString,
  }
  setmetatable(fakeCode, self)
  self.__index = self

  return fakeCode
end

function FakeCode:send(context)
  context:receiveFakeCode(self)
end

function FakeCode:toString()
  return self.valueFromToString
end

