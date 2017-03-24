Cons = {}

function Cons:new(car, cdr)
  local cons = {
    car = car,
    cdr = cdr,
  }
  setmetatable(cons, self)
  self.__index = self

  return cons
end

