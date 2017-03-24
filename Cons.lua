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

function Cons:getCar()
  return self.car
end

function Cons:setCar(car)
  self.car = car
end

function Cons:getCdr()
  return self.cdr
end

function Cons:setCdr(cdr)
  self.car = cdr
end
