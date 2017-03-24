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
  self.cdr = cdr
end

function Cons:toString()
  local result = ""

  if type(self.car) == "table" then
    result = result .. self.car:toString()
  else
    result = result .. self.car
  end

  if type(self.cdr) == "table" then
    result = result .. " " .. self.cdr:toString()
  else
    result = result .. " " .. self.cdr
  end

  return result
end

