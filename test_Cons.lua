require "Cons"
require "test_utilities"

-- Begin unit tests

local test_Cons = {}

function test_Cons.getCar()
  local carIs1 = Cons:new(1, 2)
  assert(1 == carIs1:getCar())
end

function test_Cons.setCar()
  local carIs3 = Cons:new(1, 2)
  carIs3:setCar(3)
  assert(3 == carIs3:getCar())
end

function test_Cons.getCdr()
  local cdrIs2 = Cons:new(1, 2)
  assert(2 == cdrIs2:getCdr())
end

function test_Cons.setCdr()
  local cdrIs4 = Cons:new(1, 2)
  carIs4:setCdr(4)
  assert(4 == cdrIs4:getCdr())
end

-- End unit tests

function testCons()
  runTests("Cons", test_Cons)
end

