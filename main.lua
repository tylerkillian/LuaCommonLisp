require "Parser"
require "Environment"

local parser = Parser:new()
local codeString = [[(format t (concatenate 'string "hel" "lo~%"))]]
local code
local environment = Environment:new()
for codeStringIdx = 1, string.len(codeString) do
  local nextCharacter = string.sub(codeString, codeStringIdx, codeStringIdx)
  code = parser:readCharacter(nextCharacter)
  if code then
    environment:eval(code)
  end
end
assert(type(code) == "table")
assert(#code == 3)
assert(code[1] == "format")
assert(code[2] == "t")
assert(type(code[3]) == "table")
assert(#code[3] == 4)
assert(code[3][1] == "concatenate")
assert(code[3][2] == "'string")
assert(code[3][3] == "\"hel\"")
assert(code[3][4] == "\"lo~%\"")

function codeToString(code)
  if type(code) == "string" then
    return code
  end

  local theString = "("
  local first = true
  for _, value in ipairs(code) do
    if first then
      theString = theString .. " "
    end
    theString = theString .. " " .. codeToString(value)
  end
  theString = theString .. ")"

  return theString
end

local input = io.open("allTests.cl")
local parser = Parser:new()
local environment = Environment:new()
local nextCharacter = input:read(1)
while nextCharacter do
print(nextCharacter)
  code = parser:readCharacter(nextCharacter)
  if code then
    print(codeToString(code))
    environment:eval(code)
  end
  nextCharacter = input:read(1)
end
