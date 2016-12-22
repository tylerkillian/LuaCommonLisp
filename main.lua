require "Parser"
require "Environment"

function codeToString(code)
  if type(code) == "string" then
    return code
  end

  local theString = "("
  local first = true
  for _, value in ipairs(code) do
    if not first then
      theString = theString .. " "
    end
    theString = theString .. codeToString(value)

    first = false
  end
  theString = theString .. ")"

  return theString
end

local input = io.open("allTests.cl")
local parser = Parser:new()
local environment = Environment:new()
local nextCharacter = input:read(1)
while nextCharacter do
  code = parser:readCharacter(nextCharacter)
  if code then
    print("> " .. codeToString(code))
    environment:eval(code)
  end
  nextCharacter = input:read(1)
end
