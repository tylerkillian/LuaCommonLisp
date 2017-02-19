require "Parser"

local function Parser_construct()
  local defaultParser = Parser:new()
  assert("scan" == defaultParser:toString())
end

function testCodeTypes()
  Parser_construct()
end

