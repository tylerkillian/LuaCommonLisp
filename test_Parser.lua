require "Parser"

local function Parser_construct()
  local defaultParser = Parser:new()
  assert("scan" == defaultParser:toString())
end

function testParser()
  Parser_construct()
end

