Environment = {}

function Environment_format(theString)
  local formattedString = string.gsub(theString, "~%%", "\n")
  io.write(formattedString)
end

function Environment:new()
  local environment = {
    global = {},
  }
  setmetatable(environment, self)
  self.__index = self

  return environment
end

function Environment:eval(code)
  if type(code) == "string" then
    return code
  end
  if code[1] == "format" then
    Environment_format(self:eval(code[3]))
  end
  if code[1] == "concatenate" then
    return string.sub(code[3], 2, -2) .. string.sub(code[4], 2, -2)
  end
  if code[1] == "let" then
    local newLookup = {global, makeBindings(code[2])}
  end
end

