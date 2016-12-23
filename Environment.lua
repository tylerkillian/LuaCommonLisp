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

local function appendtoTable(theTable, theEntry)
  local newTable = {}

  for _, v in ipairs(theTable) do
    table.insert(newTable, v)
  end
  table.insert(newTable,theEntry)

  return newTable
end

local function makeBindings(assignments, lookupTable)
  local bindings = {}
  return bindings
end

function Environment:eval(code, lookupTable)
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
    local newLookup = appendToTable(lookupTable or {global}, makeBindings(code[2], lookupTable or {global}))
    for idx, subcode in ipairs(code) do
      self:eval(subcode, newLookup)
    end
  end
end

