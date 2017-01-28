local identity = function(x)
  return x
end
local multiarg = function(x, y, ...)
  return x + y
end
local noarg = function()
  return 3
end
local block = function(x)
  print(x)
end
local fatarrow = function(self, x)
  return self.value + x
end
