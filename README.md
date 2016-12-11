## Extensions

### Assign Operators
```lua
x += 1
x, y -= 2, 3
```

### Arrow Functions
```lua
local identity = x -> x
local multiarg = (x, y, ...) -> x + y
local noarg = () -> 3
local block = x -> do print(x) end

-- The fat arrow (=>) can be used to automatically bind self to the first argument.
local fatarrow = (x) => self.value + x
```

## Bind Operator
```lua
local map = function(table, func)
  local new_table = {}
  for k, v in table::pairs() do new_table[k] = func(v, k) end
  return new_table
end

{1, 2, 3, 4}::map(x -> x ^ 2)
```

## Self Sugar
```lua
local counter = {i = 0}
function counter:increment()
  .i += 1 -- .i corresponds to self.i
end
function counter:incrementAndPrint()
  print(.i)
  :increment() -- :increment() expands to self:increment
end

```

## Vector Literal
```lua
local pos2d = <0, 2> -- <0, 2> expands to vec2(0, 2), which is application defined
```

## Import Syntax
```lua
import map, each as forEach, filter from './tableutils' -- Import specific exports - can also rename somthing
import * from './tableutils' -- Import all exports into current scope
import * as TU from './tableutils' -- Import all the exports into the table TU
```

## Export Syntax
```lua
export exportedvar = {}
export function exportedfunc() end
```
