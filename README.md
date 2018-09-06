# ExtLua (Abandoned)

ExtLua is an extension of Lua designed to enhance game development. Right now, I'm mostly adding syntactic extensions like arrow functions and class syntax, which puts ExtLua in the same class as languages like MoonScript. My eventual goal is to add things like static checking of inheritance and module exports, and maybe even some form of static typing.

Unlike MoonScript, I intend to stick to lua-like syntax, using keywords like `do` and `end`.

## Extensions

### Assign Operators
This extension adds operators like `+=` that will perform a binary operation and assign in one step. Note that these are statements, and thus don't return a value.

```lua
x += 1
x, y -= 2, 3
```

### Bind Operator
This is based off of the [ES7 bind operator](https://github.com/tc39/proposal-bind-operator). The `::` operator essentially takes the left-hand expression and prepends it to the argument list of the function on the right. The purpose of this is to let you 'extend' objects in a way that doesn't pollute them globally. It also lets you extend primitives which normally are not extensible.

```lua
local map = function(table, func)
  local new_table = {}
  for k, v in table::pairs() do new_table[k] = func(v, k) end
  return new_table
end

({1, 2, 3, 4})::map(x -> x ^ 2)
```

## Planned Extensions

### Self Sugar
```lua
local counter = {i = 0}
function counter:increment()
  $i += 1 -- $i corresponds to self.i
end
function counter:incrementAndPrint()
  print(.i)
  @increment() -- @increment() expands to self:increment
end
```

### Arrow Functions
This is a shortened syntax for anonymous functions, making a lot of patterns in Lua much more concise.
```lua
local identity = x -> x
local multiarg = (x, y, ...) -> x + y
local noarg = () -> 3
local block = x -> do print(x) end

-- The fat arrow (=>) can be used to automatically bind self to the first argument.
local fatarrow = (x) => $value + x
```

### Default Arguments
```lua
function f(a = 3)
  print(a)
end
```

### Type Annotations
```lua
local a: string = "Test"
function square(n: number) return n*n end
```

### Doc Comments
A special doc comment syntax that can appear before exports and class methods. Doc comments are retrievable at runtime.

```lua
<<
This function takes in a number and returns whether or not that number
is prime.
>>
function isprime(n)
end
```

### Vector Literal
```lua
local pos2d = <0, 2> -- <0, 2> expands to vec2(0, 2), which is application defined
local pos3d = <1, 1, 1> -- Expands to vec3(1, 1, 1)
```

### Import Syntax
```lua
import map, each as forEach, filter from './tableutils' -- Import specific exports - can also rename somthing
import * from './tableutils' -- Import all exports into current scope
import * as TU from './tableutils' -- Import all the exports into the table TU
```

### Export Syntax
```lua
export exportedvar = {}
export function exportedfunc() end
```
