StrawLang has the following data types:

- str
- int
- float
- bool

- func
- struct
- enum

# Strings

String are created with an opening and closing `"`. String concatenation is supported with the '+' operator.

Formatted strings are created with surrounding backticks. Values are placed in `{}`s.

Currently only string variables are supported, no expressions or other data types.

example:
```
str my_name = "Dylan"
str greeting = `Hello, {my_name}. How are you?`
```

# Numbers

Operations performed on an integer in conjunction with a float result in a float

Example: 2 + 3.1 = 5.1

# Functions

You can declare functions as statements:

```
func foo(int bar) -> int {
    return bar + 1
}
```

or as expressions:

```
fn(int) -> int foo = func foo(int bar) -> int {
// body
}
```

since functions are values, you can return functions:

```
func make_counter() -> fn() -> int {
    int count = 0

    return func () -> int {
        count = count + 1
        return count
    }
}

fn() -> int counter = make_counter()
```

Closures are supported

# Structs

Structs types can be defined:

```
struct Person {
    int age
    str name
}
```

and then instantiated:

```
Person me = {age = 10, name = "Dylan"}
```

Structs can be nested and can contain functions as values.

# Enums

Enums are defined:

```
enum Direction {
    North,
    South,
    East,
    West
}
```

and used:

```
Direction wind_direction = Direction.North
// or using let
let wind_direction = Direction.South
```


# Arrays

```
int[] nums = [1, 2, 3, 4, 5]
print(nums[0])
nums[0] = nums[0] + 1
```

# Let keyword

Data-type omission is allowed in favor of `let` if the type can be statically inferred.
Examples:

```
let my_num = 5

let myfunc = fn(int a) -> int {}

let my_value = foo()
/* where foo is a valid function */
```

# Looping Structure

```
while i < 10 {
    // dosomething
    i++
}

for (int i = 0; i < 10; i++) {
    // dosomething else
}
```

# Control Flow

```
if num < 100 {
    print ("sub 100")
} else if num == 100 {
    print("exactly 100")
} else {
    print("greater than 100")
}
```

# Logical operators

- &&
- ||
- ==
- >
- >=
- <
- <=
- !=

# Standard Library

Currently the following functions are part of the standard library:

- print (only accepts strings)
- input (takes string input for prompt)
- int_to_str
- float_to_str
- bool_to_str
