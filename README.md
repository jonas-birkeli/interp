# Interp - Concatenative Stack-Based Programming Language

Interp is a simple concatenative, stack-based programming language inspired by Forth and Joy. Programs are sequences of values and operations that manipulate a stack.

## Table of Contents
- [Language Overview](#language-overview)
- [Data Types](#data-types)
- [Program Execution Model](#program-execution-model)
- [Interrupt System](#interrupt-system)
- [Language Reference](#language-reference)
- [Type Conversion Rules](#type-conversion-rules)
- [Limitations](#limitations)
- [Examples](#examples)

## Language Overview

Interp uses postfix notation (Reverse Polish Notation) where operations come after their operands:
```
5 3 +     # Adds 5 and 3, resulting in 8
10 2 /    # Divides 10 by 2, resulting in 5.0
```

Programs must end with exactly one value on the stack. Programs finishing with zero or multiple values are considered errors.

## Data Types

### Integers
- Syntax: `42`, `-17`, `0`
- Arbitrary precision integers

### Floats
- Syntax: `3.14`, `-0.5`, `1.0`
- Double precision floating point

### Booleans
- Syntax: `True`, `False`
- Note: Case-sensitive, `true` is not valid

### Strings
- Syntax: `" hello world "`, `" "`
- Strings preserve internal spaces
- Length counts characters, not including quotes

### Lists
- Syntax: `[ 1 2 3 ]`, `[ ]`, `[ 1 " text " [ ] ]`
- Heterogeneous - can contain mixed types
- Can be nested

### Quotations (Code Blocks)
- Syntax: `{ 1 + }`, `{ dup * }`
- First-class code blocks
- Can be stored, passed around, and executed

### Symbols
- Syntax: `age`, `myFunction`, `x`
- Unbound symbols evaluate to themselves
- Bound symbols evaluate to their values

## Program Execution Model

### Stack Operations
The stack is the central data structure. All operations consume values from the stack and push results back.

```
Stack (top on right): []
Execute: 5         -> [5]
Execute: 3         -> [5, 3]
Execute: +         -> [8]
```

### Control Flow
Control flow operations modify the program itself rather than using jumps:

```
True if { 20 } { 30 }
# Transforms to: 20
```

## Interrupt System

The interpreter uses interrupts for I/O operations to maintain purity in the core execution:

### Print Interrupt
- Created by `print` operation
- Execution pauses, message is displayed
- Execution continues after interrupt is handled

### Read Interrupt
- Created by `read` operation
- Execution pauses, input is requested
- Input is pushed to stack as string
- Execution continues

Example flow:
```
" Enter number: " print read parseInteger 5 +
1. Print creates interrupt with message
2. Message displayed
3. Read creates interrupt
4. User inputs value
5. Value pushed as string
6. parseInteger converts to number
7. Addition performed
```

## Language Reference

### Stack Operations

#### `dup` (duplicate)
- Stack effect: `( x -- x x )`
- Duplicates the top value
```
5 dup    # Stack: [5, 5]
```

#### `swap`
- Stack effect: `( x y -- y x )`
- Swaps top two values
```
1 2 swap    # Stack: [2, 1]
```

#### `pop`
- Stack effect: `( x -- )`
- Removes top value
```
1 2 3 pop    # Stack: [1, 2]
```

### Arithmetic Operations

#### `+` (addition)
- Stack effect: `( x y -- x+y )`
- Type coercion: Int + Float → Float
```
5 3 +       # Result: 8 (Int)
5 3.0 +     # Result: 8.0 (Float)
```

#### `-` (subtraction)
- Stack effect: `( x y -- x-y )`
- Left argument minus right argument
```
10 3 -      # Result: 7
```

#### `*` (multiplication)
- Stack effect: `( x y -- x*y )`
```
4 5 *       # Result: 20
```

#### `/` (float division)
- Stack effect: `( x y -- x/y )`
- Always returns float
```
10 3 /      # Result: 3.333...
```

#### `div` (integer division)
- Stack effect: `( x y -- x÷y )`
- Returns integer (truncated)
```
10 3 div    # Result: 3
```

### Comparison Operations

#### `<`, `>`, `==`
- Stack effect: `( x y -- bool )`
- Numeric comparison with type coercion
```
5 10 <      # Result: True
5 5.0 ==    # Result: True (int/float equality)
```

### Logical Operations

#### `&&` (and)
- Stack effect: `( bool bool -- bool )`
- Both arguments must be booleans
```
True False &&    # Result: False
```

#### `||` (or)
- Stack effect: `( bool bool -- bool )`
```
True False ||    # Result: True
```

#### `not`
- Stack effect: `( bool -- bool )` or `( int -- int )`
- Negates boolean or integer
```
True not     # Result: False
5 not        # Result: -5
```

### String Operations

#### `parseInteger`
- Stack effect: `( string -- int )`
- Converts string to integer
```
" 42 " parseInteger    # Result: 42
```

#### `parseFloat`
- Stack effect: `( string -- float )`
```
" 3.14 " parseFloat    # Result: 3.14
```

#### `words`
- Stack effect: `( string -- list )`
- Splits string into list of words
```
" hello world " words    # Result: ["hello", "world"]
```

### List Operations

#### `head`
- Stack effect: `( list -- item )`
- Returns first element
```
[ 1 2 3 ] head    # Result: 1
```

#### `tail`
- Stack effect: `( list -- list )`
- Returns all but first element
```
[ 1 2 3 ] tail    # Result: [2, 3]
```

#### `cons`
- Stack effect: `( item list -- list )`
- Prepends item to list
```
1 [ 2 3 ] cons    # Result: [1, 2, 3]
```

#### `append`
- Stack effect: `( list1 list2 -- list3 )`
- Concatenates lists
```
[ 1 2 ] [ 3 4 ] append    # Result: [1, 2, 3, 4]
```

#### `empty`
- Stack effect: `( list -- bool )`
- Tests if list is empty
```
[ ] empty         # Result: True
[ 1 ] empty       # Result: False
```

#### `length`
- Stack effect: `( list/string/quotation -- int )`
- Returns length
```
[ 1 2 3 ] length      # Result: 3
" hello " length      # Result: 5 (not 7 - quotes not counted)
{ 1 + } length        # Result: 2
```

### Control Flow

#### `if`
- Stack effect: `( bool -- )`
- Syntax: `condition if { then-block } { else-block }`
- Executes then-block if true, else-block if false
```
5 3 > if { " bigger " } { " smaller " } print
```

#### `times`
- Stack effect: `( n -- )`
- Repeats block n times
```
3 times { " Hi " print }    # Prints "Hi" three times
```

#### `loop`
- Stack effect: `( -- )`
- Syntax: `loop { break-condition } { body }`
- Executes body until break-condition is true
```
1 loop { dup 5 > } { dup print 1 + }
# Prints: 1 2 3 4 5
```

### Higher-Order Functions

#### `map`
- Stack effect: `( list -- list )`
- Applies block to each element
```
[ 1 2 3 ] map { 2 * }    # Result: [2, 4, 6]
```

#### `each`
- Stack effect: `( list -- )`
- Executes block for each element
```
[ 1 2 3 ] each { print }    # Prints each number
```

#### `foldl`
- Stack effect: `( list initial -- result )`
- Left fold with accumulator
```
[ 1 2 3 ] 0 foldl { + }    # Result: 6
```

### Variable Definition

#### `:=` (assignment)
- Stack effect: `( value symbol -- )`
- Binds value to symbol
```
42 age :=           # age now contains 42
[ 1 2 ] data :=     # data now contains [1, 2]
```

#### `fun` (function definition)
- Stack effect: `( quotation symbol -- )`
- Defines a named function
```
{ 2 * } double fun
5 double            # Result: 10
```

### I/O Operations

#### `print`
- Stack effect: `( value -- )`
- Displays value with newline
```
" Hello World " print
42 print
```

#### `read`
- Stack effect: `( -- string )`
- Reads line from input
```
" Enter name: " print read
```

### Quotation Operations

#### `exec`
- Stack effect: `( quotation -- )`
- Executes quotation
```
{ 5 3 + } exec    # Result: 8
```

## Type Conversion Rules

### Arithmetic Operations
- Int + Int → Int
- Int + Float → Float
- Float + Float → Float
- Same rules for -, *, /

### Division Special Cases
- `/` always returns Float
- `div` always returns Int (truncated)

### Comparison Operations
- Can compare Int with Float
- `5 == 5.0` returns `True`

### Not Operation
- On Bool: logical negation
- On Int: arithmetic negation

## Limitations

### Type Restrictions
- Booleans cannot be added/subtracted/multiplied
- Strings have no concatenation operator (use lists)
- Lists are not directly comparable with == (element-wise only)

### String Handling
- No built-in string concatenation
- To join strings, convert to list: `" hello " " world " [ ] 2 cons cons`
- No string slicing or character access

### No String Escaping
- No escape sequences in strings
- Cannot include quotes within strings

### Program Structure
- Must end with single value on stack
- No module system
- No exception handling beyond error termination

### Missing Operations
- No modulo operator
- No bitwise operations
- No string interpolation
- No regular expressions

## Examples

### Factorial
```
factorial { 
    dup 1 == 
    if { } 
       { dup 1 - factorial * }
} fun

5 factorial    # Result: 120
```

### FizzBuzz
```
fizzbuzz {
    dup 15 div dup * 15 == if { pop " FizzBuzz " print } {
    dup 3 div dup * 3 == if { pop " Fizz " print } {
    dup 5 div dup * 5 == if { pop " Buzz " print } {
    print
    }}}
} fun

15 times { i 1 + fizzbuzz }
```

### List Processing
```
# Sum a list
sum { 0 foldl { + } } fun
[ 1 2 3 4 5 ] sum    # Result: 15

# Filter even numbers
filterEven { 
    [ ] swap each { 
        dup 2 div 2 * == if { swap cons } { pop }
    }
} fun
```

### Interactive Program
```
" Enter your age: " print
read parseInteger
dup 18 >= if 
    { " You can vote! " print }
    { " Too young to vote " print }
```

### Error Example
```
# This will error - program ends with multiple values
1 2 3
# Error: ProgramFinishedWithMultipleValues [1, 2, 3]

# This will error - program ends with no values
1 pop
# Error: ProgramFinishedWithNoValues
```

## Program Structure

1. Programs are executed sequentially
2. Control flow modifies the program stream
3. I/O uses interrupts to maintain execution purity
4. Must end with exactly one value
5. All operations are postfix (RPN)