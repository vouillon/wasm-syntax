# Module Fields

Wax modules are defined by a list of fields.

## Functions

Functions are defined using the `fn` keyword.

```wax
fn add(x: i32, y: i32) -> i32 {
    return x + y
}
```

## Globals

Globals are defined using `const` (immutable) or `let` (mutable).

```wax
const g: i32 = 42
let mut_g: i32 = 0
```

## Types

Types are defined at the top level using `type` or `rec { ... }`.

```wax
type ops = { a: i32 }
```

## Tags

Tags for exceptions are defined using `tag`.

```wax
tag my_exception(num: i32)
```

## Tables and Memories

**Note**: Wax currently does not define explicit syntax for `table` or `memory` definitions within the language itself. They are typically handled during conversion or implicit in the environment. The conversion logic drops them or handles them implicitly.

## Attributes (Imports/Exports)

Imports and exports are handled via attributes on module fields. Attributes use the syntax `#[name = value]`.

### Exports

```wax
#[export = "add"]
fn add(...) { ... }
```

### Imports

Imports can be applied to functions, globals, and tags. They typically take a tuple of strings `("module", "name")`.

```wax
#[import = ("env", "log")]
fn log(msg: &string)

#[import = ("env", "g")] 
const g: i32

#[import = ("env", "error")]
tag error(code: i32)
```
