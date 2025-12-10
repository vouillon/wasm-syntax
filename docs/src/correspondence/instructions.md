# Instructions

Wax instructions are expression-oriented.

## Numeric Instructions

Binary and unary operations use standard mathematical operators. Signedness is often explicit in the operator.

| Wasm | Wax |
|------|-----|
| `i32.add` / `i64.add` | `+` |
| `i32.sub` / `i64.sub` | `-` |
| `i32.mul` / `i64.mul` | `*` |
| `i32.div_s` / `i64.div_s` | `/s` |
| `i32.div_u` / `i64.div_u` | `/u` |
| `i32.rem_s` / `i64.rem_s` | `%s` |
| `i32.rem_u` / `i64.rem_u` | `%u` |
| `i32.and` / `i64.and` | `&` |
| `i32.or` / `i64.or` | `\|` |
| `i32.xor` / `i64.xor` | `^` |
| `i32.shl` / `i64.shl` | `<<` |
| `i32.shr_s` / `i64.shr_s` | `>>s` |
| `i32.shr_u` / `i64.shr_u` | `>>u` |
| `i32.eqz` / `i64.eqz` | `!x` (logical not) |

Floating point operations:
| Wasm | Wax |
|------|-----|
| `f32.add` / `f64.add` | `+` |
| `f32.sub` / `f64.sub` | `-` |
| `f32.mul` / `f64.mul` | `*` |
| `f32.div` / `f64.div` | `/` |
| `i32.clz` ... `i64.popcnt` | `val.clz`, `val.ctz`, `val.popcnt` |
| `f32.abs` ... `f64.sqrt` | `val.abs`, `val.neg`, `val.ceil`, `val.floor`, `val.trunc`, `val.nearest`, `val.sqrt` |
| `i32.rotl` ... `i64.rotr` | `rotl(v1, v2)`, `rotr(v1, v2)` |
| `f32.min` ... `f64.copysign` | `min(v1, v2)`, `max(v1, v2)`, `copysign(v1, v2)` |



### Conversions

| Wasm | Wax |
|---|---|
| `i32.wrap_i64` | `val as i32` |
| `i64.extend_i32_s` | `val as i64_s` |
| `i64.extend_i32_u` | `val as i64_u` |
| `i32.trunc_f32_s` | `val as i32_s` |
| `f32.convert_i32_s` | `val as f32_s` |
| `f32.demote_f64` | `val as f32` |
| `f64.promote_f32` | `val as f64` |
| `i32.reinterpret_f32` | `val.to_bits` |
| `f32.reinterpret_i32` | `val.from_bits` |

## Comparison

| Wasm | Wax |
|------|-----|
| `eq` | `==` |
| `ne` | `!=` |
| `lt_s` / `lt` | `<s` / `<` |
| `lt_u` | `<u` |
| `le_s` / `le` | `<=s` / `<=` |
| `le_u` | `<=u` |
| `gt_s` / `gt` | `>s` / `>` |
| `gt_u` | `>u` |
| `ge_s` / `ge` | `>=s` / `>=` |
| `ge_u` | `>=u` |

## Variable Instructions

| Wasm | Wax |
|------|-----|
| `local.get $x` | `x` |
| `local.set $x` | `x = val` |
| `local.tee $x` | `x := val` |
| `global.get $g` | `g` |
| `global.set $g` | `g = val` |

## Control Instructions

| Wasm | Wax |
|------|-----|
| `block` | `do { ... }` or `{ ... }` |
| `loop` | `loop { ... }` |
| `if ... else ...` | `if cond { ... } else { ... }` |
| `br $l` | `br 'l` |
| `br_if $l` | `br_if 'l cond` |
| `br_table $l* $ld` | `br_table ['l* else 'ld] val` |
| `br_on_null $l` | `br_on_null 'l val` |
| `br_on_non_null $l` | `br_on_non_null 'l val` |
| `br_on_cast $l $t1 $t2` | `br_on_cast 'l t2 val` |
| `br_on_cast_fail $l $t1 $t2` | `br_on_cast_fail 'l t2 val` |
| `return` | `return val` |
| `call $f` | `f(args)` |
| `call_ref $t` | `(val as &?t)(args)` |
| `return_call $f` | `become f(args)` |
| `return_call_ref $t` | `become (val as &?t)(args)` |
| `unreachable` | `unreachable` |
| `nop` | `nop` |
| `select` | `cond ? v1 : v2` |

### Block Labels and Types

Blocks, loops, and ifs can be labeled and typed.
Labels are identifiers starting with `'` followed by a colon.

```wax
'my_block: do { ... }
'my_loop: loop { ... }
```

Block types (signatures) can be specified using `(param) -> result` syntax or a single value type.
If no type is specified, the `do` keyword is optional.

```wax
do (i32) -> i32 { ... }
do i32 { ... }
{ ... }                  ;; equivalent to do { ... }
if cond => (i32) -> i32 { ... } else { ... }
```

## Reference Instructions

| Wasm | Wax |
|------|-----|
| `ref.null` | `null` |
| `ref.is_null` | `!val` |
| `ref.as_non_null` | `val!` |
| `ref.i31` | `val as &i31` |
| `i31.get_s` | `val as i32_s` |
| `i31.get_u` | `val as i32_u` |
| `ref.cast` | `val as &type` |
| `ref.test` | `val is &type` |

## Aggregate Instructions

| Wasm | Wax |
|------|-----|
| `struct.new $t` | `{t| field: val, ... }` |
| `struct.new_default $t` | `{t| .. }` |
| `struct.get $t $f` | `val.field` |
| `struct.set $t $f` | `val.field = new_val` |
| `array.new $t` | `[t| val; len]` |
| `array.new_default $t` | `[t| ..; len]` |
| `array.new_fixed $t` | `[t| val, ...]` |
| `array.get $t` | `arr[idx]` |
| `array.get_s $t` | `arr[idx] as i32_s` |
| `array.get_u $t` | `arr[idx] as i32_u` |
| `array.set $t` | `arr[idx] = val` |
| `array.len` | `arr.length` |



## Exception Instructions

| Wasm | Wax |
|------|-----|
| `throw $tag` | `throw tag(args)` |
| `throw_ref` | `throw_ref` |
| `try_table ... catch $tag $l ...` | `try { ... } catch [ tag -> 'l, ... ]` |
| `try_table ... catch_ref $tag $l ...` | `try { ... } catch [ tag & -> 'l, ... ]` |
| `try_table ... catch_all $l ...` | `try { ... } catch [ _ -> 'l, ... ]` |
| `try_table ... catch_all_ref $l ...` | `try { ... } catch [ _ & -> 'l, ... ]` |
| `try ... catch $tag ...` | `try { ... } catch { tag => { ... } ... }` |
| `try ... catch_all ...` | `try { ... } catch { _ => { ... } }` |

*(Note: `try` corresponds to the older instruction, `try_table` to the newer one. Wax supports both syntaxes (in AST/Output).)*
