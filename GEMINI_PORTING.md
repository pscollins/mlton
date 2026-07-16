# Porting Pitfalls and Guidelines from MPL to MLton

This document logs key pitfalls encountered and design decisions made when backporting compiler features and tests from `code/mpl` to `code/mlton`.

## 1. Handling `__inline_never__` and `-inline 0`

### Pitfall
The `code/mpl` codebase has parser and lexer support for a custom `__inline_never__` keyword attribute (e.g., `fun __inline_never__ foo ...`). Standard `mlton` does not recognize this token, and the lexer/parser will fail with an error like:
```
Function clause with illegal name: _.
```
This happens because standard MLton treats the double underscores as wildcard pattern matching variables.

### Workaround
1. **Remove `__inline_never__`** from function definitions in the ported tests.
2. Inlining is already disabled globally in standard MLton LIT tests via the `-inline 0` option, which is automatically appended by `lit-tests/tools/mlton-compile.sh`.

---

## 2. Preventing Single-Call Inlining of Test Functions

### Pitfall
Even when `-inline 0` is set, MLton's XML simplifier or early optimization passes may inline functions that are called only once (like a `runTest` function). 

When a test function wrapping a direct tail call (e.g., `doAdd (res1, res2)`) is inlined into the top level, it becomes wrapped in the program's top-level exception handler. This wraps the call with a handler (e.g. `handle _ => L_5`), converting the tail call into a non-tail call and causing `tail_only` policy assertions to fail.

### Workaround
To prevent the compiler from inlining a helper test function (like `runTest`) without using `__inline_never__`, ensure the function is called **more than once**. For example, call it twice and combine the results:
```sml
val _ = print (Int.toString (Word32.toInt (Word32.+ (runTest (), runTest ()))))
```
This keeps the function from being inlined, thereby preserving the internal tail call structure.

---

## 3. Differences in compiler IR AST (`Transfer.Call` Constructor)

### Pitfall
In `code/mpl`, the SSA `Transfer.Call` constructor includes an `inline` field:
```sml
datatype t = Call of {args: Var.t vector, func: Func.t, inline: InlineAttr.t, return: Return.t}
```
In `code/mlton`, the `Transfer.Call` constructor does not have an `inline` field:
```sml
datatype t = Call of {args: Var.t vector, func: Func.t, return: Return.t}
```

### Workaround
When copying pattern matches on `Transfer.Call` from `mpl` code, make sure to strip out the `inline` field.
