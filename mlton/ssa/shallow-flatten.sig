signature SHALLOW_FLATTEN =
sig
   (* Exports:
        val transform: Program.t -> Program.t

     which iteratively runs `flattenOnce` until convergence. The behavior of
     this call is controlled by the following flags:

       -shallow-flatten-max-iters=N: limit the number of iterations to N

       -shallow-flatten-policy=maxWidth:$N sets the policy to `MaxWidth(n)`
    *)
   include SSA_TRANSFORM
   structure FlattenUtil: FLATTEN_UTIL
   type funcsMap = FlattenUtil.funcsMap


   (* Abstract interface for applying the flattening transformation *)
   type flattener = {
      (* Transformation applied to all `Type.t`s in the program that *do not*
         appear in a `Statement.t`, specifically:
           * Datatype type constructor type arguments
           * Function `args`, `returns` and `raises`
           * Block arguments
       *)
      updateType: Type.t -> Type.t,
      (* Transformation applied to all `Statement.t`s, specifically:
           * Global declarations
           * Block bodies
       *)
      updateStatements: Statement.t vector -> Statement.t vector
   }

   (* Applies `flattener` to the specified program in source order. *)
   val flattenProgram: flattener -> Program.t -> Program.t


   (* If the provided `Type.t` is an array or vector of tuples, returns the
   corresponding tuple of arrays or vectors, i.e.:

     ('a * 'b) array -> SOME ('a array * 'b array)
     ('a * 'b) vector -> SOME ('a vector * 'b vector)

     Otherwise, returns NONE.
    *)
   val maybeFlattenType: Type.t -> Type.t option


   (* What array/vector types should be flattened? *)
   datatype flattenPolicy =
        (* Flatten all tuple array types over <= `MaxWidth` tuple members *)
        MaxWidth of int

   (* Update an entire nested subject to `flattenPolicy`

      Unlike `maybeFlattenType`, this function traverses through non-flattenable
      types and transforms any flattenable nodes in the provided `Type.t`, i.e.

        ('a * 'b) array -> 'a array * 'b array
        (('a * b) array) ref -> (('a array) * ('b array)) ref


      Additionally, this function runs until convergence, i.e. we have:

        (('a * b) array) array
          -> (('a array) * ('b array)) array
          -> ('a array array) * ('b array array)
   *)
   val deepFlattenTypeForPolicy: flattenPolicy -> Type.t -> Type.t

   (* Returns `true` if `Statement.t` requires the `maybeFlattenStatement`
      transformation (below) under `policy`, `false` otherwise.

      Specifically:

        * Non-`PrimApp`: false
        * Non-`Vector_*` or `Array_*` `PrimApp`: false

        * Most `Vector_*` or `Array_*` `PrimApp`s: true if the type argument is
          a tuple type that matches `policy`, `false` otheriwse.

          - Exception: `Array_uninitIsNop` has an `a' array` type argument, and
            so we check the policy against `a'`
    *)
   val doesPolicyFlattenStatement: flattenPolicy -> Statement.t -> bool

   (* Joinly flattens statements and the types they contain.

      For any `PrimApp` statements that require flattening under `policy`,
      transforms them according to the rules of `maybeFlattenStatements`
      (below). Then applies the `deepFlattenTypeForPolicy` on all resulting
      `Statement.t`s (perhaps just the original one.

       Examples:

         * Flattenable statement, no type transformation required
         arr: ('a * b) array = Array_alloc['a * b](n]
         -->
         arr_a: 'a array = Array_alloc['a](n)
         arr_b: 'b array = Array_alloc['b](n)
         arr: ('a array) * (b' array) = tuple (arr_a, arr_b)


         * Non-flattenable statement, with type transformation required
         arr: ('a * b) array array = Array_alloc[('a * b') array](n]
         -->
         arr: ('a array * b array) array = Array_alloc['a array * b' array](n]

    *)
   val deepFlattenStatementsForPolicy: flattenPolicy -> Statement.t -> Statement.t vector

   (* Describes a flattening decision for a nested type *)
   datatype conDecision =
            (* Pass the existing type through unflattened and recurse *)
            PreserveNode of conDecision vector
            (* Apply `maybeFlattenType` to this type and recurse *)
            | FlattenNode of conDecision vector

   (* Calculates the flattening decision for this type according to the supplied
      policy. *)
   val getConDecisionForPolicy:
       flattenPolicy -> Type.t -> conDecision

   (* Recursively applies `conDecision` to the supplied type.

    If the decision is invalid (i.e. `maybeFlattenType` returns NONE for a
   `flattenNode` layer, raises InvalidConFlattening.
   *)
   exception InvalidConFlattening
   val applyConDecision: conDecision * Type.t ->
                         Type.t

   (* Tracks flattening decisions for variables, arguments, and constructors. *)
   type flattenedVars
   val newFlattenedVars: unit -> flattenedVars
   val destroyFlattenedVars: flattenedVars -> unit
   (* Marks the provided `Var.t` for flattening. It is only valid to call this
   function once  on a particular `(fv, v)` pair *)
   val markForFlatten: flattenedVars * Var.t -> unit
   (* If the provided `Var.t` was previously marked for flattening (above),
   returns true. Otherwise, returns false. *)
   val isMarkedForFlatten: flattenedVars * Var.t -> bool
   (* Like `markFlatten`, but for `Con.t` *)
   val setConFlatteningDecision: flattenedVars * Con.t * conDecision vector -> unit
   (* Like `isMarkedForFlatten`, but for `Con.t` *)
   val getConFlatteningDecision: flattenedVars * Con.t -> conDecision vector
   (* Like `markFlatten`, but for function/block arguments. The user should
   always have the pair (Var.t * Type.t) available, but there's no need to pass
   the type here. *)
   val setArgFlatteningDecision: flattenedVars * Var.t * conDecision -> unit
   (* Like `isMarkedForFlatten`, but for function/block arguments. *)
   val getArgFlatteningDecision: flattenedVars * Var.t -> conDecision

   (* Returns the total number of variables and constructors marked for
   flattening *)
   val markedCount: flattenedVars -> int

    (* Tracks types of `Var.t`s  and argument/return types *)
    type varTypes
    val newVarTypes: unit -> varTypes
    val destroyVarTypes: varTypes -> unit

   (* Sets the type for a future `getVarType` call. Valid to call multiple times
   (updating the stored type) *)
   val setVarType: varTypes * Var.t * Type.t -> unit
   (* Returns the type set by a previous `setVarType` call. *)
   val getVarType: varTypes * Var.t -> Type.t
   (* Sets the type for a future `getReturnType` call. Valid to call multiple
   times (updating the stored type) *)
   val setReturnType: varTypes * Func.t * Type.t vector option -> unit
   (* Returns the type set by a pervious `setFuncArgType` call. *)
   val getReturnType: varTypes * Func.t -> Type.t vector option

   (* Applies all changes to argument/return types recorded in `varTypes` to the
      provided function.

      TODO(pscollins): Statement types are currently propagated separately --
      this is a bit ugly; revisit.
   *)
   val updateToSavedTypes: varTypes * Function.t -> Function.t

   (* Marks any vars in `Statement.t` that must be flattened according to the
   provided policy. The following statement types may induce flattening:

     * Any binding introducing a new array-typed variable:
       x: ('a * 'b ...) array = ...

     * Any argument (block or function) introducing a new array-typed variable:
       f(x: 'a * b * ... array, ...)

     * Any `Con.t` over an array type
       datatype t = ConT of ('a * 'b * ...) array

     * TODO(pscollins): More types? Should handle vector too
    *)

   val markStatementForPolicy: (flattenedVars * flattenPolicy) ->
                               Statement.t -> unit
   val markArgForPolicy: (flattenedVars * flattenPolicy) ->
                         (Var.t * Type.t) -> unit
   val markDatatypeForPolicy: (flattenedVars * flattenPolicy) ->
                               Datatype.t -> unit


   (* If the provided `Var.t` is not marked for flattening, returns the original
   (var, type). Otherwise, returns (var, flattenedType), where `flattenedType`
   is flattened according to the rules of `maybeFlattenType`: if `type` is not
   flattenable, raises BadFlattenError. *)
   exception BadFlattenError
   val maybeFlattenArg: flattenedVars * (Var.t * Type.t) ->
                        Var.t * Type.t

   (* Flattens the provided `Statement.t` into a sequence of statements, if
   possible. Otherwise, returns NONE.

   All transformations below preserve any applicable flags (e.g. `raw` for
   `Array_alloc`, `readBarrier` for `Array_sub`, `writeBarrier` for
   `Array_update`).

   The following `PrimApp` expressions are flattenable:

     1. `Array_alloc` on tuple types
       x: ('a * 'b * ...) array = Array_alloc['a * 'b * ...](n)
       -->
       arr_a: 'a array = Array_alloc['a](n)
       arr_b: 'b array = Array_alloc['b](n)
       ...
       x: 'a array * 'b array * ... = tuple(arr_a, arr_b, ...)

    2. `Array_length` on tuple types:
      arr: ('a * 'b * ...) array = ...
      n: int = Array_length['a * b * ...](arr)
      -->
      (* by 1., arr is now 'a array * b array * ... *)
      arr_a = select (arr, 0)
      n: int = Array_length['a](arr)
      ...

    3. `Array_sub` on tuple types:
      arr: ('a * 'b * ...) array = ...
      x: ('a * 'b * ...) = Array_sub['a * 'b * ...](x, i)
      -->
      (* by 1., arr is now 'a array * b array * ... *)
      arr_a: 'a array = select (arr, 0)
      x_a: 'a =  Array_sub['a](arr_a, i)
      arr_b: 'b array = select (arr, 1)
      x_b: 'b =  Array_sub['b](arr_b, i)
      ...
      x: ('a * 'b * ...) = tuple(x_a, x_b, ...)

    4. `Array_update` on tuple types:
      arr: ('a * 'b * ...) array = ...
      x: ('a * 'b * ...) = ...
      _ = Array_update['a * 'b * ...](arr, i, x)
      -->
      (* by 1., arr is now 'a array * b array * ... *)
      arr_a: 'a array = select(arr, 0)
      x_a: 'a = select(x, 1)
      _ = Array_update['a])(arr_a, i, x_a)
      arr_b: 'b array = select(arr, 1)
      x_b: 'b = select(x, 2)
      _ = Array_update['b])(arr_b, i, x_b)
     ...

    5. `Array_toVector` on tuple types:
      arr: ('a * 'b * ...) array = ...
      vec: ('a * 'b * ...) vector = Array_toVector['a * 'b * ...](arr)
      -->
      (* by 1., arr is now 'a array * b array * ... *)
      arr_a: 'a array = select(arr, 0)
      vec_a: 'a vector = Array_toVector['a](arr_a)
      arr_b: 'b array = select(arr, 1)
      vec_b: 'b vector = Array_toVector['b](arr_b)
      ...
      vec: 'a vector * 'b vector * ... = tuple (vec_a, vec_b, ...)

    6. `Vector_length` on tuple types:
      vec: ('a * 'b * ...) vector = ...
      n: int = Vector_length['a * b * ...](vec)
      -->
      (* vec is now 'a vector * b vector * ... *)
      vec_a = select (vec, 0)
      n: int = Vector_length['a](vec_a)

    7. `Vector_sub` on tuple types:
      vec: ('a * 'b * ...) vector = ...
      x: ('a * 'b * ...) = Vector_sub['a * 'b * ...](vec, i)
      -->
      (* vec is now 'a vector * b vector * ... *)
      vec_a: 'a vector = select (vec, 0)
      x_a: 'a =  Vector_sub['a](vec_a, i)
      vec_b: 'b vector = select (vec, 1)
      x_b: 'b =  Vector_sub['b](vec_b, i)
      ...
      x: ('a * 'b * ...) = tuple(x_a, x_b, ...)


    8. `Array_uninitIsNop` on tuple types:
      arr: ('a * 'b * ...) array = ...
      isNop: bool = Array_uninitIsNop[('a * 'b * ...) array](arr)
      -->
      isNop: bool = false

    9. `Array_toArray` on tuple types:
      arr: ('a * 'b * ...) array = ...
      arr': ('a * 'b * ...) array = Array_toArray['a * 'b * ...](arr)
      -->
      (* by 1., arr is now 'a array * b array * ... *)
      arr_a: 'a array = select (arr, 0)
      arr'_a: 'a array = Array_toArray['a](arr_a)
      arr_b: 'b array = select (arr, 1)
      arr'_b: 'b array = Array_toArray['b](arr_b)
      ...
      arr' = tuple (arr'_a, arr'_b, ...)
      ...

   10. `Array_uninit` on tuple types:
      arr: ('a * 'b * ...) array = ...
      _ = Array_uninit[('a * 'b * ...)](arr, n)
      -->
      (* by 1., arr is now 'a array * b array * ... *)
      arr_a: 'a array = select (arr, n)
      arr_b: 'b array = select (arr, 1)
      _: = Array_uninit['a](arr_a, n)
      _: = Array_uninit['b](arr_b, n)
      ...


    Non-`PrimApp` expressions and also non-array/vector `PrimApp` expressions
    always return `SOME (originalStatement)`

    *)
   val maybeFlattenStatement: Statement.t ->
                              Statement.t vector option

   (* Returns `true` if `Statement.` must be flattened.

      A statement must be flattened if it uses or defines a `Var.t` that must be
      flattened.
    *)
   val mustFlattenStatement: flattenedVars * Statement.t -> bool

   (* Propages `varTypes` through the provided `Exp.t` (if necessary)

      For `Exp.t`s that must be updated for flattening, returns
        SOME (exp, ty)
      where `exp` is the updated expression, and `ty` is the updated return
      type.

      For `Exp.t`s that do not change under flattening, `NONE`.

      Non-`PrimApp`s pass through the `exp` unchanged and return, i.e.

        * `select (t, n)`
        * `tuple (x1, x2, x3)`
        * `Var (x)`

      propagate `varTpes` in the obvious way. Other non-`PrimApp`s return NONE.

      For `PrimApp`s:

        * Ref_deref[_](arg) -> SOME (Ref_deref[type(arg)], type(arg))
        * Ref_ref[_](arg) -> SOME (Ref_ref[type(arg)], type(arg) ref)

      Note that this function does NOT support `Array_` prims -- these require
      more complicated rewrites (emitting multiple statements) and so aren't
      supported here.

      TODO: ConApp should "unify"
      TODO: PrimApp
   *)
   val maybePropagateTypesInExp: varTypes * Exp.t ->
                                 (Exp.t * Type.t) option


   (* For all statements `lhs: ty = rhs`

        1. Recomputes `(ty', rhs')` via propagation (defined above)

        2. If needed, updates `lhs` to `ty'` in `varTypes`
        3. Returns a new `Statement.t` with the updated types

     e.g. for
       * varTypes = {x -> int, y -> bool * bool}
       * statement = {y: bool * bool = tuple (x, x)}

     this call:
       1. Updates `varTypes` so that `y -> int * int`
       2. Returns the modified statement
          y: int * int = tuple (int, int)
   *)
   val propagateTypesInStatement: varTypes * Statement.t -> Statement.t

   (* Updates `returns` to match the type of all `Return`s.

   If the function's current `returns` is `NONE`, returns `NONE`.

   If the type of every `Return.t` matches, returns `SOME returnTy`

   Otherwise, if the types of the `Return.t`s are inconsistent, raises
   `InconsistentTypes`.
   *)
   exception InconsistentTypes
   val propagateReturnTypes: varTypes * Function.t -> Type.t vector option

   (* Updates `varTypes` for the provided `Transfer.t`:

      1. For `Call`/`Goto`: updates each formal parameter type to match the type
         of the passed argument (for the target `func`/`label`)
      2. For `Return`: updates the return type of the provided `Func.t`
      3. For `Call` with a `Tail` return type: updates the return type of the
         provided `Func.t` to match the return type of the target function.
   *)
   val propagateThroughTransfer: varTypes * funcsMap * Func.t * Transfer.t -> unit

   (* Flattens (according to the rules of `maybeFlattenStatement`) all
   statements in the provided `Statement.t vector` that require it (according to
   the rules of `mustFlattenStatement`), then updates types via the rules of
   `propagateTypesInStatement`.

   If some statement must be flattened, but cannot, raises
   `IllegalFlatteningDecision`.
    *)
   (* TODO: needs tests *)
   exception IllegalFlatteningDecision
   val flattenStatements: flattenedVars -> Statement.t vector ->
                          Statement.t vector
   (* Like above, but for arguments *)
   val flattenArgs: flattenedVars -> (Var.t * Type.t) vector ->
                    (Var.t * Type.t) vector

   (* Like above, but for marked `Con.t`s *)
   val flattenDatatype: flattenedVars -> Datatype.t ->
                        Datatype.t

   (* Runs one iteration of flattening, collecting all flattenable array values
      and transforming them appropriately. Returns (SOME ...) if any value was
      successfully flattened, NONE otherwise.
    *)
   val flattenOnce: flattenPolicy -> Program.t -> Program.t option
end
