signature SHALLOW_FLATTEN =
sig
   (* Exports:
        val transform: Program.t -> Program.t

     which iteratively runs `flattenOnce` until convergence. The behavior of
     this call is controlled by the following flags:

       -shallow-flatten-max-iters=N
           limits the number of iterations to N

       -shallow-flatten-policy=[maxWidth|maxWidthSameType]:$N
           sets the policy to `MaxWidth(N)`/`MaxWidthSameType(N)`

       -shallow-flatten-mechanism=[aos|soa]:
           sets the flattenMechanism
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
     (* Like `MaxWidth`, but only applies to tuples where all elements are the
     same type. *)
     | MaxWidthSameType of int

   (* How should we flatten the flattenable array/vector types? *)
   datatype flattenMechanism =
            (* Flatten in struct-of-array format, i.e.
               ('a * 'b * 'c) array ->
               ('a array * 'b array * 'c array)
             *)
            FlattenSoA
            (* Flatten in array-of-struct format, i.e.
               ('a * 'a * 'a) array ->
               'a array

               where len(flat_array) == 3 * len(array), and all indexing
               operations, etc, are adjusted appropriately.

               (For now, only compatible with MaxWidthSameType policy)
             *)
             | FlattenAoS

   (* Should this `Type.t` flattened according to `policy`? *)
   val shouldFlattenType: flattenPolicy -> Type.t -> bool

   (* Update an entire nested subject to `flattenPolicy`

      Unlike `maybeFlattenType`, this function traverses through non-flattenable
      types and transforms any flattenable nodes in the provided `Type.t`, i.e.

        ('a * 'b) array -> 'a array * 'b array
        (('a * b) array) ref -> (('a array) * ('b array)) ref


      Additionally, this function runs until convergence, i.e. we have:

        (('a * b) array) array
          -> (('a array) * ('b array)) array
          -> ('a array array) * ('b array array)

      The type that results from flattening is controlled by `flattenMechanism`,
      i.e.

        ('a * 'b) array + FlattenSoA -> 'a array * 'b array
        ('a * 'a) array + FlattenAoS -> 'a array
   *)
   val deepFlattenTypeForConfig: (flattenPolicy * flattenMechanism) -> Type.t -> Type.t

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

         * Flattenable statement, no type transformation required (for FlattenSoA)
         arr: ('a * b) array = Array_alloc['a * b](n]
         -->
         arr_a: 'a array = Array_alloc['a](n)
         arr_b: 'b array = Array_alloc['b](n)
         arr: ('a array) * (b' array) = tuple (arr_a, arr_b)


         * Non-flattenable statement, with type transformation required (for FlattenSoA)
         arr: ('a * b) array array = Array_alloc[('a * b') array](n]
         -->
         arr: ('a array * b array) array = Array_alloc['a array * b' array](n]

       The applied flattening transformation + resulting types depend on the
       supplied `flattenMechanism` in the obvious way, i.e.

         * Above example under FlattenAoS:
         arr: ('a * 'a) array = Array_alloc['a * 'a](n)
         -->
         arr: 'a array = Array_alloc['a](n * 2)

    *)
   val deepFlattenStatementsForConfig: (flattenPolicy * flattenMechanism) ->
                                       Statement.t -> Statement.t vector

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
  
      Flattened types are constructed according to the rules of the provided
      `flattenMechanism`, i.e.

         {FlattenAoS + FlattenNode + ('a * 'b) array} -> 'a array * 'b array}
         {FlattenSoA + FlattenNode + ('a * 'a) array} -> 'a array}
         {FlattenSoA + FlattenNode + ('a * 'b) array} -> InvalidConFlattening}
   *)
   exception InvalidConFlattening
   val applyConDecision: flattenMechanism ->
                         conDecision * Type.t ->
                         Type.t

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
      -->      (* by 1., arr is now 'a array * b array * ... *)
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

   (* AoS variant of the transformation above.

      For AoS-flattenable `PrimApp` expressions, converts a load from an
      array/vector-of-tuple into a load from a flat array. Unlike the SoA
      flattening transformation, the SoA flattening transformation only supports
      tuples whose members are the same type.

      The following `PrimApp` expressions are flattenable:

     1. `Array_alloc` on tuple types
       x: ('a * 'a * ...) array = Array_alloc['a * 'a * ...](n)
       -->
       tupleSize: indexTy = tupleWidth('a * 'a * ...)
       n': indexTy = n * tupleWidth
       x: 'a array = Array_alloc['a](n')

    2. `Array_length` on tuple types:
      arr: ('a * 'a * ...) array = ...
      n: int = Array_length['a * 'a * ...](arr)
      -->
      (* by 1., arr is now 'a array *)
       tupleSize: indexTy = tupleWidth('a * 'a * ...)
       n': indexTy = Array_length['a](arr)
       n: indexTy = n' / tupleSize
      ...

    3. `Array_sub` on tuple types:
      arr: ('a * 'a * ...) array = ...
      x: ('a * 'a * ...) = Array_sub['a * 'a * ...](x, i)
      -->
      (* by 1., arr is now 'a array  *)
      tupleSize: indexTy = tupleWidth('a * 'a * ...)
      x_0: 'a =  Array_sub['a](arr, i * tupleSize)
      x_1: 'a =  Array_sub['a](arr, i * tupleSize + 1)
      ...
      x_n: 'b =  Array_sub['b](arr_b, i * tupleSize + tupleSize - 1)
      x: ('a * 'a * ...) = tuple(x_1, x_1, ...)

    4. `Array_update` on tuple types:
      arr: ('a * 'a * ...) array = ...
      x: ('a * 'a * ...) = ...
      _ = Array_update['a * 'a * ...](arr, i, x)
      -->
      (* by 1., arr is now 'a array *)
      tupleSize: indexTy = tupleWidth('a * 'a * ...)
      x_0: 'a = select(x, 0)
      x_1: 'a = select(x, 1)
      ...
      _ = Array_update['a](arr, i * tupleSize + 0, x_0)
      _ = Array_update['a](arr, i * tupleSize + 1, x_1)
      ...

    5. `Array_toVector` on tuple types:
      arr: ('a * 'a * ...) array = ...
      vec: ('a * 'a * ...) vector = Array_toVector['a * 'a * ...](arr)
      -->
      (* by 1., arr is now 'a array *)
      vec: 'a vector = Array_toVector['a)(arr)

    6. `Vector_length` on tuple types:
      n: int = Vector_length['a * 'a * ...](vec)
      -->
      (* vec is now 'a vector  ... *)
       tupleSize: indexTy = tupleWidth('a * 'a * ...)
       n': indexTy = Vector_length['a](arr)
       n: indexTy = n' / tupleSize

    7. `Vector_sub` on tuple types:
      arr: ('a * 'a * ...) vector = ...
      x: ('a * 'a * ...) = Vector_sub['a * 'a * ...](x, i)
      -->
      (* by 1., arr is now 'a vector  *)
       tupleSize: indexTy = tupleWidth('a * 'a * ...)
       x_0: 'a =  Vector_sub['a](arr, i * tupleSize)
       x_1: 'a =  Vector_sub['a](arr, i * tupleSize + 1)
       ...
       x_n: 'b =  Vector_sub['b](arr_b, i * tupleSize + tupleSize - 1)
       x: ('a * 'a * ...) = tuple(x_1, x_1, ...)

    8. `Array_uninitIsNop` on tuple types:
      arr: ('a * 'a * ...) array = ...
      isNop: bool = Array_uninitIsNop['a * 'a * ...](arr)
      -->
      isNop: bool = false

    9. `Array_toArray` on tuple types:
      arr: ('a * 'a * ...) array = ...
      arr': ('a * 'a * ...) array = Array_toArray['a * 'a * ...](arr)
      -->
      (* by 1., arr is now 'a array *)
      arr': 'a array = Array_toArray['a](arr)
      ...

   10. `Array_uninit` on tuple types:
      arr: ('a * 'a * ...) array = ...
      _ = Array_uninit['a * 'a * ...](arr, n)
      -->
      (* by 1., arr is now 'a array *)
      tupleSize: indexTy = tupleWidth('a * 'a * ...)
      _: = Array_uninit['a](arr, n * tupleSize)
      _: = Array_uninit['a](arr, n * tupleSize + 1)
      ...
      _: = Array_uninit['a](arr, n * tupleSize + tupleSize - 1)

    Non-`PrimApp` expressions and also non-array/vector `PrimApp` expressions
    always return `SOME (originalStatement)`
    *)
   val maybeFlattenStatementAoS: Statement.t ->
                                 Statement.t vector option

   (* Runs one iteration of flattening, collecting all flattenable array values
      and transforming them appropriately. Returns (SOME ...) if any value was
      successfully flattened, NONE otherwise.
    *)
   val flattenOnce: (flattenPolicy * flattenMechanism)
                    -> Program.t -> Program.t option
end
