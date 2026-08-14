local
   open Ssa

   fun assert (cond, msg) =
      if cond then () else raise TestFail msg

   fun assertEqualTypes (actual, expected, msg) =
      if Type.equals (actual, expected) then ()
      else assert (false, msg ^ ": type mismatch\n" ^
                          "  Expected: " ^ Layout.toString (Type.layout expected) ^ "\n" ^
                          "  Actual:   " ^ Layout.toString (Type.layout actual))

   val _ = Control.libTargetDir := "../../build/lib/mlton/targets/self"

   val intTy = Type.intInf
   val word32Ty = Type.word WordSize.word32

   fun buildProgram (s, n) = let
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val mainBlock = Block.T {
         args = Vector.fromList [(n, intTy)],
         label = L0,
         statements = Vector.new1 s,
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock],
         inline = InlineAttr.Auto,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = L0
      }
   in
      Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }
   end
in
   (* Test 1: MaxWidthSameType + FlattenSoA on identical tuple type array. *)
   val _ = runTest ("Test 1: MaxWidthSameType + FlattenSoA (identical tuple)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = Exp.PrimApp {args = Vector.new1 n,
                            prim = allocPrim,
                            targs = Vector.new1 tuple2Ty},
         ty = arrayTuple2Ty,
         var = SOME v1
      }
      val p = buildProgram (s, n)
      val policy = ShallowFlatten.MaxWidthSameType 2
      val mechanism = ShallowFlatten.FlattenSoA
      val res = ShallowFlatten.flattenOnce (policy, mechanism) p
   in
      case res of
         SOME p' => let
            val Program.T {functions, ...} = p'
            val mainFunction' = hd functions
            val {blocks, ...} = Function.dest mainFunction'
            val Block.T {statements, ...} = Vector.sub (blocks, 0)
            val lastStmt = Vector.sub (statements, Vector.length statements - 1)
            val Statement.T {ty = actualTy, ...} = lastStmt
            val expectedTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
            val _ = assertEqualTypes (actualTy, expectedTy, "SoA output type")
         in () end
       | NONE => raise TestFail "Expected flattening to occur"
   end)

   (* Test 2: MaxWidthSameType + FlattenSoA on mixed tuple type array. *)
   val _ = runTest ("Test 2: MaxWidthSameType + FlattenSoA (mixed tuple, no-op)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val arrayMixedTupleTy = Type.array mixedTupleTy
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = Exp.PrimApp {args = Vector.new1 n,
                            prim = allocPrim,
                            targs = Vector.new1 mixedTupleTy},
         ty = arrayMixedTupleTy,
         var = SOME v1
      }
      val p = buildProgram (s, n)
      val policy = ShallowFlatten.MaxWidthSameType 2
      val mechanism = ShallowFlatten.FlattenSoA
      val res = ShallowFlatten.flattenOnce (policy, mechanism) p
   in
      assert (Option.isNone res, "Should not flatten mixed tuple under MaxWidthSameType policy")
   end)

   (* Test 3: MaxWidth + FlattenSoA on mixed tuple type array. *)
   val _ = runTest ("Test 3: MaxWidth + FlattenSoA (mixed tuple)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val arrayMixedTupleTy = Type.array mixedTupleTy
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = Exp.PrimApp {args = Vector.new1 n,
                            prim = allocPrim,
                            targs = Vector.new1 mixedTupleTy},
         ty = arrayMixedTupleTy,
         var = SOME v1
      }
      val p = buildProgram (s, n)
      val policy = ShallowFlatten.MaxWidth 2
      val mechanism = ShallowFlatten.FlattenSoA
      val res = ShallowFlatten.flattenOnce (policy, mechanism) p
   in
      case res of
         SOME p' => let
            val Program.T {functions, ...} = p'
            val mainFunction' = hd functions
            val {blocks, ...} = Function.dest mainFunction'
            val Block.T {statements, ...} = Vector.sub (blocks, 0)
            val lastStmt = Vector.sub (statements, Vector.length statements - 1)
            val Statement.T {ty = actualTy, ...} = lastStmt
            val expectedTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array word32Ty])
            val _ = assertEqualTypes (actualTy, expectedTy, "SoA mixed output type")
         in () end
       | NONE => raise TestFail "Expected flattening to occur"
   end)

   (* Test 4: MaxWidthSameType + FlattenAoS on identical tuple type array. *)
   val _ = runTest ("Test 4: MaxWidthSameType + FlattenAoS (identical tuple)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = Exp.PrimApp {args = Vector.new1 n,
                            prim = allocPrim,
                            targs = Vector.new1 tuple2Ty},
         ty = arrayTuple2Ty,
         var = SOME v1
      }
      val p = buildProgram (s, n)
      val policy = ShallowFlatten.MaxWidthSameType 2
      val mechanism = ShallowFlatten.FlattenAoS
      val res = ShallowFlatten.flattenOnce (policy, mechanism) p
   in
      case res of
         SOME p' => let
            val Program.T {functions, ...} = p'
            val mainFunction' = hd functions
            val {blocks, ...} = Function.dest mainFunction'
            val Block.T {statements, ...} = Vector.sub (blocks, 0)
            val lastStmt = Vector.sub (statements, Vector.length statements - 1)
            val Statement.T {ty = actualTy, ...} = lastStmt
            val expectedTy = Type.array intTy
            val _ = assertEqualTypes (actualTy, expectedTy, "AoS output type")
         in () end
       | NONE => raise TestFail "Expected AoS flattening to occur"
   end)

   val _ = summarize ()
end
