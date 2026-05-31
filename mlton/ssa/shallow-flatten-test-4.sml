structure SmlString = String
local
   open Ssa

   fun assert (cond, msg) =
      if cond then () else raise TestFail msg

   fun statementEquals (Statement.T {exp=e1, ty=t1, var=v1},
                        Statement.T {exp=e2, ty=t2, var=v2}) =
      Exp.equals (e1, e2) andalso
      Type.equals (t1, t2) andalso
      (case (v1, v2) of
          (SOME v1', SOME v2') => Var.equals (v1', v2')
        | (NONE, NONE) => true
        | _ => false)

   fun assertType (Statement.T {ty, ...}, expected, msg) =
      if Type.equals (ty, expected) then ()
      else assert (false, msg ^ ": type mismatch")

   fun isFlatten cd =
      case cd of
         ShallowFlatten.FlattenNode _ => true
       | _ => false
in
(* Test 26: Nested Tuple constructors *)
   val _ = runTest ("Test 26: Nested Tuple constructors", fn () => let
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val n = Var.fromString "n"
      val other = Var.fromString "other"
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val z = Var.fromString "z"
      
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty

      val s1 = Statement.T {
         exp = Exp.PrimApp {args = Vector.new1 n,
                            prim = Prim.Array_alloc {raw = false},
                            targs = Vector.new1 tuple2Ty},
         ty = arrayTuple2Ty,
         var = SOME x
      }
      
      val s2 = Statement.T {
         exp = Exp.Tuple (Vector.fromList [x, other]),
         ty = Type.tuple (Vector.fromList [arrayTuple2Ty, word32Ty]),
         var = SOME y
      }

      val s3 = Statement.T {
         exp = Exp.Tuple (Vector.fromList [y, other]),
         ty = Type.tuple (Vector.fromList [Type.tuple (Vector.fromList [arrayTuple2Ty, word32Ty]), word32Ty]),
         var = SOME z
      }

      val mainBlock = Block.T {
         args = Vector.fromList [(n, intTy), (other, word32Ty)],
         label = L0,
         statements = Vector.fromList [s1, s2, s3],
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock],
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = L0
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }
      
      val policy = ShallowFlatten.MaxWidth 2
      val res = ShallowFlatten.flattenOnce policy p
      val p' = case res of
                  SOME p' => p'
                | NONE => raise TestFail "Should have flattened"
      
      val Program.T {functions = funcs', ...} = p'
      val mainFunction' = List.first funcs'
      val {blocks = blocks', ...} = Function.dest mainFunction'
      val block' = Vector.sub (blocks', 0)
      val stmts' = Block.statements block'
      
      (* Check the type of z in the rewritten stmts *)
      val z_stmt = Vector.last stmts'
      val Statement.T {ty = z_ty, ...} = z_stmt
      
      val flattenedXty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val expectedYty = Type.tuple (Vector.fromList [flattenedXty, word32Ty])
      val expectedZty = Type.tuple (Vector.fromList [expectedYty, word32Ty])
      
      val _ = if Type.equals (z_ty, expectedZty) then ()
              else assert (false, "z type mismatch: " ^ (Layout.toString (Type.layout z_ty)) ^ 
                                 " expected " ^ (Layout.toString (Type.layout expectedZty)))
   in () end)(* Test 27: non-PrimApp flattening (array) *)
   val _ = runTest ("Test 27: non-PrimApp flattening (array)", fn () => let
      val v1 = Var.newString "v1"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      val s = Statement.T {
         exp = Exp.Var v1,
         ty = arrayTuple2Ty,
         var = SOME (Var.newString "x")
      }

      val res = ShallowFlatten.maybeFlattenStatement s
      val ss = case res of
                  SOME ss => ss
                | NONE => raise TestFail "Should return SOME (original) for non-PrimApp"
      val _ = assert (Vector.length ss = 1, "Should result in exactly one statement")
      val s' = Vector.sub (ss, 0)
   in
      assert (statementEquals (s, s'), "Resulting statement should be the original")
   end)(* Test 28: non-PrimApp flattening (vector) *)
   val _ = runTest ("Test 28: non-PrimApp flattening (vector)", fn () => let
      val v1 = Var.newString "v1"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val vectorTuple2Ty = Type.vector tuple2Ty
      
      val s = Statement.T {
         exp = Exp.Var v1,
         ty = vectorTuple2Ty,
         var = SOME (Var.newString "x")
      }

      val res = ShallowFlatten.maybeFlattenStatement s
      val ss = case res of
                  SOME ss => ss
                | NONE => raise TestFail "Should return SOME (original) for non-PrimApp"
      val _ = assert (Vector.length ss = 1, "Should result in exactly one statement")
      val s' = Vector.sub (ss, 0)
   in
      assert (statementEquals (s, s'), "Resulting statement should be the original")
   end)(* Test 29: non-PrimApp no-flattening (not a tuple) *)
   val _ = runTest ("Test 29: non-PrimApp no-flattening (not a tuple)", fn () => let
      val v1 = Var.newString "v1"
      val intTy = Type.intInf
      val arrayIntTy = Type.array intTy
      
      val s = Statement.T {
         exp = Exp.Var v1,
         ty = arrayIntTy,
         var = SOME (Var.newString "x")
      }

      val res = ShallowFlatten.maybeFlattenStatement s
   in
      case res of
          SOME ss => assert (Vector.length ss = 1 andalso statementEquals (s, Vector.sub (ss, 0)), 
                            "Should return SOME (original)")
        | NONE => raise TestFail "Should return SOME (original)"
   end)(* Test 30: varTypes *)
   val _ = runTest ("Test 30: varTypes", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val v1 = Var.newString "v1"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      (* First set *)
      val _ = ShallowFlatten.setVarType (vt, v1, intTy)
      val resTy1 = ShallowFlatten.getVarType (vt, v1)
      val _ = assert (Type.equals (resTy1, intTy), "getVarType should return the first set type")
      
      (* Second set (update) *)
      val _ = ShallowFlatten.setVarType (vt, v1, word32Ty)
      val resTy2 = ShallowFlatten.getVarType (vt, v1)
      val _ = assert (Type.equals (resTy2, word32Ty), "getVarType should return the updated type")

      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 30b: varTypes returnType *)
   val _ = runTest ("Test 30b: varTypes returnType", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val f1 = Func.fromString "f1"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      (* Test initial/set with NONE *)
      val _ = ShallowFlatten.setReturnType (vt, f1, NONE)
      val res1 = ShallowFlatten.getReturnType (vt, f1)
      val _ = assert (Option.isNone res1, "getReturnType should return NONE")
      
      (* Test set with SOME *)
      val tys = Vector.fromList [intTy, word32Ty]
      val _ = ShallowFlatten.setReturnType (vt, f1, SOME tys)
      val res2 = ShallowFlatten.getReturnType (vt, f1)
      val _ = case res2 of
                  SOME tys' => assert (Vector.forall2 (tys, tys', Type.equals), "getReturnType should return correct types")
                | NONE => raise TestFail "getReturnType should return SOME"
                
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 31: propagateTypesInStatement (Tuple) *)
   val _ = runTest ("Test 31: propagateTypesInStatement (Tuple)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      val word2Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty])
      val _ = ShallowFlatten.setVarType (vt, y, word2Ty)
      
      val s = Statement.T {
         exp = Exp.Tuple (Vector.fromList [x, x]),
         ty = word2Ty,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      val expectedTy = Type.tuple (Vector.fromList [intTy, intTy])
      
      val _ = assertType (s', expectedTy, "Tuple type should be updated")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), expectedTy), "varTypes updated")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 32: propagateTypesInStatement (Select) *)
   val _ = runTest ("Test 32: propagateTypesInStatement (Select)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val xTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val _ = ShallowFlatten.setVarType (vt, x, xTy)
      
      val s = Statement.T {
         exp = Exp.Select {offset = 1, tuple = x},
         ty = intTy,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      val _ = assertType (s', word32Ty, "Select(1, x) type should be word32")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), word32Ty), "varTypes updated")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 33: propagateTypesInStatement (Var) *)
   val _ = runTest ("Test 33: propagateTypesInStatement (Var)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      
      val s = Statement.T {
         exp = Exp.Var x,
         ty = word32Ty,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      val _ = assertType (s', intTy, "Var(x) type should be intInf")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), intTy), "varTypes updated")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 34: propagateTypesInStatement (Const) *)
   val _ = runTest ("Test 34: propagateTypesInStatement (Const)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val _ = ShallowFlatten.setVarType (vt, y, word32Ty)
      val s = Statement.T {
         exp = Exp.Const (Const.IntInf 1),
         ty = word32Ty,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      val _ = assertType (s', word32Ty, "Const type should be preserved")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), word32Ty), "varTypes updated")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 35: propagateTypesInStatement (PrimApp - noop) *)
   val _ = runTest ("Test 35: propagateTypesInStatement (PrimApp - noop)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      
      val _ = ShallowFlatten.setVarType (vt, y, intTy)
      val s = Statement.T {
         exp = Exp.PrimApp {args = Vector.new1 x,
                            prim = Prim.IntInf_add,
                            targs = Vector.new0 ()},
         ty = intTy,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      (* Should be exactly the same (no-op) *)
      val Statement.T {ty, ...} = s'
      val _ = assert (Type.equals (ty, intTy), "PrimApp type preserved")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), intTy), "varTypes should be unchanged/preserved")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 36: propagateTypesInStatement (ConApp) *)
   val _ = runTest ("Test 36: propagateTypesInStatement (ConApp)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val _ = ShallowFlatten.setVarType (vt, y, intTy)
      val c = Con.fromString "C"
      val s = Statement.T {
         exp = Exp.ConApp {args = Vector.new0 (), con = c},
         ty = word32Ty,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      val _ = assertType (s', word32Ty, "ConApp type preserved/recomputed")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), word32Ty), "varTypes updated")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 37: propagateTypesInStatement (NONE var) *)
   val _ = runTest ("Test 37: propagateTypesInStatement (NONE var)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      
      val s = Statement.T {
         exp = Exp.Var x,
         ty = word32Ty,
         var = NONE
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      val _ = assertType (s', intTy, "Statement type updated even if var is NONE")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)(* Test 38: Constructor flattening marks *)
   val _ = runTest ("Test 38: Constructor flattening marks", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val c1 = Con.fromString "C1"
      val c2 = Con.fromString "C2"

      val v1 = Vector.new1 (ShallowFlatten.FlattenNode (Vector.new0 ()))
      val _ = ShallowFlatten.setConFlatteningDecision (fv, c1, v1)
      val _ = assert (isFlatten (Vector.sub (ShallowFlatten.getConFlatteningDecision (fv, c1), 0)), "C1 should be marked")
      
      val v2 = Vector.new1 (ShallowFlatten.PreserveNode (Vector.new0 ()))
      val _ = ShallowFlatten.setConFlatteningDecision (fv, c2, v2)
      val _ = assert (not (isFlatten (Vector.sub (ShallowFlatten.getConFlatteningDecision (fv, c2), 0))), "C2 should not be marked")

      val _ = ShallowFlatten.destroyFlattenedVars fv
      in () end)(* Test 39: markDatatypeForPolicy *)
      val _ = runTest ("Test 39: markDatatypeForPolicy", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val policy = ShallowFlatten.MaxWidth 3

      val tycon = Tycon.fromString "t"
      val con1 = Con.fromString "Con1" (* (int * int) array -> Mark *)
      val con2 = Con.fromString "Con2" (* (int * int * int * int) array -> No Mark (too wide) *)
      val con3 = Con.fromString "Con3" (* int array -> No Mark (not a tuple) *)
      val con4 = Con.fromString "Con4" (* (int * int * int) array -> Mark *)

      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val tuple3Ty = Type.tuple (Vector.fromList [intTy, intTy, intTy])
      val tuple4Ty = Type.tuple (Vector.fromList [intTy, intTy, intTy, intTy])

      val dt = Datatype.T {
        cons = Vector.fromList [
           {args = Vector.fromList [Type.array tuple2Ty], con = con1},
           {args = Vector.fromList [Type.array tuple4Ty], con = con2},
           {args = Vector.fromList [Type.array intTy], con = con3},
           {args = Vector.fromList [Type.array tuple3Ty], con = con4}
        ],
        tycon = tycon
      }

      val _ = ShallowFlatten.markDatatypeForPolicy (fv, policy) dt

      val _ = assert (isFlatten (Vector.sub (ShallowFlatten.getConFlatteningDecision (fv, con1), 0)), "Con1 should be marked")
      val _ = assert (not (isFlatten (Vector.sub (ShallowFlatten.getConFlatteningDecision (fv, con2), 0))), "Con2 should not be marked (too wide)")
      val _ = assert (not (isFlatten (Vector.sub (ShallowFlatten.getConFlatteningDecision (fv, con3), 0))), "Con3 should not be marked (not a tuple)")
      val _ = assert (isFlatten (Vector.sub (ShallowFlatten.getConFlatteningDecision (fv, con4), 0)), "Con4 should be marked")

      val _ = ShallowFlatten.destroyFlattenedVars fv
      in () end)(* Test 40: flattenDatatype *)
   val _ = runTest ("Test 40: flattenDatatype", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val flattenedTuple2Ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])

      val con1 = Con.fromString "Con1"
      val con2 = Con.fromString "Con2"
      
      val dt = Datatype.T {
         tycon = Tycon.fromString "t",
         cons = Vector.fromList [
            {con = con1, args = Vector.new1 arrayTuple2Ty},
            {con = con2, args = Vector.new1 arrayTuple2Ty}
         ]
      }

      (* Mark only con1 for flattening *)
      val policy = ShallowFlatten.MaxWidth 10
      val _ = ShallowFlatten.setConFlatteningDecision (fv, con1, Vector.new1 (ShallowFlatten.getConDecisionForPolicy policy arrayTuple2Ty))
      val policy' = ShallowFlatten.MaxWidth 1
      val _ = ShallowFlatten.setConFlatteningDecision (fv, con2, Vector.new1 (ShallowFlatten.getConDecisionForPolicy policy' arrayTuple2Ty))
      
      val dt' = ShallowFlatten.flattenDatatype fv dt
      val Datatype.T {cons = cons', ...} = dt'
      
      fun findCon c =
         case Vector.peek (cons', fn {con, ...} => Con.equals (con, c)) of
            SOME x => x
          | NONE => raise TestFail ("Constructor " ^ Con.toString c ^ " not found")

      val {args = args1, ...} = findCon con1
      val {args = args2, ...} = findCon con2
      
      val _ = assert (Type.equals (Vector.sub (args1, 0), flattenedTuple2Ty), 
                      "Con1 should be flattened")
      val _ = assert (Type.equals (Vector.sub (args2, 0), arrayTuple2Ty), 
                      "Con2 should NOT be flattened")

      val _ = ShallowFlatten.destroyFlattenedVars fv
   in () end)(* Test 41: flattenOnce with datatypes *)
   val _ = runTest ("Test 41: flattenOnce with datatypes", fn () => let
      val policy = ShallowFlatten.MaxWidth 3
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val flattenedTuple2Ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])

      val tycon = Tycon.fromString "t"
      val con1 = Con.fromString "Con1"
      val dt = Datatype.T {
         tycon = tycon,
         cons = Vector.new1 {con = con1, args = Vector.new1 arrayTuple2Ty}
      }

      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      
      (* v1 = Array_alloc[ (int * int) array ](n) *)
      val s1 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.new1 n,
            prim = Prim.Array_alloc {raw = false},
            targs = Vector.new1 tuple2Ty
         },
         ty = arrayTuple2Ty,
         var = SOME v1
      }

      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
         statements = Vector.new1 s1,
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainFunction = Function.new {
         args = Vector.new1 (n, Type.intInf),
         blocks = Vector.fromList [mainBlock],
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = mainLabel
      }
      val p = Program.T {
         datatypes = Vector.new1 dt,
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val p' = case ShallowFlatten.flattenOnce policy p of
                  SOME p' => p'
                | NONE => raise TestFail "flattenOnce failed to flatten"

      val Program.T {datatypes = dts', ...} = p'
      val dt' = Vector.sub (dts', 0)
      val Datatype.T {cons = cons', ...} = dt'
      val {args = args1, ...} = Vector.sub (cons', 0)
      
      val _ = assert (Type.equals (Vector.sub (args1, 0), flattenedTuple2Ty), 
                      "Datatype should be flattened in flattenOnce")
   in () end)

   (* Test 42: maybePropagateTypesInExp (Var) *)
   val _ = runTest ("Test 42: maybePropagateTypesInExp (Var)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val intTy = Type.intInf
      
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      
      (* Var expression *)
      val exp = Exp.Var x
      
      val res = ShallowFlatten.maybePropagateTypesInExp (vt, exp)
      val _ = case res of
                  SOME (exp', ty') =>
                  (assert (Exp.equals (exp, exp'), "Exp should be unchanged");
                   assert (Type.equals (ty', intTy), "Returned type should be intInf"))
                | NONE => raise TestFail "maybePropagateTypesInExp returned NONE for Var"
      
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 43: maybePropagateTypesInExp (Tuple) *)
   val _ = runTest ("Test 43: maybePropagateTypesInExp (Tuple)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      val _ = ShallowFlatten.setVarType (vt, y, word32Ty)
      
      val exp = Exp.Tuple (Vector.fromList [x, y])
      
      val res = ShallowFlatten.maybePropagateTypesInExp (vt, exp)
      val _ = case res of
                  SOME (exp', ty') =>
                  let
                     val expectedTy = Type.tuple (Vector.fromList [intTy, word32Ty])
                  in
                     assert (Exp.equals (exp, exp'), "Exp should be unchanged");
                     assert (Type.equals (ty', expectedTy), "Returned type should be intInf * word32")
                  end
                | NONE => raise TestFail "maybePropagateTypesInExp returned NONE for Tuple"
      
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 44: maybePropagateTypesInExp (Select) *)
   val _ = runTest ("Test 44: maybePropagateTypesInExp (Select)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val xTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      
      val _ = ShallowFlatten.setVarType (vt, x, xTy)
      
      val exp = Exp.Select {offset = 1, tuple = x}
      
      val res = ShallowFlatten.maybePropagateTypesInExp (vt, exp)
      val _ = case res of
                  SOME (exp', ty') =>
                  (assert (Exp.equals (exp, exp'), "Exp should be unchanged");
                   assert (Type.equals (ty', word32Ty), "Returned type should be word32"))
                | NONE => raise TestFail "maybePropagateTypesInExp returned NONE for Select"
      
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 45: maybePropagateTypesInExp (Const - noop) *)
   val _ = runTest ("Test 45: maybePropagateTypesInExp (Const - noop)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val exp = Exp.Const (Const.Word (WordX.fromInt (1, WordSize.word32)))
      
      val res = ShallowFlatten.maybePropagateTypesInExp (vt, exp)
      val _ = case res of
                  SOME _ => raise TestFail "maybePropagateTypesInExp should return NONE for Const"
                | NONE => ()
      
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 46: maybePropagateTypesInExp (Ref_deref) *)
   val _ = runTest ("Test 46: maybePropagateTypesInExp (Ref_deref)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val v = Var.fromString "v"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val innerTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val refTy = Type.reff innerTy
      
      val _ = ShallowFlatten.setVarType (vt, v, refTy)
      
      val exp = Exp.PrimApp {
         args = Vector.fromList [v],
         prim = Prim.Ref_deref,
         targs = Vector.fromList [intTy]
      }
      
      val expectedExp = Exp.PrimApp {
         args = Vector.fromList [v],
         prim = Prim.Ref_deref,
         targs = Vector.fromList [innerTy]
      }
      
      val res = ShallowFlatten.maybePropagateTypesInExp (vt, exp)
      val _ = case res of
                  SOME (exp', ty') =>
                  (assert (Exp.equals (exp', expectedExp), "Exp should be updated with new innerTy");
                   assert (Type.equals (ty', innerTy), "Returned type should be innerTy"))
                | NONE => raise TestFail "maybePropagateTypesInExp returned NONE for Ref_deref"
      
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 47: maybePropagateTypesInExp (Ref_ref) *)
   val _ = runTest ("Test 47: maybePropagateTypesInExp (Ref_ref)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val v = Var.fromString "v"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val innerTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val refTy = Type.reff innerTy
      
      val _ = ShallowFlatten.setVarType (vt, v, innerTy)
      
      val exp = Exp.PrimApp {
         args = Vector.fromList [v],
         prim = Prim.Ref_ref,
         targs = Vector.fromList [intTy]
      }
      
      val expectedExp = Exp.PrimApp {
         args = Vector.fromList [v],
         prim = Prim.Ref_ref,
         targs = Vector.fromList [innerTy]
      }
      
      val res = ShallowFlatten.maybePropagateTypesInExp (vt, exp)
      val _ = case res of
                  SOME (exp', ty') =>
                  (assert (Exp.equals (exp', expectedExp), "Exp should be updated with new innerTy");
                   assert (Type.equals (ty', refTy), "Returned type should be refTy"))
                | NONE => raise TestFail "maybePropagateTypesInExp returned NONE for Ref_ref"
      
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 48: propagateTypesInStatement (Ref_deref) *)
   val _ = runTest ("Test 48: propagateTypesInStatement (Ref_deref)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val v = Var.fromString "v"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val innerTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      
      val _ = ShallowFlatten.setVarType (vt, v, Type.reff innerTy)
      val _ = ShallowFlatten.setVarType (vt, y, intTy)
      
      val s = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v],
            prim = Prim.Ref_deref,
            targs = Vector.fromList [intTy]
         },
         ty = intTy,
         var = SOME y
      }
      
      val s' = ShallowFlatten.propagateTypesInStatement (vt, s)
      
      val expectedExp = Exp.PrimApp {
         args = Vector.fromList [v],
         prim = Prim.Ref_deref,
         targs = Vector.fromList [innerTy]
      }
      
      val Statement.T {exp=exp', ty=ty', var=var'} = s'
      val _ = assert (Exp.equals (exp', expectedExp), "Exp should be updated with new innerTy")
      val _ = assert (Type.equals (ty', innerTy), "Statement type should be updated to innerTy")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, y), innerTy), "varTypes should be updated for y")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)


   (* Test 49: propagateReturnTypes (returns = NONE) *)
   val _ = runTest ("Test 49: propagateReturnTypes (returns = NONE)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      val f = Function.new {
         args = Vector.new0 (),
         blocks = Vector.new1 mainBlock,
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = NONE,
         start = mainLabel
      }
      val res = ShallowFlatten.propagateReturnTypes (vt, f)
      val _ = assert (Option.isNone res, "propagateReturnTypes should return NONE if returns is NONE")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 50: propagateReturnTypes (returns = SOME, matching returns) *)
   val _ = runTest ("Test 50: propagateReturnTypes (returns = SOME, matching returns)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      val _ = ShallowFlatten.setVarType (vt, y, intTy)
      
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val L1 = Label.fromString "L1"
      
      val b0 = Block.T {
         args = Vector.new0 (),
         label = L0,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new1 x)
      }
      val b1 = Block.T {
         args = Vector.new0 (),
         label = L1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new1 y)
      }
      
      val f = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [b0, b1],
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new1 Type.bool),
         start = L0
      }
      
      val res = ShallowFlatten.propagateReturnTypes (vt, f)
      val _ = case res of
                  SOME tyVec =>
                     assert (Vector.length tyVec = 1 andalso Type.equals (Vector.sub (tyVec, 0), intTy),
                             "Expected SOME [intTy]")
                | NONE => raise TestFail "Expected SOME return types"
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

   (* Test 51: propagateReturnTypes (returns = SOME, inconsistent returns) *)
   val _ = runTest ("Test 51: propagateReturnTypes (returns = SOME, inconsistent returns)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val _ = ShallowFlatten.setVarType (vt, x, intTy)
      val _ = ShallowFlatten.setVarType (vt, y, word32Ty)
      
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val L1 = Label.fromString "L1"
      
      val b0 = Block.T {
         args = Vector.new0 (),
         label = L0,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new1 x)
      }
      val b1 = Block.T {
         args = Vector.new0 (),
         label = L1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new1 y)
      }
      
      val f = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [b0, b1],
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new1 intTy),
         start = L0
      }
      
      val worked = (ShallowFlatten.propagateReturnTypes (vt, f); false)
                   handle ShallowFlatten.InconsistentTypes => true
                        | _ => false
      val _ = assert (worked, "Expected InconsistentTypes exception")
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)


   (* Test 52: propagateReturnTypes (returns = SOME, no Return.t transfers) *)
   val _ = runTest ("Test 52: propagateReturnTypes (returns = SOME, no Return.t transfers)", fn () => let
      val vt = ShallowFlatten.newVarTypes ()
      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
         statements = Vector.new0 (),
         transfer = Transfer.Bug
      }
      val f = Function.new {
         args = Vector.new0 (),
         blocks = Vector.new1 mainBlock,
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new1 Type.intInf),
         start = mainLabel
      }
      val res = ShallowFlatten.propagateReturnTypes (vt, f)
      val _ = case res of
                  SOME tyVec =>
                     assert (Vector.length tyVec = 0, "Expected SOME (Vector.new0())")
                | NONE => raise TestFail "Expected SOME (Vector.new0())"
      val _ = ShallowFlatten.destroyVarTypes vt
   in () end)

(* Test 53: propagateThroughTransfer *)
   val _ = runTest ("Test 53: propagateThroughTransfer", fn () => let
      val mainFunc = Func.fromString "main"
      val calleeFunc = Func.fromString "calleeFunc"
      val callerFunc = Func.fromString "callerFunc"

      val L_main_start = Label.fromString "L_main_start"
      val L_callee_start = Label.fromString "L_callee_start"
      val L_caller_start = Label.fromString "L_caller_start"
      val L_cont = Label.fromString "L_cont"
      val L_goto_dst = Label.fromString "L_goto_dst"

      val v_arg = Var.fromString "v_arg"
      val v_callee_arg = Var.fromString "v_callee_arg"
      val v_cont_param = Var.fromString "v_cont_param"
      val v_goto_param = Var.fromString "v_goto_param"

      val intTy = Type.intInf
      val word32Ty = Type.word WordSize.word32
      val tupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])

      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = L_main_start,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock],
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = L_main_start
      }

      val calleeBlock = Block.T {
         args = Vector.fromList [(v_callee_arg, word32Ty)],
         label = L_callee_start,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new1 v_callee_arg)
      }
      val calleeFunction = Function.new {
         args = Vector.fromList [(v_callee_arg, word32Ty)],
         blocks = Vector.fromList [calleeBlock],
         mayInline = true,
         name = calleeFunc,
         raises = NONE,
         returns = SOME (Vector.new1 word32Ty),
         start = L_callee_start
      }

      val callerStartBlock = Block.T {
         args = Vector.new0 (),
         label = L_caller_start,
         statements = Vector.new0 (),
         transfer = Transfer.Call {
            args = Vector.new1 v_arg,
            func = calleeFunc,
            return = Return.NonTail {cont = L_cont, handler = Handler.Dead}
         }
      }
      val callerContBlock = Block.T {
         args = Vector.fromList [(v_cont_param, intTy)],
         label = L_cont,
         statements = Vector.new0 (),
         transfer = Transfer.Goto {
            args = Vector.new1 v_cont_param,
            dst = L_goto_dst
         }
      }
      val callerGotoDstBlock = Block.T {
         args = Vector.fromList [(v_goto_param, intTy)],
         label = L_goto_dst,
         statements = Vector.new0 (),
         transfer = Transfer.Call {
            args = Vector.new1 v_goto_param,
            func = calleeFunc,
            return = Return.Tail
         }
      }
      val callerFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [callerStartBlock, callerContBlock, callerGotoDstBlock],
         mayInline = true,
         name = callerFunc,
         raises = NONE,
         returns = SOME (Vector.new1 word32Ty),
         start = L_caller_start
      }

      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [calleeFunction, callerFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val fm = FlattenUtil.newFuncsMap p
      val vt = ShallowFlatten.newVarTypes ()

      (* Set up initial mismatched types *)
      val _ = ShallowFlatten.setVarType (vt, v_arg, tupleTy)
      val _ = ShallowFlatten.setVarType (vt, v_callee_arg, intTy)
      val _ = ShallowFlatten.setVarType (vt, v_cont_param, intTy)
      val _ = ShallowFlatten.setVarType (vt, v_goto_param, intTy)
      val _ = ShallowFlatten.setReturnType (vt, calleeFunc, SOME (Vector.new1 tupleTy))
      val _ = ShallowFlatten.setReturnType (vt, callerFunc, SOME (Vector.new1 intTy))

      (* Case 1: Call + NonTail *)
      val transfer1 = Block.transfer callerStartBlock
      val _ = ShallowFlatten.propagateThroughTransfer (vt, fm, callerFunc, transfer1)
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, v_callee_arg), tupleTy),
                      "Case 1: Call + NonTail failed to propagate arg type")
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, v_cont_param), tupleTy),
                      "Case 1: Call + NonTail failed to propagate return type to cont block arg")

      (* Reset v_callee_arg for independent test in subsequent cases *)
      val _ = ShallowFlatten.setVarType (vt, v_callee_arg, intTy)

      (* Case 2: Goto *)
      (* Since v_cont_param is now tupleTy, propagating Goto transfer should update v_goto_param to tupleTy *)
      val transfer2 = Block.transfer callerContBlock
      val _ = ShallowFlatten.propagateThroughTransfer (vt, fm, callerFunc, transfer2)
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, v_goto_param), tupleTy),
                      "Case 2: Goto failed to propagate arg type")

      (* Case 3: Call + Tail *)
      val transfer3 = Block.transfer callerGotoDstBlock
      val _ = ShallowFlatten.propagateThroughTransfer (vt, fm, callerFunc, transfer3)
      (* formal arg in target function: v_callee_arg (originally intTy, should match v_goto_param: tupleTy) *)
      val _ = assert (Type.equals (ShallowFlatten.getVarType (vt, v_callee_arg), tupleTy),
                      "Case 3: Call + Tail failed to propagate arg type")
      (* callerFunc return type should match calleeFunc return type (SOME (Vector.new1 tupleTy)) *)
      val _ = case ShallowFlatten.getReturnType (vt, callerFunc) of
                 SOME tys => assert (Vector.length tys = 1 andalso Type.equals (Vector.sub (tys, 0), tupleTy),
                                     "Case 3: Call + Tail failed to propagate caller return type")
               | NONE => raise TestFail "Case 3: Expected caller return type to be SOME"

      (* Case 4: Return *)
      (* Reset calleeFunc return type to mismatched type *)
      val _ = ShallowFlatten.setReturnType (vt, calleeFunc, SOME (Vector.new1 intTy))
      val _ = ShallowFlatten.setVarType (vt, v_callee_arg, tupleTy)
      val transfer4 = Block.transfer calleeBlock
      val _ = ShallowFlatten.propagateThroughTransfer (vt, fm, calleeFunc, transfer4)
      val _ = case ShallowFlatten.getReturnType (vt, calleeFunc) of
                 SOME tys => assert (Vector.length tys = 1 andalso Type.equals (Vector.sub (tys, 0), tupleTy),
                                     "Case 4: Return failed to propagate return type")
               | NONE => raise TestFail "Case 4: Expected callee return type to be SOME"

      val _ = ShallowFlatten.destroyVarTypes vt
      val _ = #destroyFuncsMap fm ()
   in () end)

   (* Test 54: propagateThroughTransfer - Call to function returning NONE with continuation block arguments *)
   val _ = runTest ("Test 54: propagateThroughTransfer - Call to function returning NONE with continuation block arguments", fn () => let
      val _ = Control.libTargetDir := "../../build/lib/mlton/targets/self"

      val f_main = Func.fromString "f_main"
      val f_noreturn = Func.fromString "f_noreturn"
      val L_start = Label.fromString "L_start"
      val L_cont = Label.fromString "L_cont"
      val L_noreturn = Label.fromString "L_noreturn"

      val intTy = Type.intInf
      val v_cont_arg = Var.fromString "v_cont_arg"

      val noreturnBlock = Block.T {
         args = Vector.new0 (),
         label = L_noreturn,
         statements = Vector.new0 (),
         transfer = Transfer.Bug
      }

      val noreturnFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [noreturnBlock],
         mayInline = true,
         name = f_noreturn,
         raises = NONE,
         returns = NONE,
         start = L_noreturn
      }

      val transfer = Transfer.Call {
         args = Vector.new0 (),
         func = f_noreturn,
         return = Return.NonTail {
            cont = L_cont,
            handler = Handler.Caller
         }
      }

      val mainStartBlock = Block.T {
         args = Vector.new0 (),
         label = L_start,
         statements = Vector.new0 (),
         transfer = transfer
      }

      val mainContBlock = Block.T {
         args = Vector.fromList [(v_cont_arg, intTy)],
         label = L_cont,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }

      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainStartBlock, mainContBlock],
         mayInline = true,
         name = f_main,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = L_start
      }

      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [noreturnFunction, mainFunction],
         globals = Vector.new0 (),
         main = f_main
      }

      val vt = ShallowFlatten.newVarTypes ()
      val fm = ShallowFlatten.FlattenUtil.newFuncsMap p

      (* Initialize types for functions and variables *)
      val _ = ShallowFlatten.setReturnType (vt, f_noreturn, NONE)
      val _ = ShallowFlatten.setReturnType (vt, f_main, SOME (Vector.new0 ()))
      val _ = ShallowFlatten.setVarType (vt, v_cont_arg, intTy)

      (* Under the buggy compiler, propagateThroughTransfer raises Fail "Vector.foldi2From" *)
      val _ =
         case (SOME (ShallowFlatten.propagateThroughTransfer (vt, fm, f_main, transfer)))
              handle Fail msg =>
                 if SmlString.hasPrefix (msg, {prefix = "Vector.foldi2From"}) then NONE
                 else raise Fail msg of
           NONE => (assert (true, "Symptom of the original bug present in propagateThroughTransfer");
                    ShallowFlatten.destroyVarTypes vt;
                    #destroyFuncsMap fm ();
                    raise TestFail "Bug is present: propagateThroughTransfer raised Vector.foldi2From")
         | SOME () => ()

      val _ = ShallowFlatten.destroyVarTypes vt
      val _ = #destroyFuncsMap fm ()
   in () end)

   val _ = summarize ()
end

