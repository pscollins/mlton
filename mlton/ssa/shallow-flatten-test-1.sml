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

   fun assertEqualTypes (actual, expected, input, msg) =
      if Type.equals (actual, expected) then ()
      else assert (false, msg ^ ": type mismatch\n" ^
                          "  Input:    " ^ Layout.toString (Type.layout input) ^ "\n" ^
                          "  Expected: " ^ Layout.toString (Type.layout expected) ^ "\n" ^
                          "  Actual:   " ^ Layout.toString (Type.layout actual))

   fun assertType (Statement.T {ty, ...}, expected, input, msg) =
      assertEqualTypes (ty, expected, input, msg)
in
(* Test 1: Simple program *)
   val _ = runTest ("Test 1: Simple program", fn () => let
      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
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
         start = mainLabel
      }
      val p1 = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val _ = ShallowFlatten.transform p1
   in () end)

(* Test 2: flattenProgram empty *)
   val _ = runTest ("Test 2: flattenProgram empty", fn () => let
      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
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
         start = mainLabel
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val fl = {
         updateType = fn ty => ty,
         updateStatements = fn stmts => stmts
      }

      val p' = ShallowFlatten.flattenProgram fl p
      val Program.T {datatypes, functions, globals, main} = p'
   in
      assert (Vector.length datatypes = 0, "datatypes empty");
      assert (Vector.length globals = 0, "globals empty");
      assert (length functions = 1, "functions length");
      assert (Func.equals (main, mainFunc), "main equals")
   end)

(* Test 3: flattenProgram transformations *)
   val _ = runTest ("Test 3: flattenProgram transformations", fn () => let
      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      val conC = Con.fromString "C"
      val tyconT = Tycon.fromString "T"
      val dt = Datatype.T {
         cons = Vector.fromList [{args = Vector.fromList [Type.bool], con = conC}],
         tycon = tyconT
      }
      val vGlob = Var.fromString "glob"
      val vArg = Var.fromString "arg"
      val vBArg = Var.fromString "barg"
      val vStmt = Var.fromString "stmt"

      val gstmt = Statement.T {
         exp = Exp.Var vGlob,
         ty = Type.bool,
         var = SOME vGlob
      }
      val bstmt = Statement.T {
         exp = Exp.Var vStmt,
         ty = Type.bool,
         var = SOME vStmt
      }

      val mainBlock = Block.T {
         args = Vector.fromList [(vBArg, Type.bool)],
         label = mainLabel,
         statements = Vector.fromList [bstmt],
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainFunction = Function.new {
         args = Vector.fromList [(vArg, Type.bool)],
         blocks = Vector.fromList [mainBlock],
         mayInline = true,
         name = mainFunc,
         raises = SOME (Vector.fromList [Type.bool]),
         returns = SOME (Vector.fromList [Type.bool]),
         start = mainLabel
      }
      val p = Program.T {
         datatypes = Vector.fromList [dt],
         functions = [mainFunction],
         globals = Vector.fromList [gstmt],
         main = mainFunc
      }

      val fl = {
         updateType = fn ty => if Type.equals (ty, Type.bool) then Type.intInf else ty,
         updateStatements = fn stmts =>
            Vector.map (stmts, fn Statement.T {exp, ty, var} =>
               Statement.T {exp = exp,
                            ty = if Type.equals (ty, Type.bool) then Type.intInf else ty,
                            var = var})
      }

      val p' = ShallowFlatten.flattenProgram fl p
      val Program.T {datatypes, functions, globals, main} = p'

      (* Verify datatypes *)
      val _ = assert (Vector.length datatypes = 1, "transformed datatypes length")
      val Datatype.T {cons, tycon} = Vector.sub (datatypes, 0)
      val _ = assert (Tycon.equals (tycon, tyconT), "tycon equals")
      val _ = assert (Vector.length cons = 1, "cons length")
      val {args = dtArgs, con = dtCon} = Vector.sub (cons, 0)
      val _ = assert (Con.equals (dtCon, conC), "dtCon equals")
      val _ = assert (Vector.length dtArgs = 1, "dtArgs length")
      val _ = assert (Type.equals (Vector.sub (dtArgs, 0), Type.intInf), "dtArg transformed to intInf")

      (* Verify globals *)
      val _ = assert (Vector.length globals = 1, "transformed globals length")
      val Statement.T {ty = gTy, ...} = Vector.sub (globals, 0)
      val _ = assert (Type.equals (gTy, Type.intInf), "global transformed to intInf")

      (* Verify functions *)
      val _ = assert (length functions = 1, "transformed functions length")
      val func = hd functions
      val {args = fArgs, blocks = fBlocks, raises = fRaises, returns = fReturns, ...} = Function.dest func

      (* Verify function args *)
      val _ = assert (Vector.length fArgs = 1, "fArgs length")
      val (_, fArgTy) = Vector.sub (fArgs, 0)
      val _ = assert (Type.equals (fArgTy, Type.intInf), "fArg transformed to intInf")

      (* Verify function returns and raises *)
      val _ = case fRaises of
                 SOME ts => assert (Type.equals (Vector.sub (ts, 0), Type.intInf), "fRaises transformed")
               | NONE => assert (false, "expected SOME raises")
      val _ = case fReturns of
                 SOME ts => assert (Type.equals (Vector.sub (ts, 0), Type.intInf), "fReturns transformed")
               | NONE => assert (false, "expected SOME returns")

      (* Verify block *)
      val _ = assert (Vector.length fBlocks = 1, "fBlocks length")
      val Block.T {args = bArgs, statements = bStatements, ...} = Vector.sub (fBlocks, 0)

      (* Verify block args *)
      val _ = assert (Vector.length bArgs = 1, "bArgs length")
      val (_, bArgTy) = Vector.sub (bArgs, 0)
      val _ = assert (Type.equals (bArgTy, Type.intInf), "bArg transformed to intInf")

      (* Verify block statements *)
      val _ = assert (Vector.length bStatements = 1, "bStatements length")
      val Statement.T {ty = bTy, ...} = Vector.sub (bStatements, 0)
      val _ = assert (Type.equals (bTy, Type.intInf), "block statement transformed to intInf")
   in () end)

(* Test 4: deepFlattenTypeForPolicy normal cases *)
   val _ = runTest ("Test 4: deepFlattenTypeForPolicy normal cases", fn () => let
      val intTy = Type.intInf
      val policy = ShallowFlatten.MaxWidth 2

      (* 1. ('a * 'b) array -> 'a array * 'b array *)
      val t1 = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val e1 = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val r1 = ShallowFlatten.deepFlattenTypeForPolicy policy t1

      (* 2. (('a * 'b) array) ref -> ('a array * 'b array) ref *)
      val t2 = Type.reff t1
      val e2 = Type.reff e1
      val r2 = ShallowFlatten.deepFlattenTypeForPolicy policy t2

      (* 3. (('a * 'b) array) vector -> 'a array vector * 'b array vector *)
      val t3 = Type.vector t1
      val e3 = Type.tuple (Vector.fromList [Type.vector (Type.array intTy), Type.vector (Type.array intTy)])
      val r3 = ShallowFlatten.deepFlattenTypeForPolicy policy t3
   in
      assertEqualTypes (r1, e1, t1, "t1 deepFlatten");
      assertEqualTypes (r2, e2, t2, "t2 deepFlatten");
      assertEqualTypes (r3, e3, t3, "t3 deepFlatten")
   end)

(* Test 5: deepFlattenTypeForPolicy edge cases *)
   val _ = runTest ("Test 5: deepFlattenTypeForPolicy edge cases", fn () => let
      val intTy = Type.intInf
      val policy = ShallowFlatten.MaxWidth 2

      (* 1. Width > MaxWidth: (int * int * int) array -> (int * int * int) array *)
      val t1 = Type.array (Type.tuple (Vector.fromList [intTy, intTy, intTy]))
      val e1 = t1
      val r1 = ShallowFlatten.deepFlattenTypeForPolicy policy t1

      (* 2. Double nesting (requires convergence): (('a * 'b) array) array -> 'a array array * 'b array array *)
      val t2_inner = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val t2 = Type.array t2_inner
      val e2 = Type.tuple (Vector.fromList [Type.array (Type.array intTy), Type.array (Type.array intTy)])
      val r2 = ShallowFlatten.deepFlattenTypeForPolicy policy t2

      (* 3. Array of tuple of arrays (outer is flattenable): (('a array) * ('b array)) array -> ('a array) array * ('b array) array *)
      val t3_inner = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val t3 = Type.array t3_inner
      val e3 = Type.tuple (Vector.fromList [Type.array (Type.array intTy), Type.array (Type.array intTy)])
      val r3 = ShallowFlatten.deepFlattenTypeForPolicy policy t3

      (* 4. Triple nesting: ((('a * 'b) array) array) array -> 'a array array array * 'b array array array *)
      val t4_inner_inner = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val t4_inner = Type.array t4_inner_inner
      val t4 = Type.array t4_inner
      val e4 = Type.tuple (Vector.fromList [Type.array (Type.array (Type.array intTy)),
                                            Type.array (Type.array (Type.array intTy))])
      val r4 = ShallowFlatten.deepFlattenTypeForPolicy policy t4

      (* 5. Ref nesting: ((('a * 'b) array) array) ref -> ('a array array * 'b array array) ref *)
      val t5_inner = Type.array (Type.array (Type.tuple (Vector.fromList [intTy, intTy])))
      val t5 = Type.reff t5_inner
      val e5_inner = Type.tuple (Vector.fromList [Type.array (Type.array intTy),
                                                  Type.array (Type.array intTy)])
      val e5 = Type.reff e5_inner
      val r5 = ShallowFlatten.deepFlattenTypeForPolicy policy t5

      (* 6. Alternating array/vector nesting: ((('a * 'b) array) vector) array -> 'a array vector array * 'b array vector array *)
      val t6_inner_inner = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val t6_inner = Type.vector t6_inner_inner
      val t6 = Type.array t6_inner
      val e6 = Type.tuple (Vector.fromList [Type.array (Type.vector (Type.array intTy)),
                                            Type.array (Type.vector (Type.array intTy))])
      val r6 = ShallowFlatten.deepFlattenTypeForPolicy policy t6

      (* 7. Tuple of array nesting (MaxWidth 3): (('a * 'b) array * 'c) array -> 'a array array * 'b array array * 'c array *)
      val policy3 = ShallowFlatten.MaxWidth 3
      val t7_array = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val t7_inner = Type.tuple (Vector.fromList [t7_array, intTy])
      val t7 = Type.array t7_inner
      val e7 = Type.tuple (Vector.fromList
                             [Type.tuple (Vector.fromList
                                            [Type.array (Type.array intTy),
                                             Type.array (Type.array intTy)]),
                              Type.array intTy])
      val r7 = ShallowFlatten.deepFlattenTypeForPolicy policy3 t7
   in
      assertEqualTypes (r1, e1, t1, "t1 deepFlatten edge case");
      assertEqualTypes (r2, e2, t2, "t2 deepFlatten edge case");
      assertEqualTypes (r3, e3, t3, "t3 deepFlatten edge case");
      assertEqualTypes (r4, e4, t4, "t4 deepFlatten iterative convergence");
      assertEqualTypes (r5, e5, t5, "t5 deepFlatten iterative convergence");
      assertEqualTypes (r6, e6, t6, "t6 deepFlatten iterative convergence");
      assertEqualTypes (r7, e7, t7, "t7 deepFlatten iterative convergence")
   end)

(* Test 6: doesPolicyFlattenStatement positive and negative cases *)
   val _ = runTest ("Test 6: doesPolicyFlattenStatement positive and negative cases", fn () => let
      val policy = ShallowFlatten.MaxWidth 2
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val tuple3Ty = Type.tuple (Vector.fromList [intTy, intTy, intTy])
      val vVar = Var.fromString "v"
      val nVar = Var.fromString "n"
      
      fun makePrimAppStatement (p, targs) =
         Statement.T {
            exp = Exp.PrimApp {
               args = Vector.new1 nVar,
               prim = p,
               targs = Vector.fromList targs
            },
            ty = intTy,
            var = SOME vVar
         }

      (* Positive Case 1: Array_alloc on 2-tuple (matches MaxWidth 2) *)
      val s_pos1 = makePrimAppStatement (Prim.Array_alloc {raw = false}, [tuple2Ty])
      val r_pos1 = ShallowFlatten.doesPolicyFlattenStatement policy s_pos1

      (* Positive Case 2: Vector_length on 2-tuple (matches MaxWidth 2) *)
      val s_pos2 = makePrimAppStatement (Prim.Vector_length, [tuple2Ty])
      val r_pos2 = ShallowFlatten.doesPolicyFlattenStatement policy s_pos2

      (* Positive Case 3 (Exception check): Array_uninitIsNop on 2-tuple array *)
      val s_pos3 = makePrimAppStatement (Prim.Array_uninitIsNop, [tuple2Ty])
      val r_pos3 = ShallowFlatten.doesPolicyFlattenStatement policy s_pos3

      (* Negative Case 1: Non-PrimApp (Const) *)
      val s_neg1 = Statement.T {
         exp = Exp.Const (Const.IntInf 1),
         ty = intTy,
         var = SOME vVar
      }
      val r_neg1 = ShallowFlatten.doesPolicyFlattenStatement policy s_neg1

      (* Negative Case 2: Non-Array/Vector PrimApp (Ref_ref) *)
      val s_neg2 = makePrimAppStatement (Prim.Ref_ref, [tuple2Ty])
      val r_neg2 = ShallowFlatten.doesPolicyFlattenStatement policy s_neg2

      (* Negative Case 3: Array_alloc on non-tuple *)
      val s_neg3 = makePrimAppStatement (Prim.Array_alloc {raw = false}, [intTy])
      val r_neg3 = ShallowFlatten.doesPolicyFlattenStatement policy s_neg3

      (* Negative Case 4: Array_alloc on 3-tuple (width 3 > MaxWidth 2) *)
      val s_neg4 = makePrimAppStatement (Prim.Array_alloc {raw = false}, [tuple3Ty])
      val r_neg4 = ShallowFlatten.doesPolicyFlattenStatement policy s_neg4

      (* Negative Case 5: Array_uninitIsNop on non-tuple array *)
      val s_neg5 = makePrimAppStatement (Prim.Array_uninitIsNop, [intTy])
      val r_neg5 = ShallowFlatten.doesPolicyFlattenStatement policy s_neg5

      (* Negative Case 6: Array_uninitIsNop on 3-tuple array (width 3 > MaxWidth 2) *)
      val s_neg6 = makePrimAppStatement (Prim.Array_uninitIsNop, [tuple3Ty])
      val r_neg6 = ShallowFlatten.doesPolicyFlattenStatement policy s_neg6
   in
      assert (r_pos1 = true, "r_pos1: Array_alloc on 2-tuple should flatten");
      assert (r_pos2 = true, "r_pos2: Vector_length on 2-tuple should flatten");
      assert (r_pos3 = true, "r_pos3: Array_uninitIsNop on 2-tuple should flatten");
      assert (r_neg1 = false, "r_neg1: Const should not flatten");
      assert (r_neg2 = false, "r_neg2: Ref_ref should not flatten");
      assert (r_neg3 = false, "r_neg3: Array_alloc on int should not flatten");
      assert (r_neg4 = false, "r_neg4: Array_alloc on 3-tuple should not flatten");
      assert (r_neg5 = false, "r_neg5: Array_uninitIsNop on int should not flatten");
      assert (r_neg6 = false, "r_neg6: Array_uninitIsNop on 3-tuple should not flatten")
   end)

(* Test 7: deepFlattenStatementsForPolicy *)
   val _ = runTest ("Test 7: deepFlattenStatementsForPolicy", fn () => let
      val policy = ShallowFlatten.MaxWidth 2
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val arrVar = Var.fromString "arr"
      val nVar = Var.fromString "n"

      (* 1. Case 1: Flattenable statement
         arr: (int * int) array = Array_alloc[int * int](n)
       *)
      val s1 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.new1 nVar,
            prim = Prim.Array_alloc {raw = false},
            targs = Vector.new1 tuple2Ty
         },
         ty = arrayTuple2Ty,
         var = SOME arrVar
      }
      
      val res1 = ShallowFlatten.deepFlattenStatementsForPolicy policy s1
      
      (* Assertions on res1 *)
      val _ = assert (Vector.length res1 = 3, "res1 length should be 3")
      
      (* Statement 0: arr_a: int array = Array_alloc[int](n) *)
      val Statement.T {exp = e1_0, ty = t1_0, var = v1_0} = Vector.sub (res1, 0)
      val _ = assertType (Vector.sub (res1, 0), Type.array intTy, arrayTuple2Ty, "res1[0] type")
      val _ = assert (Option.isSome v1_0, "res1[0] var should be SOME")
      val _ = case e1_0 of
                 Exp.PrimApp {prim = Prim.Array_alloc {raw = false}, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), nVar), "res1[0] args");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "res1[0] targs")
                 )
               | _ => raise TestFail "res1[0] should be Array_alloc PrimApp"

      (* Statement 1: arr_b: int array = Array_alloc[int](n) *)
      val Statement.T {exp = e1_1, ty = t1_1, var = v1_1} = Vector.sub (res1, 1)
      val _ = assertType (Vector.sub (res1, 1), Type.array intTy, arrayTuple2Ty, "res1[1] type")
      val _ = assert (Option.isSome v1_1, "res1[1] var should be SOME")
      val _ = case e1_1 of
                 Exp.PrimApp {prim = Prim.Array_alloc {raw = false}, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), nVar), "res1[1] args");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "res1[1] targs")
                 )
               | _ => raise TestFail "res1[1] should be Array_alloc PrimApp"

      (* Statement 2: arr: int array * int array = tuple (arr_a, arr_b) *)
      val Statement.T {exp = e1_2, ty = t1_2, var = v1_2} = Vector.sub (res1, 2)
      val expectedTupleTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val _ = assertType (Vector.sub (res1, 2), expectedTupleTy, arrayTuple2Ty, "res1[2] type")
      val _ = case v1_2 of
                 SOME v => assert (Var.equals (v, arrVar), "res1[2] var should be original arrVar")
               | NONE => raise TestFail "res1[2] var should be SOME"
      val _ = case e1_2 of
                 Exp.Tuple vs => (
                    assert (Vector.length vs = 2, "res1[2] tuple length");
                    assert (Var.equals (Vector.sub (vs, 0), valOf v1_0), "res1[2] tuple element 0");
                    assert (Var.equals (Vector.sub (vs, 1), valOf v1_1), "res1[2] tuple element 1")
                 )
               | _ => raise TestFail "res1[2] should be Tuple expression"

      (* 2. Case 2: Non-flattenable statement, with type transformation required
         arr: (int * int) array array = Array_alloc[(int * int) array](n)
       *)
      val s2 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.new1 nVar,
            prim = Prim.Array_alloc {raw = false},
            targs = Vector.new1 arrayTuple2Ty
         },
         ty = Type.array arrayTuple2Ty,
         var = SOME arrVar
      }
      
      val res2 = ShallowFlatten.deepFlattenStatementsForPolicy policy s2
      
      val _ = assert (Vector.length res2 = 3, "res2 length should be 3")
      val Statement.T {exp = e2_0, ty = t2_0, var = v2_0} = Vector.sub (res2, 0)
      val Statement.T {exp = e2_1, ty = t2_1, var = v2_1} = Vector.sub (res2, 1)
      val Statement.T {exp = e2_2, ty = t2_2, var = v2_2} = Vector.sub (res2, 2)
      
      val expectedLHSTy = Type.tuple (Vector.fromList [Type.array (Type.array intTy), Type.array (Type.array intTy)])
      
      val _ = assertType (Vector.sub (res2, 0), Type.array (Type.array intTy), Type.array arrayTuple2Ty, "res2[0] type")
      val _ = assert (Option.isSome v2_0, "res2[0] var should be SOME")
      val _ = case e2_0 of
                 Exp.PrimApp {prim = Prim.Array_alloc {raw = false}, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), nVar), "res2[0] args");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), Type.array intTy), "res2[0] targs")
                 )
               | _ => raise TestFail "res2[0] should be Array_alloc PrimApp"

      val _ = assertType (Vector.sub (res2, 1), Type.array (Type.array intTy), Type.array arrayTuple2Ty, "res2[1] type")
      val _ = assert (Option.isSome v2_1, "res2[1] var should be SOME")
      val _ = case e2_1 of
                 Exp.PrimApp {prim = Prim.Array_alloc {raw = false}, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), nVar), "res2[1] args");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), Type.array intTy), "res2[1] targs")
                 )
               | _ => raise TestFail "res2[1] should be Array_alloc PrimApp"

      val _ = assertType (Vector.sub (res2, 2), expectedLHSTy, Type.array arrayTuple2Ty, "res2[2] type")
      val _ = case v2_2 of
                 SOME v => assert (Var.equals (v, arrVar), "res2[2] var should be original arrVar")
               | NONE => raise TestFail "res2[2] var should be SOME"
      val _ = case e2_2 of
                 Exp.Tuple vs => (
                    assert (Vector.length vs = 2, "res2[2] tuple length");
                    assert (Var.equals (Vector.sub (vs, 0), valOf v2_0), "res2[2] tuple element 0");
                    assert (Var.equals (Vector.sub (vs, 1), valOf v2_1), "res2[2] tuple element 1")
                 )
               | _ => raise TestFail "res2[2] should be Tuple expression"
   in () end)

   val _ = summarize ()
end
