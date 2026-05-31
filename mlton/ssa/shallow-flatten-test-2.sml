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
in
(* Test 4: maybeFlattenType *)
   val _ = runTest ("Test 4: maybeFlattenType", fn () => let
      fun check (input, expected, msg) =
         let
            val res = ShallowFlatten.maybeFlattenType input
         in
            case (res, expected) of
               (NONE, NONE) => ()
             | (SOME r, SOME e) => 
               if Type.equals (r, e) then ()
               else assert (false, msg ^ ": type mismatch")
             | (SOME _, NONE) => assert (false, msg ^ ": expected NONE, got SOME")
             | (NONE, SOME _) => assert (false, msg ^ ": expected SOME, got NONE")
         end

      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val expected2 = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])

      val tuple3Ty = Type.tuple (Vector.fromList [intTy, intTy, intTy])
      val arrayTuple3Ty = Type.array tuple3Ty
      val expected3 = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy, Type.array intTy])

      val nestedTupleTy = Type.tuple (Vector.fromList [intTy, tuple2Ty])
      val arrayNestedTupleTy = Type.array nestedTupleTy
      val expectedNested = Type.tuple (Vector.fromList [Type.array intTy, Type.array tuple2Ty])
   in
      check (arrayTuple2Ty, SOME expected2, "simple 2-tuple array");
      check (arrayTuple3Ty, SOME expected3, "simple 3-tuple array");
      check (arrayNestedTupleTy, SOME expectedNested, "nested tuple array");
      check (intTy, NONE, "not an array");
      check (Type.array intTy, NONE, "array of non-tuple");
      check (tuple2Ty, NONE, "tuple but not array")
   end)(* Test 5: flattenedVars *)
   val _ = runTest ("Test 5: flattenedVars", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      
      val _ = assert (not (ShallowFlatten.isMarkedForFlatten (fv, v1)), "v1 should not be marked initially")
      val _ = ShallowFlatten.markForFlatten (fv, v1)
      val _ = assert (ShallowFlatten.isMarkedForFlatten (fv, v1), "v1 should be marked after markForFlatten")
      val _ = assert (not (ShallowFlatten.isMarkedForFlatten (fv, v2)), "v2 should not be marked")
   in () end)(* Test 6: markedCount *)
   val _ = runTest ("Test 6: markedCount", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val v3 = Var.fromString "v3"

      val _ = assert (ShallowFlatten.markedCount fv = 0, "Initial count should be 0")
      
      val _ = ShallowFlatten.markForFlatten (fv, v1)
      val _ = assert (ShallowFlatten.markedCount fv = 1, "Count should be 1 after marking v1")
      
      val _ = ShallowFlatten.markForFlatten (fv, v2)
      val _ = assert (ShallowFlatten.markedCount fv = 2, "Count should be 2 after marking v2")

      (* Re-marking the same variable is illegal *)
      
      val _ = ShallowFlatten.markForFlatten (fv, v3)
      val _ = assert (ShallowFlatten.markedCount fv = 3, "Count should be 3 after marking v3")
   in () end)(* Test 7: maybeFlattenArg *)
   val _ = runTest ("Test 7: maybeFlattenArg", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val v3 = Var.fromString "v3"
      
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val expected2 = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])

      val policyFlatten = ShallowFlatten.MaxWidth 2
      val policyPreserve = ShallowFlatten.MaxWidth 0

      (* Case 1: Not marked for flattening *)
      val _ = ShallowFlatten.setArgFlatteningDecision (fv, v1, ShallowFlatten.getConDecisionForPolicy policyPreserve arrayTuple2Ty)
      val (rv1, rt1) = ShallowFlatten.maybeFlattenArg (fv, (v1, arrayTuple2Ty))
      val _ = assert (Var.equals (rv1, v1), "Case 1: var mismatch")
      val _ = assert (Type.equals (rt1, arrayTuple2Ty), "Case 1: type mismatch")

      (* Case 2: Marked for flattening, valid type *)
      val _ = ShallowFlatten.setArgFlatteningDecision (fv, v2, ShallowFlatten.getConDecisionForPolicy policyFlatten arrayTuple2Ty)
      val (rv2, rt2) = ShallowFlatten.maybeFlattenArg (fv, (v2, arrayTuple2Ty))
      val _ = assert (Var.equals (rv2, v2), "Case 2: var mismatch")
      val _ = assert (Type.equals (rt2, expected2), "Case 2: type mismatch")

      (* Case 3: Marked for flattening, invalid type *)
      val _ = ShallowFlatten.setArgFlatteningDecision (fv, v3, ShallowFlatten.FlattenNode (Vector.fromList []))
      val _ = (ShallowFlatten.maybeFlattenArg (fv, (v3, intTy)); 
               assert (false, "Case 3: should have raised BadFlattenError"))
              handle ShallowFlatten.BadFlattenError => ()
                   | _ => assert (false, "Case 3: raised wrong exception")
   in () end)(* Test 8: maybeFlattenStatement *)
   val _ = runTest ("Test 8: maybeFlattenStatement (Array_length)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val allocPrim = Prim.Array_alloc {raw = false}

      (* Case 1: Non-flattenable statement (Const) *)
      val s1 = Statement.T {exp = Exp.Const (Const.IntInf 1), ty = intTy, var = SOME v1}
      val res1 = ShallowFlatten.maybeFlattenStatement s1
      val _ = case res1 of
                 SOME ss => assert (Vector.length ss = 1 andalso statementEquals (s1, Vector.sub (ss, 0)), 
                                   "s1 should return SOME (original)")
               | NONE => raise TestFail "s1 should return SOME (original)"

      (* Case 2: Array_alloc on non-tuple type *)
      val s2 = Statement.T {
         exp = primApp (allocPrim, [n], [intTy]),
         ty = Type.array intTy,
         var = SOME v1
      }
      val _ = assert (Option.isNone (ShallowFlatten.maybeFlattenStatement s2), "s2 should not be flattenable")

      (* Case 4: Array_length on tuple type *)
      val lengthPrim = Prim.Array_length
      val arr = Var.fromString "arr"
      val s4 = Statement.T {
         exp = primApp (lengthPrim, [arr], [tuple2Ty]),
         ty = intTy,
         var = SOME v1
      }
      val res4 = ShallowFlatten.maybeFlattenStatement s4
      val stmts4 = case res4 of
                      SOME s => s
                    | NONE => raise TestFail "s4 should be flattenable"
      val _ = assert (Vector.length stmts4 = 2, "s4 should flatten to 2 statements")
      val _ = assertType (Vector.sub (stmts4, 0), Type.array intTy, "s4 stmt 0 type")
      val _ = assertType (Vector.sub (stmts4, 1), intTy, "s4 stmt 1 type")
   in () end)(* Test 8b: maybeFlattenStatement (non-PrimApp always returns SOME original) *)
   val _ = runTest ("Test 8b: maybeFlattenStatement (non-PrimApp always returns SOME original)", fn () => let
      val v1 = Var.fromString "v1"
      val x = Var.fromString "x"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val vectorTuple2Ty = Type.vector tuple2Ty

      fun checkSome (s, msg) =
         case ShallowFlatten.maybeFlattenStatement s of
            SOME ss => 
               if Vector.length ss = 1 then
                  let val s' = Vector.sub (ss, 0)
                  in if statementEquals (s, s') then ()
                     else raise TestFail (msg ^ ": statement changed")
                  end
               else raise TestFail (msg ^ ": expected 1 statement")
          | NONE => raise TestFail (msg ^ ": expected SOME, got NONE")

      (* Exp.Const *)
      val s_const = Statement.T {exp = Exp.Const (Const.IntInf 1), ty = arrayTuple2Ty, var = SOME v1}
      val _ = checkSome (s_const, "Const with array-of-tuple type")

      (* Exp.Var *)
      val s_var = Statement.T {exp = Exp.Var x, ty = arrayTuple2Ty, var = SOME v1}
      val _ = checkSome (s_var, "Var with array-of-tuple type")

      (* Exp.Tuple *)
      val s_tuple = Statement.T {exp = Exp.Tuple (Vector.new1 x), ty = arrayTuple2Ty, var = SOME v1}
      val _ = checkSome (s_tuple, "Tuple with array-of-tuple type")

      (* Exp.Select *)
      val s_select = Statement.T {exp = Exp.Select {offset = 0, tuple = x}, ty = arrayTuple2Ty, var = SOME v1}
      val _ = checkSome (s_select, "Select with array-of-tuple type")

      (* Exp.Profile *)
      val s_profile = Statement.T {exp = Exp.Profile (ProfileExp.Enter SourceInfo.unknown), ty = Type.unit, var = NONE}
      val _ = checkSome (s_profile, "Profile")
      
      (* Check Vector as well *)
      val s_vec_const = Statement.T {exp = Exp.Const (Const.IntInf 1), ty = vectorTuple2Ty, var = SOME v1}
      val _ = checkSome (s_vec_const, "Const with vector-of-tuple type")

   in () end)(* Test 9: maybeFlattenStatement (Array_alloc) *)
   val _ = runTest ("Test 9: maybeFlattenStatement (Array_alloc)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val allocPrim = Prim.Array_alloc {raw = false}

      (* Case 3: Array_alloc on tuple type *)
      val s3 = Statement.T {
         exp = primApp (allocPrim, [n], [tuple2Ty]),
         ty = arrayTuple2Ty,
         var = SOME v1
      }
      val res3 = ShallowFlatten.maybeFlattenStatement s3
      val stmts = case res3 of
                     SOME s => s
                   | NONE => raise TestFail "s3 should be flattenable"
      val _ = assert (Vector.length stmts = 3,
                      concat ["s3 should flatten to 3 statements, got ",
                              Int.toString (Vector.length stmts)])
      val _ = assertType (Vector.sub (stmts, 0), Type.array intTy, "s3 stmt 0 type")
      val _ = assertType (Vector.sub (stmts, 1), Type.array intTy, "s3 stmt 1 type")
      val _ = assertType (Vector.sub (stmts, 2), 
                          Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy]), 
                          "s3 stmt 2 type")
   in () end)(* Test 10: maybeFlattenStatement (Array_sub) *)
   val _ = runTest ("Test 10: maybeFlattenStatement (Array_sub)", fn () => let
      val v1 = Var.fromString "v1"
      val arr = Var.fromString "arr"
      val i = Var.fromString "i"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])

      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val subPrim = Prim.Array_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arr, i], [tuple2Ty]),
         ty = tuple2Ty,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatement s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_sub should be flattenable"
      
      (* Expected:
         1. arr_a = select(arr, 0)
         2. x_a = Array_sub(arr_a, i)
         3. arr_b = select(arr, 1)
         4. x_b = Array_sub(arr_b, i)
         5. v1 = tuple(x_a, x_b)
      *)
      val _ = assert (Vector.length stmts = 5, "Array_sub should flatten to 5 statements")
      val _ = assertType (Vector.sub (stmts, 0), Type.array intTy, "Array_sub stmt 0 type")
      val _ = assertType (Vector.sub (stmts, 1), Type.array intTy, "Array_sub stmt 1 type")
      val _ = assertType (Vector.sub (stmts, 2), intTy, "Array_sub stmt 2 type")
      val _ = assertType (Vector.sub (stmts, 3), intTy, "Array_sub stmt 3 type")
      val _ = assertType (Vector.sub (stmts, 4), tuple2Ty, "Array_sub stmt 4 type")
      val _ = Vector.foreach (stmts, fn Statement.T {exp, ty, var} =>
         case exp of
            Exp.Select {offset, tuple} => assert (Var.equals (tuple, arr), "Select should be from arr")
          | Exp.PrimApp {prim, ...} => 
            (case prim of
                Prim.Array_sub => ()
              | _ => assert (false, "Expected Array_sub or Select or Tuple"))
          | Exp.Tuple _ => ()
          | _ => assert (false, "Unexpected expression in flattened Array_sub"))
   in () end)(* Test 10v: maybeFlattenStatement (Vector_sub) *)
   val _ = runTest ("Test 10v: maybeFlattenStatement (Vector_sub)", fn () => let
      val v1 = Var.fromString "v1"
      val vec = Var.fromString "vec"
      val i = Var.fromString "i"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])

      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val subPrim = Prim.Vector_sub
      val s = Statement.T {
         exp = primApp (subPrim, [vec, i], [tuple2Ty]),
         ty = tuple2Ty,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatement s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Vector_sub should be flattenable"
      
      (* Expected:
         1. vec_a = select(vec, 0)
         2. x_a = Vector_sub(vec_a, i)
         3. vec_b = select(vec, 1)
         4. x_b = Vector_sub(vec_b, i)
         5. v1 = tuple(x_a, x_b)
      *)
      val _ = assert (Vector.length stmts = 5, "Vector_sub should flatten to 5 statements")
      val _ = assertType (Vector.sub (stmts, 0), Type.vector intTy, "Vector_sub stmt 0 type")
      val _ = assertType (Vector.sub (stmts, 1), Type.vector intTy, "Vector_sub stmt 1 type")
      val _ = assertType (Vector.sub (stmts, 2), intTy, "Vector_sub stmt 2 type")
      val _ = assertType (Vector.sub (stmts, 3), intTy, "Vector_sub stmt 3 type")
      val _ = assertType (Vector.sub (stmts, 4), tuple2Ty, "Vector_sub stmt 4 type")
      val _ = Vector.foreach (stmts, fn Statement.T {exp, ty, var} =>
         case exp of
            Exp.Select {offset, tuple} => assert (Var.equals (tuple, vec), "Select should be from vec")
          | Exp.PrimApp {prim, args, ...} => 
            (case prim of
                Prim.Vector_sub => 
                   assert (Vector.length args = 2, "Vector_sub should have 2 arguments")
              | _ => assert (false, "Expected Vector_sub or Select or Tuple"))
          | Exp.Tuple _ => ()
          | _ => assert (false, "Unexpected expression in flattened Vector_sub"))
   in () end)(* Test 11: maybeFlattenStatement (Array_update) *)
   val _ = runTest ("Test 11: maybeFlattenStatement (Array_update)", fn () => let
      val arr = Var.fromString "arr"
      val i = Var.fromString "i"
      val x = Var.fromString "x"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])

      fun assertType (Statement.T {ty, ...}, expected, msg) =
         if Type.equals (ty, expected) then ()
         else assert (false, msg ^ ": type mismatch (got " ^ (Layout.toString (Type.layout ty)) ^ ")")

      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val updatePrim = Prim.Array_update
      val s = Statement.T {
         exp = primApp (updatePrim, [arr, i, x], [tuple2Ty]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatement s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_update should be flattenable"
      
      (* Expected:
         1. arr_a = select(arr, 0)
         2. x_a = select(x, 0)
         3. _ = Array_update(arr_a, i, x_a)
         4. arr_b = select(arr, 1)
         5. x_b = select(x, 1)
         6. _ = Array_update(arr_b, i, x_b)
      *)
      val _ = assert (Vector.length stmts = 6, "Array_update should flatten to 6 statements")
      val _ = assertType (Vector.sub (stmts, 0), Type.array intTy, "Array_update stmt 0 type")
      val _ = assertType (Vector.sub (stmts, 1), Type.array intTy, "Array_update stmt 1 type")
      val _ = assertType (Vector.sub (stmts, 2), intTy, "Array_update stmt 2 type")
      val _ = assertType (Vector.sub (stmts, 3), intTy, "Array_update stmt 3 type")
      val _ = assertType (Vector.sub (stmts, 4), Type.unit, "Array_update stmt 4 type")
      val _ = assertType (Vector.sub (stmts, 5), Type.unit, "Array_update stmt 5 type")
      val _ = Vector.foreach (stmts, fn Statement.T {exp, ty, var} =>
         case exp of
            Exp.Select {offset, tuple} => 
            assert (Var.equals (tuple, arr) orelse Var.equals (tuple, x), "Select should be from arr or x")
          | Exp.PrimApp {prim, ...} => 
            (case prim of
                Prim.Array_update => ()
              | _ => assert (false, "Expected Array_update or Select"))
          | _ => assert (false, "Unexpected expression in flattened Array_update"))
   in () end)(* Test 11b: maybeFlattenStatement (Array_uninitIsNop) *)
   val _ = runTest ("Test 11b: maybeFlattenStatement (Array_uninitIsNop)", fn () => let
      val arr = Var.fromString "arr"
      val v1 = Var.fromString "v1"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val boolTy = Type.bool

      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val uninitIsNopPrim = Prim.Array_uninitIsNop
      val s = Statement.T {
         exp = primApp (uninitIsNopPrim, [arr], [tuple2Ty]),
         ty = boolTy,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatement s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_uninitIsNop should be flattenable"
      
      (* Expected:
         1. v1 = false
      *)
      val _ = assert (Vector.length stmts = 1, "Array_uninitIsNop should flatten to 1 statement")
      val stmt0 = Vector.sub (stmts, 0)
      val _ = assertType (stmt0, boolTy, "stmt0 type")
      val Statement.T {exp, var, ...} = stmt0
      val _ = case var of
                 SOME v => assert (Var.equals (v, v1), "v1 mismatch")
               | NONE => raise TestFail "expected SOME var"
      val _ = case exp of
                 Exp.ConApp {con, args} =>
                 assert (Con.equals (con, Con.falsee) andalso Vector.length args = 0, "expected Con.falsee")
               | _ => raise TestFail "expected ConApp false"
   in () end)(* Test 11c: maybeFlattenStatement (Array_toArray) *)
   val _ = runTest ("Test 11c: maybeFlattenStatement (Array_toArray)", fn () => let
      val arr = Var.fromString "arr"
      val v1 = Var.fromString "v1"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}

      val s = Statement.T {
         exp = primApp (Prim.Array_toArray, [arr], [tuple2Ty]),
         ty = arrayTuple2Ty,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatement s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_toArray should be flattenable"
      
      val _ = assert (Vector.length stmts = 5, "Array_toArray should flatten to 5 statements")
      
      val selectStmts = Vector.tabulate (2, fn i => Vector.sub (stmts, i))
      val _ = Vector.foreach (selectStmts, fn stmt =>
         case stmt of
            Statement.T {exp = Exp.Select {offset, tuple}, ty, var = SOME _} =>
            let
               val _ = assert (Var.equals (tuple, arr), "offset tuple mismatch")
               val _ = assert (offset = 0 orelse offset = 1, "offset mismatch")
               val _ = assert (Type.equals (ty, Type.array intTy), "select type mismatch")
            in () end
          | _ => raise TestFail "Expected Select statement with SOME var")

      val arrStmts = Vector.tabulate (2, fn i => Vector.sub (stmts, 2 + i))
      val _ = Vector.foreach (arrStmts, fn stmt =>
         case stmt of
            Statement.T {exp = Exp.PrimApp {args, prim, targs}, ty, var = SOME _} =>
            let
               val _ = case prim of
                          Prim.Array_toArray => ()
                        | _ => raise TestFail "Expected Array_toArray primitive"
               val _ = assert (Vector.length args = 1, "Array_toArray should have 1 argument")
               val _ = assert (Vector.length targs = 1, "Array_toArray should have 1 targ")
               val _ = assert (Type.equals (Vector.sub (targs, 0), intTy), "targ mismatch")
               val _ = assert (Type.equals (ty, Type.array intTy), "array type mismatch")
            in () end
          | _ => raise TestFail "Expected PrimApp statement with SOME var")

      val tupleStmt = Vector.sub (stmts, 4)
      val _ = case tupleStmt of
                 Statement.T {exp = Exp.Tuple args, ty, var = SOME v} =>
                 let
                    val _ = assert (Var.equals (v, v1), "v1 mismatch")
                    val _ = assert (Vector.length args = 2, "Tuple should have 2 arguments")
                    val expectedTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
                    val _ = assert (Type.equals (ty, expectedTy), "tuple type mismatch")
                 in () end
               | _ => raise TestFail "Expected Tuple statement with SOME var"
   in () end)(* Test 12: mustFlattenStatement *)
   val _ = runTest ("Test 12: mustFlattenStatement", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val v3 = Var.fromString "v3"
      val intTy = Type.intInf
      
      (* Case 1: Defines marked variable *)
      val _ = ShallowFlatten.markForFlatten (fv, v1)
      val s1 = Statement.T {exp = Exp.Const (Const.IntInf 1), ty = intTy, var = SOME v1}
      val _ = assert (ShallowFlatten.mustFlattenStatement (fv, s1), "Case 1: should be true (defines v1)")
      
      (* Case 2: Uses marked variable *)
      val s2 = Statement.T {exp = Exp.Var v1, ty = intTy, var = SOME v2}
      val _ = assert (ShallowFlatten.mustFlattenStatement (fv, s2), "Case 2: should be true (uses v1)")
      
      (* Case 3: Neither defines nor uses marked variable *)
      val s3 = Statement.T {exp = Exp.Var v2, ty = intTy, var = SOME v3}
      val _ = assert (not (ShallowFlatten.mustFlattenStatement (fv, s3)), "Case 3: should be false")
      
      (* Case 4: Statement with NONE var, but uses marked variable *)
      val s4 = Statement.T {exp = Exp.Var v1, ty = intTy, var = NONE}
      val _ = assert (ShallowFlatten.mustFlattenStatement (fv, s4), "Case 4: should be true (uses v1, var=NONE)")

      (* Case 5: Complex expression using marked variable *)
      val s5 = Statement.T {
         exp = Exp.Tuple (Vector.fromList [v2, v1]),
         ty = Type.tuple (Vector.fromList [intTy, intTy]),
         var = SOME v3
      }
      val _ = assert (ShallowFlatten.mustFlattenStatement (fv, s5), "Case 5: should be true (uses v1 in tuple)")
   in () end)(* Test 13: markStatementForPolicy *)
   val _ = runTest ("Test 13: markStatementForPolicy", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      fun primApp (p, args, targs) = 
         Exp.PrimApp {args = Vector.fromList args,
                      prim = p,
                      targs = Vector.fromList targs}
      val allocPrim = Prim.Array_alloc {raw = false}

      (* Policy: Flatten if tuple width <= 2 *)
      val policy = ShallowFlatten.MaxWidth 2

      (* Case 1: Array of 2-tuple. Should be marked. *)
      val s1 = Statement.T {
         exp = primApp (allocPrim, [n], [tuple2Ty]),
         ty = arrayTuple2Ty,
         var = SOME v1
      }
      val _ = ShallowFlatten.markStatementForPolicy (fv, policy) s1
      val _ = assert (ShallowFlatten.isMarkedForFlatten (fv, v1), "Case 1: v1 should be marked")

      (* Case 2: Array of 4-tuple. Should NOT be marked (MaxWidth 3). *)
      val v2 = Var.fromString "v2"
      val tuple4Ty = Type.tuple (Vector.tabulate (4, fn _ => intTy))
      val s2 = Statement.T {
         exp = primApp (allocPrim, [n], [tuple4Ty]),
         ty = Type.array tuple4Ty,
         var = SOME v2
      }
      val _ = ShallowFlatten.markStatementForPolicy (fv, policy) s2
      val _ = assert (not (ShallowFlatten.isMarkedForFlatten (fv, v2)), "Case 2: v2 should NOT be marked")

      (* Case 4: Array of 3-tuple. Should NOT be marked (MaxWidth 2, 3 <= 2 is false). *)
      val v4 = Var.fromString "v4"
      val tuple3Ty = Type.tuple (Vector.tabulate (3, fn _ => intTy))
      val s4 = Statement.T {
         exp = primApp (allocPrim, [n], [tuple3Ty]),
         ty = Type.array tuple3Ty,
         var = SOME v4
      }
      val _ = ShallowFlatten.markStatementForPolicy (fv, policy) s4
      val _ = assert (not (ShallowFlatten.isMarkedForFlatten (fv, v4)), "Case 4: v4 should NOT be marked")

      (* Case 3: Non-array binding. Should NOT be marked. *)
      val v3 = Var.fromString "v3"
      val s3 = Statement.T {
         exp = Exp.Const (Const.IntInf 1),
         ty = intTy,
         var = SOME v3
      }
      val _ = ShallowFlatten.markStatementForPolicy (fv, policy) s3
      val _ = assert (not (ShallowFlatten.isMarkedForFlatten (fv, v3)), "Case 3: v3 should NOT be marked")
   in () end)(* Test 14: markArgForPolicy *)
   val _ = runTest ("Test 14: markArgForPolicy", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val intTy = Type.intInf
      val policy = ShallowFlatten.MaxWidth 2

      fun isFlattened v =
          case ShallowFlatten.getArgFlatteningDecision (fv, v) of
              ShallowFlatten.FlattenNode _ => true
            | _ => false

      (* Case 1: Array of 2-tuple argument. Should be marked. *)
      val v1 = Var.fromString "v1"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val arg1 = (v1, arrayTuple2Ty)
      val _ = ShallowFlatten.markArgForPolicy (fv, policy) arg1
      val _ = assert (isFlattened v1, "Case 1: v1 (arg) should be marked")

      (* Case 2: Array of 4-tuple argument. Should NOT be marked (MaxWidth 2). *)
      val v2 = Var.fromString "v2"
      val tuple4Ty = Type.tuple (Vector.tabulate (4, fn _ => intTy))
      val arrayTuple4Ty = Type.array tuple4Ty
      val arg2 = (v2, arrayTuple4Ty)
      val _ = ShallowFlatten.markArgForPolicy (fv, policy) arg2
      val _ = assert (not (isFlattened v2), "Case 2: v2 (arg) should NOT be marked")

      (* Case 3: Non-array argument. Should NOT be marked. *)
      val v3 = Var.fromString "v3"
      val arg3 = (v3, intTy)
      val _ = ShallowFlatten.markArgForPolicy (fv, policy) arg3
      val _ = assert (not (isFlattened v3), "Case 3: v3 (arg) should NOT be marked")

      (* Case 4: Array of 3-tuple argument. Should NOT be marked (MaxWidth 2). *)
      val v4 = Var.fromString "v4"
      val tuple3Ty = Type.tuple (Vector.tabulate (3, fn _ => intTy))
      val arrayTuple3Ty = Type.array tuple3Ty
      val arg4 = (v4, arrayTuple3Ty)
      val _ = ShallowFlatten.markArgForPolicy (fv, policy) arg4
      val _ = assert (not (isFlattened v4), "Case 4: v4 (arg) should NOT be marked")
   in () end)(* Test 15: flattenOnce (no flattening needed) *)
   val _ = runTest ("Test 15: flattenOnce (no flattening needed)", fn () => let
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
      val policy = ShallowFlatten.MaxWidth 3
      val res = ShallowFlatten.flattenOnce policy p
   in
      assert (Option.isNone res, "Should return NONE when no flattening is possible")
   end)
      val _ = summarize ()
end
