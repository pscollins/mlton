local
   open Ssa

   fun assert (cond, msg) =
      if cond then () else raise TestFail msg

   fun decisionEquals (d1, d2) =
      case (d1, d2) of
         (ShallowFlatten.PreserveNode v1, ShallowFlatten.PreserveNode v2) =>
         Vector.length v1 = Vector.length v2 andalso
         Vector.forall2 (v1, v2, decisionEquals)
       | (ShallowFlatten.FlattenNode v1, ShallowFlatten.FlattenNode v2) =>
         Vector.length v1 = Vector.length v2 andalso
         Vector.forall2 (v1, v2, decisionEquals)
       | _ => false
in
   val _ = runTest ("Test 7: set/getArgFlatteningDecision", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val v = Var.newString "v"
      
      (* Create a conDecision *)
      val decision = ShallowFlatten.FlattenNode (Vector.new0 ())
      
      (* Set the decision *)
      val _ = ShallowFlatten.setArgFlatteningDecision (fv, v, decision)
      
      (* Get the decision *)
      val decision' = ShallowFlatten.getArgFlatteningDecision (fv, v)
      
      (* Check equality *)
      val _ = if decisionEquals (decision, decision') then ()
              else assert (false, "Decision mismatch")

      val _ = ShallowFlatten.destroyFlattenedVars fv
   in () end)

   val _ = runTest ("Test 8: Nested array flattening", fn () => let
      val f_test = Func.fromString "f_test"
      val L_start = Label.fromString "L_start"
      
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val xyTy = Type.tuple (Vector.fromList [arrayTuple2Ty, intTy])
      
      val v_xy = Var.fromString "xy"
      val v_x = Var.fromString "x"
      val v_xlen = Var.fromString "xlen"
      
      val s1 = Statement.T {
         exp = Exp.Select {offset = 0, tuple = v_xy},
         ty = arrayTuple2Ty,
         var = SOME v_x
      }
      val s2 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v_x],
            prim = Prim.Array_length,
            targs = Vector.fromList [tuple2Ty]
         },
         ty = intTy,
         var = SOME v_xlen
      }
      
      val block = Block.T {
         args = Vector.new0 (),
         label = L_start,
         statements = Vector.fromList [s1, s2],
         transfer = Transfer.Return (Vector.fromList [v_xlen])
      }
      
      val func = Function.new {
         args = Vector.fromList [(v_xy, xyTy)],
         blocks = Vector.fromList [block],
         mayInline = true,
         name = f_test,
         raises = NONE,
         returns = SOME (Vector.fromList [intTy]),
         start = L_start
      }
      
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [func],
         globals = Vector.new0 (),
         main = f_test
      }
      
      val policy = ShallowFlatten.MaxWidth 2
      val p_opt = ShallowFlatten.flattenOnce policy p
      
      val _ = case p_opt of
         NONE => assert (false, "Should have flattened something")
       | SOME p' => let
            val Program.T {functions, ...} = p'
            val func' = case functions of
                           x :: _ => x
                         | _ => raise TestFail "No functions"
            val {args, blocks, ...} = Function.dest func'
            
            (* Check xy type *)
            val (_, ty_xy') = Vector.sub (args, 0)
            val expected_xy_ty = Type.tuple (Vector.fromList [
               Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy]),
               intTy
            ])
            val _ = assert (Type.equals (ty_xy', expected_xy_ty), "xy type should be flattened")
            
            val b = Vector.sub (blocks, 0)
            val stmts = Block.statements b
            
            (* Expected stmts:
               x = select(xy, 0)
               flatArr_0 = select(x, 0)
               xlen = Array_length[int](flatArr_0)
            *)
            val _ = assert (Vector.length stmts = 3, "Should have 3 statements")
            
            val s1' = Vector.sub (stmts, 0)
            val s2' = Vector.sub (stmts, 1)
            val s3' = Vector.sub (stmts, 2)
            
            val (s1_exp, s1_ty, s1_var) = let val Statement.T {exp, ty, var} = s1' in (exp, ty, var) end
            
            val _ = case (s1_exp, s1_ty) of
               (Exp.Select {offset=0, tuple}, ty) => (
                  assert (Var.equals (tuple, v_xy), "s1 tuple mismatch");
                  assert (Type.equals (ty, Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])), "s1 type mismatch")
               )
             | _ => assert (false, "s1 should be select")
            
            val (s2_exp, s2_var) = let val Statement.T {exp, var, ...} = s2' in (exp, var) end
            val _ = case s2_exp of
               Exp.Select {offset=0, tuple} => (
                  case s1_var of
                     SOME v_x' => assert (Var.equals (tuple, v_x'), "s2 tuple mismatch")
                   | NONE => assert (false, "s1 has no var")
               )
             | _ => assert (false, "s2 should be select")
             
            val s3_exp = let val Statement.T {exp, ...} = s3' in exp end
            val _ = case s3_exp of
               Exp.PrimApp {prim=Prim.Array_length, targs, args=args'} => (
                  assert (Vector.length targs = 1, "s3 targs length");
                  assert (Type.equals (Vector.sub (targs, 0), intTy), "s3 targ should be int");
                  assert (Vector.length args' = 1, "s3 args length");
                  case s2_var of
                     SOME v_x0 => assert (Var.equals (Vector.sub (args', 0), v_x0), "s3 arg mismatch")
                   | NONE => assert (false, "s2 has no var")
               )
             | _ => assert (false, "s3 should be Array_length")
         in () end
   in () end)

   (* Test 9: Ref_deref flattening
    *
    * Expected input IR:
    *   val x = prim Ref_deref [(intInf * intInf) array] (xy)
    *
    * Expected output IR (when flattened):
    *   val x = prim Ref_deref [(intInf array * intInf array)] (xy)
    *)
   val _ = runTest ("Test 9: Ref_deref flattening", fn () => let
      val f_test = Func.fromString "f_test"
      val L_start = Label.fromString "L_start"
      
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val refArrayTuple2Ty = Type.reff arrayTuple2Ty
      
      val v_xy = Var.fromString "xy"
      val v_x = Var.fromString "x"
      
      val s1 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v_xy],
            prim = Prim.Ref_deref,
            targs = Vector.fromList [arrayTuple2Ty]
         },
         ty = arrayTuple2Ty,
         var = SOME v_x
      }
      
      val block = Block.T {
         args = Vector.new0 (),
         label = L_start,
         statements = Vector.fromList [s1],
         transfer = Transfer.Return (Vector.fromList [v_x])
      }
      
      val func = Function.new {
         args = Vector.fromList [(v_xy, refArrayTuple2Ty)],
         blocks = Vector.fromList [block],
         mayInline = true,
         name = f_test,
         raises = NONE,
         returns = SOME (Vector.fromList [arrayTuple2Ty]),
         start = L_start
      }
      
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [func],
         globals = Vector.new0 (),
         main = f_test
      }
      
      val policy = ShallowFlatten.MaxWidth 2
      val p_opt = ShallowFlatten.flattenOnce policy p
      
      val _ = case p_opt of
         NONE => assert (false, "Should have flattened something")
       | SOME p' => let
            val Program.T {functions, ...} = p'
            val func' = case functions of
                           x :: _ => x
                         | _ => raise TestFail "No functions"
            val {args, blocks, ...} = Function.dest func'
            
            (* Check xy type: should be (int array * int array) ref *)
            val (_, ty_xy') = Vector.sub (args, 0)
            val expected_inner_ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
            val expected_xy_ty = Type.reff expected_inner_ty
            val _ = assert (Type.equals (ty_xy', expected_xy_ty), "xy type should be flattened")
            
            val b = Vector.sub (blocks, 0)
            val stmts = Block.statements b
            
            (* Expected stmts:
               x = Ref_deref(xy)
            *)
            val _ = assert (Vector.length stmts = 1, "Should have 1 statement")
            val s1' = Vector.sub (stmts, 0)
            val (s1_exp, s1_ty, s1_var) = let val Statement.T {exp, ty, var} = s1' in (exp, ty, var) end
            
            val _ = case s1_exp of
               Exp.PrimApp {prim=Prim.Ref_deref, targs, args=args'} => (
                  assert (Vector.length targs = 1, "s1 targs length");
                  assert (Type.equals (Vector.sub (targs, 0), expected_inner_ty), "s1 targ should be flattened");
                  assert (Vector.length args' = 1, "s1 args length");
                  assert (Var.equals (Vector.sub (args', 0), v_xy), "s1 arg mismatch")
               )
             | _ => assert (false, "s1 should be Ref_deref")
             
            val _ = assert (Type.equals (s1_ty, expected_inner_ty), "s1 return type should be flattened")
         in () end
   in () end)

   (* Test 10: Nested Ref_deref flattening
    *
    * Expected input IR:
    *   val x = prim Ref_deref [((intInf * intInf) array * intInf)] (xy)
    *
    * Expected output IR (when flattened):
    *   val x = prim Ref_deref [((intInf array * intInf array) * intInf)] (xy)
    *)
   val _ = runTest ("Test 10: Nested Ref_deref flattening", fn () => let
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val xy = Var.fromString "xy"
      val x = Var.fromString "x"
      
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val nestedTupleTy = Type.tuple (Vector.fromList [arrayTuple2Ty, intTy])
      val refNestedTupleTy = Type.reff nestedTupleTy

      val s1 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.new1 xy,
            prim = Prim.Ref_deref,
            targs = Vector.new1 nestedTupleTy
         },
         ty = nestedTupleTy,
         var = SOME x
      }

      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = L0,
         statements = Vector.new1 s1,
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainFunction = Function.new {
         args = Vector.new1 (xy, refNestedTupleTy),
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
      val p' = case ShallowFlatten.flattenOnce policy p of
                  SOME p' => p'
                | NONE => raise TestFail "Should have flattened"
      
      val Program.T {functions = funcs', ...} = p'
      val mainFunction' = List.first funcs'
      val {args = args', blocks = blocks', ...} = Function.dest mainFunction'
      
      (* Check xy arg type *)
      val (_, xyTy') = Vector.sub (args', 0)
      val flattenedArrayTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val expectedNestedTy = Type.tuple (Vector.fromList [flattenedArrayTy, intTy])
      val expectedRefTy = Type.reff expectedNestedTy
      
      val _ = if Type.equals (xyTy', expectedRefTy) then ()
              else assert (false, "xy type mismatch: " ^ (Layout.toString (Type.layout xyTy')) ^
                                 " expected " ^ (Layout.toString (Type.layout expectedRefTy)))

      (* Check x statement type *)
      val block' = Vector.sub (blocks', 0)
      val stmts' = Block.statements block'
      val x_stmt = Vector.sub (stmts', 0)
      val Statement.T {exp = x_exp, ty = x_ty, ...} = x_stmt
      
      val _ = if Type.equals (x_ty, expectedNestedTy) then ()
              else assert (false, "x type mismatch: " ^ (Layout.toString (Type.layout x_ty)) ^
                                 " expected " ^ (Layout.toString (Type.layout expectedNestedTy)))

      (* Check Ref_deref targs *)
      val _ = case x_exp of
                 Exp.PrimApp {prim = Prim.Ref_deref, targs, ...} =>
                 let
                    val targ = Vector.sub (targs, 0)
                 in
                    if Type.equals (targ, expectedNestedTy) then ()
                    else assert (false, "Ref_deref targ mismatch: " ^ (Layout.toString (Type.layout targ)) ^
                                       " expected " ^ (Layout.toString (Type.layout expectedNestedTy)))
                 end
               | _ => assert (false, "Not a PrimApp")
   in () end)

   val _ = runTest ("Test 11: Return type propagation bug", fn () => let
      val _ = Control.libTargetDir := "../../build/lib/mlton/targets/self"
      val f_test = Func.fromString "f_test"
      val L_start = Label.fromString "L_start"
      
      val intTy = Type.intInf
      val seqIndexTy = Type.word (Atoms.WordSize.seqIndex ())
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      val v_n = Var.fromString "n"
      val v_x = Var.fromString "x"
      
      val s0 = Statement.T {
         exp = Exp.Const (Const.word (Atoms.WordX.fromInt (1, Atoms.WordSize.seqIndex ()))),
         ty = seqIndexTy,
         var = SOME v_n
      }
      val s1 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v_n],
            prim = Prim.Array_alloc {raw = false},
            targs = Vector.fromList [tuple2Ty]
         },
         ty = arrayTuple2Ty,
         var = SOME v_x
      }
      
      val block = Block.T {
         args = Vector.new0 (),
         label = L_start,
         statements = Vector.fromList [s0, s1],
         transfer = Transfer.Return (Vector.fromList [v_x])
      }
      
      val func = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [block],
         mayInline = true,
         name = f_test,
         raises = NONE,
         returns = SOME (Vector.fromList [arrayTuple2Ty]),
         start = L_start
      }
      
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [func],
         globals = Vector.new0 (),
         main = f_test
      }
      
      val policy = ShallowFlatten.MaxWidth 2
      val p_opt = ShallowFlatten.flattenOnce policy p
      
      val _ = case p_opt of
         NONE => assert (false, "Should have flattened something")
       | SOME p' => let
            val Program.T {functions = funcs', ...} = p'
            val f_test' = List.first funcs'
            val {args, blocks, returns, ...} = Function.dest f_test'
            
            val expectedRetTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
            
            (* Assertion 1: Returns type of f_test should be updated/flattened *)
            val _ = case returns of
                       SOME retTys => 
                          if Vector.length retTys = 1 andalso Type.equals (Vector.sub (retTys, 0), expectedRetTy) then ()
                          else assert (false, "returns type mismatch")
                     | NONE => assert (false, "expected SOME returns")
            
            (* Assertion 2: Statements check *)
            val b = Vector.sub (blocks, 0)
            val stmts = Block.statements b
            val _ = assert (Vector.length stmts = 4, "expected 4 statements")
            
            val s0' = Vector.sub (stmts, 0)
            val s1' = Vector.sub (stmts, 1)
            val s2' = Vector.sub (stmts, 2)
            val s3' = Vector.sub (stmts, 3)
            
            (* Check s0': val n = 1 *)
             val Statement.T {ty = ty0, ...} = s0'
             val _ = assert (Type.equals (ty0, seqIndexTy), "s0' type mismatch")
             
             (* Check s1' and s2': val flatBind_0 = Array_alloc[int](n) *)
             fun checkAlloc (Statement.T {exp, ty, ...}) =
                case exp of
                   Exp.PrimApp {prim = Prim.Array_alloc _, targs, ...} =>
                      assert (Type.equals (ty, Type.array intTy) andalso
                              Vector.length targs = 1 andalso
                              Type.equals (Vector.sub (targs, 0), intTy),
                              "alloc statement check")
                 | _ => assert (false, "expected Array_alloc")
             val _ = checkAlloc s1'
             val _ = checkAlloc s2'
             
             (* Check s3': val x = (flatBind_0, flatBind_1) *)
             val _ = case s3' of
                        Statement.T {exp = Exp.Tuple args, ty, ...} =>
                           assert (Type.equals (ty, expectedRetTy) andalso
                                   Vector.length args = 2,
                                   "tuple statement check")
                      | _ => assert (false, "expected Tuple")

             val _ = typeCheck p'
          in () end
   in () end)

   (* Test 12: Array_update flattening
    *
    * Expected input IR:
    *   val n = 1
    *   val x = prim Array_alloc [intInf * intInf] (n)
    *   val idx = 0
    *   val val = (idx, idx)
    *   val _ = prim Array_update [intInf * intInf] (x, idx, val)
    *
    * Expected output IR (when flattened):
    *   val n = 1
    *   val flatBind_2 = prim Array_alloc [intInf] (n)
    *   val flatBind_3 = prim Array_alloc [intInf] (n)
    *   val x = (flatBind_2, flatBind_3)
    *   val idx = 0
    *   val val = (idx, idx)
    *   val flatArr_0 = #0 (x)
    *   val flatArr_1 = #1 (x)
    *   val flatVal_0 = #0 (val)
    *   val flatVal_1 = #1 (val)
    *   val _ = prim Array_update [intInf] (flatArr_0, idx, flatVal_0)
    *   val _ = prim Array_update [intInf] (flatArr_1, idx, flatVal_1)
    *)
   val _ = runTest ("Test 12: Array_update flattening", fn () => let
      val _ = Control.libTargetDir := "../../build/lib/mlton/targets/self"
      val f_test = Func.fromString "f_test"
      val L_start = Label.fromString "L_start"
      
      val intTy = Type.intInf
      val seqIndexTy = Type.word (Atoms.WordSize.seqIndex ())
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      val v_n = Var.fromString "n"
      val v_x = Var.fromString "x"
      val v_idx = Var.fromString "idx"
      val v_val_elem = Var.fromString "val_elem"
      val v_val = Var.fromString "val"
      
      val s0 = Statement.T {
         exp = Exp.Const (Const.word (Atoms.WordX.fromInt (1, Atoms.WordSize.seqIndex ()))),
         ty = seqIndexTy,
         var = SOME v_n
      }
      val s1 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v_n],
            prim = Prim.Array_alloc {raw = false},
            targs = Vector.fromList [tuple2Ty]
         },
         ty = arrayTuple2Ty,
         var = SOME v_x
      }
      val s2 = Statement.T {
         exp = Exp.Const (Const.word (Atoms.WordX.fromInt (0, Atoms.WordSize.seqIndex ()))),
         ty = seqIndexTy,
         var = SOME v_idx
      }
      val s_val_elem = Statement.T {
         exp = Exp.Const (Const.IntInf 0),
         ty = intTy,
         var = SOME v_val_elem
      }
      val s3 = Statement.T {
         exp = Exp.Tuple (Vector.fromList [v_val_elem, v_val_elem]),
         ty = tuple2Ty,
         var = SOME v_val
      }
      val s4 = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v_x, v_idx, v_val],
            prim = Prim.Array_update,
            targs = Vector.fromList [tuple2Ty]
         },
         ty = Type.unit,
         var = NONE
      }
      
      val block = Block.T {
         args = Vector.new0 (),
         label = L_start,
         statements = Vector.fromList [s0, s1, s2, s_val_elem, s3, s4],
         transfer = Transfer.Return (Vector.fromList [v_x])
      }
      
      val func = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [block],
         mayInline = true,
         name = f_test,
         raises = NONE,
         returns = SOME (Vector.fromList [arrayTuple2Ty]),
         start = L_start
      }
      
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [func],
         globals = Vector.new0 (),
         main = f_test
      }
      
      val policy = ShallowFlatten.MaxWidth 2
      val p_opt = ShallowFlatten.flattenOnce policy p
      
      val _ = case p_opt of
         NONE => assert (false, "Should have flattened something")
       | SOME p' => let
            val Program.T {functions = funcs', ...} = p'
            val f_test' = List.first funcs'
            val {args, blocks, returns, ...} = Function.dest f_test'
            
            (* Assertion 1: Returns type of f_test should be updated/flattened *)
            val expectedRetTy = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
            val _ = case returns of
                       SOME retTys => 
                          if Vector.length retTys = 1 andalso Type.equals (Vector.sub (retTys, 0), expectedRetTy) then ()
                          else assert (false, "returns type mismatch")
                     | NONE => assert (false, "expected SOME returns")
            
            (* Assertion 2: Statements check *)
            val b = Vector.sub (blocks, 0)
            val stmts = Block.statements b
            val _ = assert (Vector.length stmts = 13, "expected 13 statements")
            
            (* The last 6 statements check *)
            val s7 = Vector.sub (stmts, 7)
            val s8 = Vector.sub (stmts, 8)
            val s9 = Vector.sub (stmts, 9)
            val s10 = Vector.sub (stmts, 10)
            val s11 = Vector.sub (stmts, 11)
            val s12 = Vector.sub (stmts, 12)

            (* Check that s7 and s8 are Selects from x *)
            fun checkSelectFromVar (Statement.T {exp, ...}, expectedFrom) =
               case exp of
                  Exp.Select {tuple, ...} =>
                     assert (Var.equals (tuple, expectedFrom), "expected Select from variable")
                 | _ => assert (false, "expected Select")
            val _ = checkSelectFromVar (s7, v_x)
            val _ = checkSelectFromVar (s8, v_x)
            
            (* Check that s9 and s10 are Selects from val *)
            val _ = checkSelectFromVar (s9, v_val)
            val _ = checkSelectFromVar (s10, v_val)

            (* Check that s11 and s12 are Array_update PrimApps *)
            fun checkStore (Statement.T {exp, ty, ...}) =
               case exp of
                  Exp.PrimApp {args, prim = Prim.Array_update, targs} =>
                     assert (Type.equals (ty, Type.unit) andalso
                             Vector.length args = 3 andalso
                             Vector.length targs = 1 andalso
                             Type.equals (Vector.sub (targs, 0), intTy),
                             "store statement check")
                 | _ => assert (false, "expected Array_update")
            val _ = checkStore s11
            val _ = checkStore s12

            val _ = typeCheck p'
         in () end
   in () end)

   (* Test 13: Tail call return type propagation bug
    *
    * Expected input IR:
    *   fun f_g (arg_g: intInf): {returns = SOME (intInf)} =
    *     L_start_g () =>
    *       val alloc_n = 1
    *       val alloc_x = prim Array_alloc [intInf * intInf] (alloc_n)
    *       return (arg_g)
    *
    *   fun f_test (arg_f: intInf): {returns = SOME (intInf)} =
    *     L_start_f () =>
    *       call tail f_g (arg_f)
    *
    *   fun f_main (arg_main: intInf): {returns = SOME (intInf)} =
    *     L_start_main () =>
    *       call L_cont_main (f_test (arg_main))
    *     L_cont_main (ret_main) =>
    *       return (ret_main)
    *
    * Expected output IR (when flattened/propagated correctly):
    *   fun f_g (arg_g: intInf): {returns = SOME (intInf)} =
    *     L_start_g () =>
    *       val alloc_n = 1
    *       val flatBind_4 = prim Array_alloc [intInf] (alloc_n)
    *       val flatBind_5 = prim Array_alloc [intInf] (alloc_n)
    *       val alloc_x = (flatBind_4, flatBind_5)
    *       return (arg_g)
    *
    *   fun f_test (arg_f: intInf): {returns = SOME (intInf)} =
    *     L_start_f () =>
    *       call tail f_g (arg_f)
    *
    *   fun f_main (arg_main: intInf): {returns = SOME (intInf)} =
    *     L_start_main () =>
    *       call L_cont_main (f_test (arg_main))
    *     L_cont_main (ret_main) =>
    *       return (ret_main)
    *
    * Bug details:
    *   Due to the bug in propagateReturnTypes, the tail call transfer in f_test
    *   is ignored, causing f_test's return type to propagate to SOME () (empty).
    *   This type mismatch breaks f_main's call site and fails type-checking.
    *)
   val _ = runTest ("Test 13: Tail call return type propagation bug", fn () => let
      val _ = Control.libTargetDir := "../../build/lib/mlton/targets/self"
      val f_test = Func.fromString "f_test"
      val f_g = Func.fromString "f_g"
      val f_main = Func.fromString "f_main"
      val L_start_f = Label.fromString "L_start_f"
      val L_start_g = Label.fromString "L_start_g"
      val L_start_main = Label.fromString "L_start_main"
      val L_cont_main = Label.fromString "L_cont_main"
      
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      val v_arg_g = Var.fromString "arg_g"
      val v_arg_f = Var.fromString "arg_f"
      val v_arg_main = Var.fromString "arg_main"
      val v_ret_main = Var.fromString "ret_main"
      val v_alloc_n = Var.fromString "alloc_n"
      val v_alloc_x = Var.fromString "alloc_x"
      
      val s_alloc_n = Statement.T {
         exp = Exp.Const (Const.word (Atoms.WordX.fromInt (1, Atoms.WordSize.seqIndex ()))),
         ty = Type.word (Atoms.WordSize.seqIndex ()),
         var = SOME v_alloc_n
      }
      val s_alloc_x = Statement.T {
         exp = Exp.PrimApp {
            args = Vector.fromList [v_alloc_n],
            prim = Prim.Array_alloc {raw = false},
            targs = Vector.fromList [tuple2Ty]
         },
         ty = arrayTuple2Ty,
         var = SOME v_alloc_x
      }

      val block_g = Block.T {
         args = Vector.new0 (),
         label = L_start_g,
         statements = Vector.fromList [s_alloc_n, s_alloc_x],
         transfer = Transfer.Return (Vector.fromList [v_arg_g])
      }
      val func_g = Function.new {
         args = Vector.fromList [(v_arg_g, intTy)],
         blocks = Vector.fromList [block_g],
         mayInline = true,
         name = f_g,
         raises = NONE,
         returns = SOME (Vector.fromList [intTy]),
         start = L_start_g
      }

      val block_f = Block.T {
         args = Vector.new0 (),
         label = L_start_f,
         statements = Vector.new0 (),
         transfer = Transfer.Call {
            args = Vector.fromList [v_arg_f],
            func = f_g,
            return = Return.Tail
         }
      }
      val func_f = Function.new {
         args = Vector.fromList [(v_arg_f, intTy)],
         blocks = Vector.fromList [block_f],
         mayInline = true,
         name = f_test,
         raises = NONE,
         returns = SOME (Vector.fromList [intTy]),
         start = L_start_f
      }

      val s_arg_main = Statement.T {
         exp = Exp.Const (Const.IntInf 0),
         ty = intTy,
         var = SOME v_arg_main
      }
      val block_main = Block.T {
         args = Vector.new0 (),
         label = L_start_main,
         statements = Vector.fromList [s_arg_main],
         transfer = Transfer.Call {
            args = Vector.fromList [v_arg_main],
            func = f_test,
            return = Return.NonTail {
               cont = L_cont_main,
               handler = Handler.Caller
            }
         }
      }
      val block_main_cont = Block.T {
         args = Vector.fromList [(v_ret_main, intTy)],
         label = L_cont_main,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.fromList [v_ret_main])
      }
      val func_main = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [block_main, block_main_cont],
         mayInline = true,
         name = f_main,
         raises = NONE,
         returns = SOME (Vector.fromList [intTy]),
         start = L_start_main
      }

      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [func_g, func_f, func_main],
         globals = Vector.new0 (),
         main = f_main
      }

      val policy = ShallowFlatten.MaxWidth 2
      val p_opt = ShallowFlatten.flattenOnce policy p
      
      val _ = case p_opt of
         NONE => assert (false, "Should have flattened something")
       | SOME p' => let
            val Program.T {functions = funcs', ...} = p'
            val f_test' = case List.peek (funcs', fn f => Func.equals (Function.name f, f_test)) of
                             SOME f => f
                           | NONE => raise TestFail "f_test not found"
            val {returns, ...} = Function.dest f_test'
            
            val _ = case returns of
                       SOME retTys => 
                          if Vector.length retTys = 1 andalso Type.equals (Vector.sub (retTys, 0), intTy) then ()
                          else assert (false, "returns type mismatch: expected intInf, got length " ^ Int.toString (Vector.length retTys))
                     | NONE => assert (false, "expected SOME returns")
            
            val _ = typeCheck p'
         in () end
   in () end)

   val _ = summarize ()
end
