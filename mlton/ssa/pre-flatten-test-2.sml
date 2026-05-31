local
   open Ssa
in
   val _ = let
      val _ = print "Test 9: buildBindBlock with empty binds\n"
      val targetLabel = Label.fromString "target3"
      val block = PreFlatten.buildBindBlock (Vector.new0 (), targetLabel)
      val Block.T {statements, ...} = block
      val _ = assert (Vector.length statements = 0, "buildBindBlock should have 0 statements")
      val _ = print "Test 9 passed\n"
   in () end

   (* Test 9a: buildBindBlock with BindCon *)
   val _ = let
      val _ = print "Test 9a: buildBindBlock with BindCon\n"
      val toVar = Var.fromString "c"
      val f1 = Var.fromString "f1"
      val con = Con.fromString "SomeCon"
      val binds = Vector.fromList [PreFlatten.BindCon {to = (toVar, Type.unit), froms = Vector.fromList [f1], con = con}]
      val targetLabel = Label.fromString "target"

      val block = PreFlatten.buildBindBlock (binds, targetLabel)
      val Block.T {statements, ...} = block

      val _ = assert (Vector.length statements = 1, "buildBindBlock should have 1 statement")

      val s0 = Vector.sub (statements, 0)
      val Statement.T {exp, var, ...} = s0
      val _ =
         case var of
            SOME v => assert (Var.equals (v, toVar), "statement var mismatch")
          | NONE => (print "statement should have a var\n"; OS.Process.exit OS.Process.failure)

      val _ =
         case exp of
            Exp.ConApp {args, con = con', ...} =>
               let
                  val _ = assert (Vector.length args = 1, "ConApp exp should have 1 arg")
                  val _ = assert (Var.equals (Vector.sub (args, 0), f1), "ConApp arg 1 mismatch")
                  val _ = assert (Con.equals (con, con'), "ConApp con mismatch")
               in () end
          | _ => (print "exp should be a ConApp\n"; OS.Process.exit OS.Process.failure)

      val _ = print "Test 9a passed\n"
   in () end

   (* Test 10: buildFlattenedFunction with Preserve and FlattenTuple *)
   val _ = let
      val _ = print "Test 10: buildFlattenedFunction with Preserve and FlattenTuple\n"
      val fName = Func.fromString "f10"
      val l1 = Label.fromString "L10"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])
      val b1 = Block.T {
         args = Vector.new0 (),
         label = l1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.fromList [v1])
      }
      val f = Function.new {
         args = Vector.fromList [(v1, t1), (v2, tTuple)],
         blocks = Vector.fromList [b1],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.fromList [t1]),
         start = l1
      }

      val res = PreFlatten.buildFlattenedFunction
                    (f, Vector.fromList [PreFlatten.Preserve,
                                         PreFlatten.FlattenTuple])
      val {args, blocks, start,
           name = resName, ...} = Function.dest res
      val _ = assert (not (Label.equals(fName,  resName)),
                      "new function must have a fresh name")
      val _ = assert (Vector.length args = 3,
                      "Flattened function should have 3 args")
      val (_, rt0) = Vector.sub (args, 0)
      val (_, rt1) = Vector.sub (args, 1)
      val (_, rt2) = Vector.sub (args, 2)
      val _ = assert (Type.equals (rt0, t1), "Arg 0 type mismatch")
      val _ = assert (Type.equals (rt1, t1), "Arg 1 type mismatch")
      val _ = assert (Type.equals (rt2, t2), "Arg 2 type mismatch")
      fun checkPreservedVar (v: Var.t, _) = ()
      val _ = Function.foreachVar (res, checkPreservedVar)

      (* Verify that v1 and v2 are actually present in the new function *)
      val allVars = ref []
      val _ = Function.foreachVar (res, fn (v, _) => allVars := v :: !allVars)
      val _ = assert (List.exists (!allVars, fn v => Var.equals (v, v1)), "v1 not preserved")
      val _ = assert (List.exists (!allVars, fn v => Var.equals (v, v2)), "v2 not preserved")

      val startBlock = Vector.peek (blocks, fn b => Label.equals (Block.label b, start))
      val _ = case startBlock of
         SOME (Block.T {statements, ...}) =>
            let
               val found = Vector.exists (statements, fn Statement.T {var, ...} =>
                  case var of
                     SOME v => Var.originalName v = "v2"
                   | NONE => false)
               val _ = assert (found, "Original var v2 not found in statements")
            in () end
       | NONE => (print "Start block not found\n"; OS.Process.exit OS.Process.failure)
      val _ = print "Test 10 passed\n"
   in () end

   (* Test 11: varChoiceManager lifecycle and default choice *)
   val _ = let
      val _ = print "Test 11: varChoiceManager lifecycle and default choice\n"
      val vcm = PreFlatten.newVarChoiceManager ()
      val v = Var.fromString "v11"
      val choice = PreFlatten.getVarChoice (vcm, v)
      val _ =
         case choice of
            PreFlatten.PreserveVar => ()
          | _ => (print "Default choice should be PreserveVar\n"; OS.Process.exit OS.Process.failure)
      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 11 passed\n"
   in () end

   (* Test 12: chooseVarsInStatement *)
   val _ = let
      val _ = print "Test 12: chooseVarsInStatement\n"
      val vcm = PreFlatten.newVarChoiceManager ()

      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val vTuple = Var.fromString "vt"

      val sTuple = Statement.T {
         exp = Exp.Tuple (Vector.fromList [v1, v2]),
         ty = Type.unit, (* Type doesn't strictly matter for chooseVarsInStatement logic as described *)
         var = SOME vTuple
      }

      val sNonTuple = Statement.T {
         exp = Exp.unit,
         ty = Type.unit,
         var = SOME v1
      }

      val _ = PreFlatten.chooseVarsInStatement (vcm, sTuple)
      val _ = PreFlatten.chooseVarsInStatement (vcm, sNonTuple)

      val choiceTuple = PreFlatten.getVarChoice (vcm, vTuple)
      val _ =
         case choiceTuple of
            PreFlatten.FlattenTupleVar vs =>
               let
                  val _ = assert (Vector.length vs = 2, "FlattenTupleVar should have 2 vars")
                  val _ = assert (Var.equals (Vector.sub (vs, 0), v1), "FlattenTupleVar var 0 mismatch")
                  val _ = assert (Var.equals (Vector.sub (vs, 1), v2), "FlattenTupleVar var 1 mismatch")
               in () end
          | _ => (print "vt should be FlattenTupleVar\n"; OS.Process.exit OS.Process.failure)

      val choiceNonTuple = PreFlatten.getVarChoice (vcm, v1)
      val _ =
         case choiceNonTuple of
            PreFlatten.PreserveVar => ()
          | _ => (print "v1 should be PreserveVar\n"; OS.Process.exit OS.Process.failure)

      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 12 passed\n"
   in () end

   (* Test 13: newVarChoicesForProgram *)
   val _ = let
      val _ = print "Test 13: newVarChoicesForProgram\n"

      val g1 = Var.fromString "g1"
      val g2 = Var.fromString "g2"
      val gt = Var.fromString "gt"
      (* gt = (g1, g2) *)
      val gs1 = Statement.T {exp = Exp.Tuple (Vector.fromList [g1, g2]), ty = Type.unit, var = SOME gt}
      val gs2 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME g1}

      val mainFunc = Func.fromString "main13"
      val mainLabel = Label.fromString "L13"
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val vt = Var.fromString "vt"
      (* vt = (v1, v2) *)
      val s1 = Statement.T {exp = Exp.Tuple (Vector.fromList [v1, v2]), ty = Type.unit, var = SOME vt}
      val s2 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME v1}

      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
         statements = Vector.fromList [s1, s2],
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
         globals = Vector.fromList [gs1, gs2],
         main = mainFunc
      }

      val vcm = PreFlatten.newVarChoicesForProgram p

      val choiceGt = PreFlatten.getVarChoice (vcm, gt)
      val _ =
         case choiceGt of
            PreFlatten.FlattenTupleVar vs =>
               let
                  val _ = assert (Vector.length vs = 2, "gt should have 2 components")
                  val _ = assert (Var.equals (Vector.sub (vs, 0), g1), "gt component 0 mismatch")
                  val _ = assert (Var.equals (Vector.sub (vs, 1), g2), "gt component 1 mismatch")
               in () end
          | _ => (print "gt should be FlattenTupleVar\n"; OS.Process.exit OS.Process.failure)

      val choiceVt = PreFlatten.getVarChoice (vcm, vt)
      val _ =
         case choiceVt of
            PreFlatten.FlattenTupleVar vs =>
               let
                  val _ = assert (Vector.length vs = 2, "vt should have 2 components")
                  val _ = assert (Var.equals (Vector.sub (vs, 0), v1), "vt component 0 mismatch")
                  val _ = assert (Var.equals (Vector.sub (vs, 1), v2), "vt component 1 mismatch")
               in () end
          | _ => (print "vt should be FlattenTupleVar\n"; OS.Process.exit OS.Process.failure)

      val choiceG1 = PreFlatten.getVarChoice (vcm, g1)
      val _ =
         case choiceG1 of
            PreFlatten.PreserveVar => ()
          | _ => (print "g1 should be PreserveVar\n"; OS.Process.exit OS.Process.failure)

      val choiceV1 = PreFlatten.getVarChoice (vcm, v1)
      val _ =
         case choiceV1 of
            PreFlatten.PreserveVar => ()
          | _ => (print "v1 should be PreserveVar\n"; OS.Process.exit OS.Process.failure)

      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 13 passed\n"
   in () end

   (* Test 14: checkFlatteningChoice *)
   val _ = let
      val _ = print "Test 14: checkFlatteningChoice\n"
      val fName = Func.fromString "f14"
      val l1 = Label.fromString "L14"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])

      val f = Function.new {
         args = Vector.fromList [(v1, t1), (v2, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = l1,
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = l1
      }

      val _ = print "Test 14a: NoOp choice\n"
      val resNoOp = PreFlatten.checkFlatteningChoice (f, Vector.fromList [PreFlatten.Preserve, PreFlatten.Preserve])
      val _ = case resNoOp of
         PreFlatten.NoOp => ()
       | _ => (print "Expected NoOp\n"; OS.Process.exit OS.Process.failure)

      val _ = print "Test 14b: Valid choice\n"
      val resValid = PreFlatten.checkFlatteningChoice (f, Vector.fromList [PreFlatten.Preserve, PreFlatten.FlattenTuple])
      val _ = case resValid of
         PreFlatten.Valid => ()
       | _ => (print "Expected Valid\n"; OS.Process.exit OS.Process.failure)

      val _ = print "Test 14c: Invalid choice (arity mismatch)\n"
      val resInvalidArity = PreFlatten.checkFlatteningChoice (f, Vector.fromList [PreFlatten.Preserve])
      val _ = case resInvalidArity of
         PreFlatten.Invalid => ()
       | _ => (print "Expected Invalid due to arity\n"; OS.Process.exit OS.Process.failure)

      val _ = print "Test 14d: Invalid choice (flattening a non-tuple)\n"
      val resInvalidNonTuple = PreFlatten.checkFlatteningChoice (f, Vector.fromList [PreFlatten.FlattenTuple, PreFlatten.Preserve])
      val _ = case resInvalidNonTuple of
         PreFlatten.Invalid => ()
       | _ => (print "Expected Invalid due to non-tuple flattening\n"; OS.Process.exit OS.Process.failure)

      val _ = print "Test 14 passed\n"
   in () end

   (* Test 15: functionManager *)
   val _ = let
      val _ = print "Test 15: functionManager\n"
      val fName = Func.fromString "f15"
      val l1 = Label.fromString "L15"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])

      val f = Function.new {
         args = Vector.fromList [(v1, t1), (v2, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = l1,
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = l1
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [f],
         globals = Vector.new0 (),
         main = fName
      }

      val fm = PreFlatten.newFunctionManager p

      val _ = print "Test 15a: getOrCreateFunc NoOp\n"
      val fNoOp = PreFlatten.getOrCreateFunc (fm, fName, Vector.fromList [PreFlatten.Preserve, PreFlatten.Preserve])
      val _ = assert (Func.equals (fNoOp, fName), "NoOp should return original Func.t")

      val _ = print "Test 15b: extractNewFunctions after NoOp\n"
      val newFuncs0 = PreFlatten.extractNewFunctions fm
      val _ = assert (List.length newFuncs0 = 0, "No new functions should be extracted after NoOp")

      val _ = print "Test 15c: getOrCreateFunc Valid\n"
      val fFlattened = PreFlatten.getOrCreateFunc (fm, fName, Vector.fromList [PreFlatten.Preserve, PreFlatten.FlattenTuple])
      val _ = assert (not (Func.equals (fFlattened, fName)), "Valid flattening should return new Func.t")

      val _ = print "Test 15d: extractNewFunctions after Valid\n"
      val newFuncs1 = PreFlatten.extractNewFunctions fm
      val _ = assert (List.length newFuncs1 = 1, "One new function should be extracted")
      val f1 = case newFuncs1 of (x::_) => x | _ => (print "Expected non-empty list\n"; OS.Process.exit OS.Process.failure)
      val _ = assert (Func.equals (Function.name f1, fFlattened), "Extracted function name mismatch")

      val _ = print "Test 15e: extractNewFunctions should clear pending\n"
      val newFuncs2 = PreFlatten.extractNewFunctions fm
      val _ = assert (List.length newFuncs2 = 0, "extractNewFunctions should clear pending list")

      val _ = PreFlatten.destroyFunctionManager fm
      val _ = print "Test 15 passed\n"
   in () end

   val _ = let
      (* TODO(gemini): Fix names *)
      val _ = print "Test 15_2: functionManager, disconnected\n"
      val fName = Func.fromString "f15_2"
      val fName' = Func.fromString "f15_2'"
      val l1 = Label.fromString "L15_2"
      val l1' = Label.fromString "L15_2'"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])

      val f = Function.new {
         args = Vector.fromList [(v1, t1)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = l1,
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = l1
      }
      val f2 = Function.new {
         args = Vector.fromList [(v1, t1)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = l1',
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName',
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = l1'
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [f, f2],
         globals = Vector.new0 (),
         main = fName
      }

      val fm = PreFlatten.newFunctionManager p

      (* Test that we can access a function that's not reachable via DFS *)
      val _ = print "Test 15_2: access disconnected function\n"
      val fNoOp = PreFlatten.getOrCreateFunc (fm, fName',
                                              Vector.fromList [PreFlatten.Preserve])
      val _ = assert (Func.equals (fNoOp, fName'), "NoOp should return original Func.t")

      val _ = PreFlatten.destroyFunctionManager fm
      val _ = print "Test 15_2 passed\n"
   in () end

   (* Test 16: flattening through a single argument for a single-argument function (Always) *)
   val _ = let
      val _ = print "Test 16: single argument function flattening: always\n"
      val fName = Func.fromString "f16"
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = fLf,
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fLf
      }

      val mainName = Func.fromString "main16"
      val t1 = Var.fromString "t1"
      val t2 = Var.fromString "t2"
      val x = Var.fromString "x"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t2}
      val s3 = Statement.T {exp = Exp.Tuple (Vector.fromList [t1, t2]), ty = tTuple, var = SOME x}
      
      val mainL = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainL,
         statements = Vector.fromList [s1, s2, s3],
         transfer = Transfer.Call {
            args = Vector.fromList [x],
            func = fName,
            return = Return.Tail
         }
      }
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock],
         mayInline = true,
         name = mainName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = mainL
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }

      val p' = (case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                   SOME p' => p'
                 | NONE => printFail "Test 16 (always): flattenOnce returned NONE")
      val Program.T {functions, ...} = p'

      val _ = assert (List.length functions = 3, "Expected 3 functions in flattened program")
      
      val _ = let
         val newFunc = List.peek (functions, fn f =>
            let val name = Function.name f in
               not (Func.equals (name, fName)) andalso not (Func.equals (name, mainName))
            end)
      in
         case newFunc of
            SOME f => let
               val {args, ...} = Function.dest f
               val _ = assert (Vector.length args = 2, "Expected 2 args in flattened function")
               val (_, t0) = Vector.sub (args, 0)
               val (_, t1) = Vector.sub (args, 1)
               val _ = assert (Type.equals (t0, tBool), "Arg 0 should be bool")
               val _ = assert (Type.equals (t1, tBool), "Arg 1 should be bool")
            in () end
          | NONE => printFail "Test 16: Flattened function not found"
      end

      val _ = print "Test 16 (always) passed\n"
   in () end

   (* Test 16b: single argument function flattening: local unpack *)
end
