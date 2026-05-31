local
   open Ssa
in
   val _ = let
      val _ = print "Test 1: Simple program\n"
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

      val _ = PreFlatten.transform p1
      val _ = print "Test 1 passed\n"
   in () end

   (* Test 2: doWalk on simple program *)
   val _ = let
      val _ = print "Test 2: doWalk on simple program\n"
      val mainFunc = Func.fromString "main2"
      val mainLabel = Label.fromString "L1"
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
      val p2 = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val _ = clearLog ()
      val _ = PreFlatten.doWalk (walker, p2)
      val expected = [
         "beforeFunc main2",
         "beforeBlock L1",
         "afterBlock L1",
         "afterFunc main2"
      ]
      val _ = assertEqualStrings (expected, getLog (), "doWalk sequence mismatch in Test 2")
      val _ = print "Test 2 passed\n"
   in () end

   (* Test 3: doWalk with statements *)
   val _ = let
      val _ = print "Test 3: doWalk with statements\n"
      val mainFunc = Func.fromString "main3"
      val mainLabel = Label.fromString "L2"
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val s1 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME v1}
      val s2 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME v2}
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
      val p3 = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val _ = clearLog ()
      val _ = PreFlatten.doWalk (walker, p3)
      val expected = [
         "beforeFunc main3",
         "beforeBlock L2",
         "statement v1",
         "statement v2",
         "afterBlock L2",
         "afterFunc main3"
      ]
      val _ = assertEqualStrings (expected, getLog (), "doWalk sequence mismatch in Test 3")
      val _ = print "Test 3 passed\n"
   in () end


   (* Test 4: doWalk with globals *)
   val _ = let
      val _ = print "Test 4: doWalk with globals\n"
      val g1 = Var.fromString "g1"
      val g2 = Var.fromString "g2"
      val gs1 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME g1}
      val gs2 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME g2}

      val mainFunc = Func.fromString "main4"
      val mainLabel = Label.fromString "L3"
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
      val p4 = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.fromList [gs1, gs2],
         main = mainFunc
      }

      val _ = clearLog ()
      val _ = PreFlatten.doWalk (walker, p4)
      val expected = [
         "statement g1",
         "statement g2",
         "beforeFunc main4",
         "beforeBlock L3",
         "afterBlock L3",
         "afterFunc main4"
      ]
      val _ = assertEqualStrings (expected, getLog (), "doWalk sequence mismatch in Test 4")
      val _ = print "Test 4 passed\n"
   in () end

   (* Test 5: Complex walk *)
   val _ = let
      val _ = print "Test 5: Complex walk\n"
      val f1Name = Func.fromString "f1"
      val f1L1 = Label.fromString "f1L1"
      val f1L2 = Label.fromString "f1L2"
      val f2Name = Func.fromString "f2"
      val v1 = Var.fromString "v1"
      val s1 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME v1}
      val b1 = Block.T {
         args = Vector.new0 (),
         label = f1L1,
         statements = Vector.fromList [s1],
         transfer = Transfer.Goto {args = Vector.new0 (), dst = f1L2}
      }
      val b2 = Block.T {
         args = Vector.new0 (),
         label = f1L2,
         statements = Vector.new0 (),
         transfer = Transfer.Call {
            args = Vector.new0 (),
            func = f2Name,
            return = Return.Tail
         }
      }
      val f1 = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [b1, b2],
         mayInline = true,
         name = f1Name,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = f1L1
      }

      val f2L1 = Label.fromString "f2L1"
      val v2 = Var.fromString "v2"
      val s2 = Statement.T {exp = Exp.unit, ty = Type.unit, var = SOME v2}
      val b3 = Block.T {
         args = Vector.new0 (),
         label = f2L1,
         statements = Vector.fromList [s2],
         transfer = Transfer.Return (Vector.new0 ())
      }
      val f2 = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [b3],
         mayInline = true,
         name = f2Name,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = f2L1
      }

      val p5 = Program.T {
         datatypes = Vector.new0 (),
         functions = [f1, f2],
         globals = Vector.new0 (),
         main = f1Name
      }

      val _ = clearLog ()
      val _ = PreFlatten.doWalk (walker, p5)
      val expected = [
         "beforeFunc f1",
         "beforeBlock f1L1",
         "statement v1",
         "afterBlock f1L1",
         "beforeBlock f1L2",
         "afterBlock f1L2",
         "afterFunc f1",
         "beforeFunc f2",
         "beforeBlock f2L1",
         "statement v2",
         "afterBlock f2L1",
         "afterFunc f2"
      ]
      val _ = assertEqualStrings (expected, getLog (), "doWalk sequence mismatch in Test 5")
      val _ = print "Test 5 passed\n"
   in () end

   (* TODO(gemini): renumber tests *)
   (* Test 14: Simple program (existing) *)
   val _ = let
      val _ = print "Test 14: mapBlocks\n"
      val mainFunc = Func.fromString "main"
      val mainLabel = Label.fromString "L0"
      fun mkBlock label = Block.T {
         args = Vector.new0 (),
         label = label,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      val mainBlock = mkBlock mainLabel
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

      fun noOpBlockF b = NONE

      val newLabel = Label.fromString "newLabel"
      fun changeNameBlockF b = SOME (mkBlock newLabel)

      fun getUniqueBlockLabel (Program.T {functions, ...}) = let
         val _ = assert (List.length functions = 1, "expect unique function")
         val func = List.first functions
         val blocks = Function.blocks func
         val _ = assert (Vector.length blocks = 1, "expect unique block")
         val block = Vector.first blocks
      in
         Block.label block
      end


      val p1' = PreFlatten.mapBlocks (p1, noOpBlockF)
      val _ = assert (Label.equals (getUniqueBlockLabel p1', mainLabel),
                      "expect original label")

      val p1'' = PreFlatten.mapBlocks (p1, changeNameBlockF)

      val _ = assert (Label.equals (getUniqueBlockLabel p1'', newLabel),
                      "expect new label")

      val _ = print "Test 14 passed\n"
   in () end

   (* Test 2: doWalk on simple program *)
   val _ = let
      val _ = print "Test 2: doWalk on simple program\n"
      val mainFunc = Func.fromString "main2"
      val mainLabel = Label.fromString "L1"
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
      val p2 = Program.T {
         datatypes = Vector.new0 (),
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val _ = clearLog ()
      val _ = PreFlatten.doWalk (walker, p2)
      val expected = [
         "beforeFunc main2",
         "beforeBlock L1",
         "afterBlock L1",
         "afterFunc main2"
      ]
      val _ = assertEqualStrings (expected, getLog (), "doWalk sequence mismatch in Test 2")
      val _ = print "Test 2 passed\n"
   in () end

   (* Test 6: flattenTupleVar *)
   val _ = let
      val _ = print "Test 6: flattenTupleVar\n"

      val t1 = Type.bool
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])

      val v = Var.fromString "v"

      val _ = print "Test 6a: flattening a 2-tuple\n"
      val res = PreFlatten.flattenTupleVar (v, tTuple)
      val _ =
         case res of
            NONE => (print "flattenTupleVar returned NONE for tuple type\n"; OS.Process.exit OS.Process.failure)
          | SOME vts =>
               let
                  val _ = assert (Vector.length vts = 2, "flattenTupleVar result length mismatch")
                  val (v1, t1') = Vector.sub (vts, 0)
                  val (v2, t2') = Vector.sub (vts, 1)
                  val _ = assert (Type.equals (t1, t1'), "flattenTupleVar type 1 mismatch")
                  val _ = assert (Type.equals (t2, t2'), "flattenTupleVar type 2 mismatch")
                  val _ = assert (not (Var.equals (v1, v)), "flattenTupleVar var 1 should be fresh")
                  val _ = assert (not (Var.equals (v2, v)), "flattenTupleVar var 2 should be fresh")
                  val _ = assert (not (Var.equals (v1, v2)), "flattenTupleVar vars should be distinct")
               in () end

      val _ = print "Test 6b: flattening a non-tuple\n"
      val resNonTuple = PreFlatten.flattenTupleVar (v, t1)
      val _ =
          case resNonTuple of
              NONE => ()
            | SOME _ => (print "flattenTupleVar should return NONE for non-tuple type\n"; OS.Process.exit OS.Process.failure)


      (* This surprising behavior is because `Type.tuple` flattens through
      single-element arguments *)
      val _ = print "Test 6b: flattening a singleton tuple (regular ctor)\n"
      val tSingletonTuple = Type.tuple (Vector.new1 t1)
      val resSingletonTuple = PreFlatten.flattenTupleVar (v, tSingletonTuple)
      val _ = case resSingletonTuple of
                  NONE => ()
                | SOME inner => printFail "unexpected flatten"

     
      val _ = print "Test 6 passed\n"
   in () end

   (* Test 7: buildBindBlock *)
   val _ = let
      val _ = print "Test 7: buildBindBlock\n"
      val toVar = Var.fromString "t"
      val f1 = Var.fromString "f1"
      val f2 = Var.fromString "f2"
      val binds = Vector.fromList [PreFlatten.BindTuple {to = (toVar, Type.unit), froms = Vector.fromList [f1, f2]}]
      val targetLabel = Label.fromString "target"

      val block = PreFlatten.buildBindBlock (binds, targetLabel)
      val Block.T {args, label = _, statements, transfer} = block

      val _ = assert (Vector.length args = 0, "buildBindBlock block should have no args")
      val _ = assert (Vector.length statements = 1, "buildBindBlock should have 1 statement")

      val s0 = Vector.sub (statements, 0)
      val Statement.T {exp, var, ...} = s0
      val _ =
         case var of
            SOME v => assert (Var.equals (v, toVar), "statement var mismatch")
          | NONE => (print "statement should have a var\n"; OS.Process.exit OS.Process.failure)

      val _ =
         case exp of
            Exp.Tuple vs =>
               let
                  val _ = assert (Vector.length vs = 2, "tuple exp should have 2 args")
                  val _ = assert (Var.equals (Vector.sub (vs, 0), f1), "tuple arg 1 mismatch")
                  val _ = assert (Var.equals (Vector.sub (vs, 1), f2), "tuple arg 2 mismatch")
               in () end
          | _ => (print "exp should be a tuple\n"; OS.Process.exit OS.Process.failure)

      val _ =
         case transfer of
            Transfer.Goto {args, dst} =>
               let
                  val _ = assert (Vector.length args = 0, "goto should have no args")
                  val _ = assert (Label.equals (dst, targetLabel), "goto dst mismatch")
               in () end
          | _ => (print "transfer should be a goto\n"; OS.Process.exit OS.Process.failure)

      val _ = print "Test 7 passed\n"
   in () end

   (* Test 8: buildBindBlock with multiple binds *)
   val _ = let
      val _ = print "Test 8: buildBindBlock with multiple binds\n"
      val t1 = Var.fromString "t1"
      val f1 = Var.fromString "f1"
      val t2 = Var.fromString "t2"
      val f2 = Var.fromString "f2"
      val f3 = Var.fromString "f3"
      val binds = Vector.fromList [
         PreFlatten.BindTuple {to = (t1, Type.unit), froms = Vector.fromList [f1]},
         PreFlatten.BindTuple {to = (t2, Type.unit), froms = Vector.fromList [f2, f3]}
      ]
      val targetLabel = Label.fromString "target2"

      val block = PreFlatten.buildBindBlock (binds, targetLabel)
      val Block.T {statements, ...} = block

      val _ = assert (Vector.length statements = 2, "buildBindBlock should have 2 statements")
      val _ = print "Test 8 passed\n"
   in () end

   (* Test 9: buildBindBlock with empty binds *)
   val _ = let
      val _ = print "Test 9: buildBindBlock with empty binds\n"
      val targetLabel = Label.fromString "target3"
      val block = PreFlatten.buildBindBlock (Vector.new0 (), targetLabel)
      val Block.T {statements, transfer, ...} = block
      val _ = assert (Vector.length statements = 0, "empty binds should have 0 statements")
      val _ =
         case transfer of
            Transfer.Goto {dst, ...} => assert (Label.equals (dst, targetLabel), "goto dst mismatch")
          | _ => printFail "transfer should be a goto"
      val _ = print "Test 9 passed\n"
   in () end

   (* Test 10: foldTransformation empty *)
   val _ = let
      val _ = print "Test 10: foldTransformation empty\n"
      val res = PreFlatten.foldTransformation ([], fn _ => NONE, 0)
      val _ = assert (Option.isNone res, "foldTransformation empty should return NONE")
      val _ = print "Test 10 passed\n"
   in () end

   (* Test 11: foldTransformation all NONE *)
   val _ = let
      val _ = print "Test 11: foldTransformation all NONE\n"
      val steps = [1, 2, 3]
      fun stepF (s, b) = NONE
      val res = PreFlatten.foldTransformation (steps, stepF, 0)
      val _ = assert (Option.isNone res, "foldTransformation all NONE should return NONE")
      val _ = print "Test 11 passed\n"
   in () end

   (* Test 12: foldTransformation with SOME *)
   val _ = let
      val _ = print "Test 12: foldTransformation with SOME\n"
      val steps = [1, 2, 3]
      fun stepF (s, b) = if s = 2 then SOME (b + s) else NONE
      val res = PreFlatten.foldTransformation (steps, stepF, 10)
      val _ =
         case res of
            SOME v => assert (v = 12, "foldTransformation SOME value mismatch")
          | NONE => printFail "foldTransformation SOME should return SOME"
      val _ = print "Test 12 passed\n"
   in () end

   (* Test 13: foldTransformation order and accumulation *)
   val _ = let
      val _ = print "Test 13: foldTransformation order and accumulation\n"
      val steps = ["a", "b", "c"]
      fun stepF (s, b) = SOME (b ^ s)
      val res = PreFlatten.foldTransformation (steps, stepF, "")
      val _ =
         case res of
            SOME v => assert (v = "abc", "foldTransformation order/accumulation mismatch")
          | NONE => printFail "foldTransformation SOME should return SOME"
      val _ = print "Test 13 passed\n"
   in () end
end
