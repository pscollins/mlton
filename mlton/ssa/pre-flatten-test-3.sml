local
   open Ssa
in
   val _ = let
      val _ = print "Test 16: single argument function flattening: local unpack\n"
      val fName = Func.fromString "f16b"
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

      val mainName = Func.fromString "main16b"
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

      val _ = (case PreFlatten.flattenOnce (PreFlatten.FlattenForAnyUnpack, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                   SOME _ => printFail "Test 16 (local unpack): flattenOnce returned SOME, expected NONE"
                 | NONE => ())

      val _ = print "Test 16 (local unpack) passed\n"
   in () end

   (* Test 17: flattening through a single argument for a multi-argument function *)
   val _ = let
      val _ = print "Test 17: single argument flattening in multi-arg function\n"
      val fName = Func.fromString "f17"
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tTuple), (Var.fromString "arg2", tBool)],
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

      val mainName = Func.fromString "main17"
      val t1 = Var.fromString "t1"
      val t2 = Var.fromString "t2"
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t2}
      val s3 = Statement.T {exp = Exp.Tuple (Vector.fromList [t1, t2]), ty = tTuple, var = SOME x}
      val s4 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME y}
      
      val mainL = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainL,
         statements = Vector.fromList [s1, s2, s3, s4],
         transfer = Transfer.Call {
            args = Vector.fromList [x, y],
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
                 | NONE => printFail "Test 17: flattenOnce returned NONE")
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
               val _ = assert (Vector.length args = 3, "Expected 3 args in flattened function")
               val (_, t0) = Vector.sub (args, 0)
               val (_, t1) = Vector.sub (args, 1)
               val (_, t2) = Vector.sub (args, 2)
               val _ = assert (Type.equals (t0, tBool), "Arg 0 should be bool")
               val _ = assert (Type.equals (t1, tBool), "Arg 1 should be bool")
               val _ = assert (Type.equals (t2, tBool), "Arg 2 should be bool")
            in () end
          | NONE => printFail "Test 17: Flattened function not found"
      end

      val _ = print "Test 17 passed\n"
   in () end

   (* Test 18: flattening through multiple arguments for a multi-argument function *)
   val _ = let
      val _ = print "Test 18: multi-argument flattening\n"
      val fName = Func.fromString "f18"
      val tBool = Type.bool
      val tTup1 = Type.tuple (Vector.fromList [tBool, tBool, tBool])
      val tTup2 = Type.tuple (Vector.fromList [tBool, tBool])
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tTup1), (Var.fromString "arg2", tTup2)],
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

      val mainName = Func.fromString "main18"
      val b1 = Var.fromString "b1"
      val b2 = Var.fromString "b2"
      val b3 = Var.fromString "b3"
      val x = Var.fromString "x"
      val y = Var.fromString "y"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b2}
      val s3 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b3}
      val s4 = Statement.T {exp = Exp.Tuple (Vector.fromList [b1, b2, b3]),
                            ty = tTup1, var = SOME x}
      val s5 = Statement.T {exp = Exp.Tuple (Vector.fromList [b2, b3]), ty = tTup2, var = SOME y}
      
      val mainL = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainL,
         statements = Vector.fromList [s1, s2, s3, s4, s5],
         transfer = Transfer.Call {
            args = Vector.fromList [x, y],
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

      val _ = printProgram ("test18", p)

      val p' = (case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                   SOME p' => p'
                 | NONE => printFail "Test 18: flattenOnce returned NONE")
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
               val _ = assert (Vector.length args = 5, "Expected 5 args in flattened function")
               val _ = Vector.foreach (args, fn (_, t) =>
                          assert (Type.equals (t, tBool), "All args should be bool"))
            in () end
          | NONE => printFail "Test 18: Flattened function not found"
      end

      val _ = print "Test 18 passed\n"
   in () end

   (* Test 19: an unflattenable call *)
   val _ = let
      val _ = print "Test 19: unflattenable call\n"
      val fName = Func.fromString "f19"
      val tBool = Type.bool
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tBool)],
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
      val mainName = Func.fromString "main19"
      val x = Var.fromString "x"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME x}
      val mainL = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainL,
         statements = Vector.fromList [s1],
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
      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                 NONE => ()
               | SOME _ => printFail "Test 19: expected NONE, but got SOME"
      val _ = print "Test 19 passed\n"
   in () end

   (* Test 20: a mixture of flattenable and unflattenable calls for the same function *)
   val _ = let
      val _ = print "Test 20: mixture of flattenable and unflattenable calls\n"
      val fName = Func.fromString "f20"
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

      val mainName = Func.fromString "main20"
      val t1 = Var.fromString "t1"
      val t2 = Var.fromString "t2"
      val x1 = Var.fromString "x1"
      val x2 = Var.fromString "x2"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t2}
      val s3 = Statement.T {exp = Exp.Tuple (Vector.fromList [t1, t2]), ty = tTuple, var = SOME x1}
      (* x2 is NOT a tuple from a tuple expression *)
      val s4 = Statement.T {exp = Exp.unit, ty = tTuple, var = SOME x2}
      
      val lMain = Label.fromString "Lmain"
      val lCall2 = Label.fromString "Lcall2"
      
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = lMain,
         statements = Vector.fromList [s1, s2, s3, s4],
         transfer = Transfer.Call {
            args = Vector.fromList [x1],
            func = fName,
            return = Return.NonTail {cont = lCall2, handler = Handler.Caller}
         }
      }
      val call2Block = Block.T {
         args = Vector.new0 (),
         label = lCall2,
         statements = Vector.new0 (),
         transfer = Transfer.Call {
            args = Vector.fromList [x2],
            func = fName,
            return = Return.Tail
         }
      }
      
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock, call2Block],
         mayInline = true,
         name = mainName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = lMain
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }
      
      val p' = (case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                   SOME p' => p'
                 | NONE => printFail "Test 20: flattenOnce returned NONE")
      val Program.T {functions, ...} = p'
      
      (* Should have 3 functions: f, main, and f_flattened (for x1 call) *)
      val _ = assert (List.length functions = 3, "Expected 3 functions in flattened program")

      val _ = let
         val origFunc = List.peek (functions, fn f => Func.equals (Function.name f, fName))
         val newFunc = List.peek (functions, fn f =>
            let val name = Function.name f in
               not (Func.equals (name, fName)) andalso not (Func.equals (name, mainName))
            end)
      in
         case (origFunc, newFunc) of
            (SOME fOrig, SOME fNew) => let
               val {args = argsOrig, ...} = Function.dest fOrig
               val {args = argsNew, ...} = Function.dest fNew
               val _ = assert (Vector.length argsOrig = 1, "Original function should have 1 arg")
               val _ = assert (Type.equals (#2 (Vector.sub (argsOrig, 0)), tTuple),
                               "Original arg should be tuple")
               val _ = assert (Vector.length argsNew = 2, "New function should have 2 args")
               val _ = assert (Type.equals (#2 (Vector.sub (argsNew, 0)), tBool), "New arg 0 should be bool")
               val _ = assert (Type.equals (#2 (Vector.sub (argsNew, 1)), tBool), "New arg 1 should be bool")
            in () end
          | _ => printFail "Test 20: Original or new function not found"
      end

      val _ = print "Test 20 passed\n"
   in () end

   (* Test 21: a mixture of different flattening decisions for the same function *)
   val _ = let
      val _ = print "Test 21: mixture of different flattening decisions\n"
      val fName = Func.fromString "f21"
      val tBool = Type.bool
      val tTup1 = Type.tuple (Vector.fromList [tBool, tBool])
      val tTup2 = Type.tuple (Vector.fromList [tBool, tBool])
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tTup1), (Var.fromString "arg2", tTup2)],
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

      val mainName = Func.fromString "main21"
      val b1 = Var.fromString "b1"
      val b2 = Var.fromString "b2"
      val b3 = Var.fromString "b3"
      val x1 = Var.fromString "x1"
      val y1 = Var.fromString "y1"
      val x2 = Var.fromString "x2"
      val y2 = Var.fromString "y2"

      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b2}
      val s3 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b3}

      val sx1 = Statement.T {exp = Exp.Tuple (Vector.fromList [b1, b2]), ty = tTup1, var = SOME x1}
      val sy1 = Statement.T {exp = Exp.unit, ty = tTup2, var = SOME y1} (* No flatten *)

      val sx2 = Statement.T {exp = Exp.unit, ty = tTup1, var = SOME x2} (* No flatten *)
      val sy2 = Statement.T {exp = Exp.Tuple (Vector.fromList [b2, b3]), ty = tTup2, var = SOME y2}

      val lMain = Label.fromString "Lmain"
      val lCall2 = Label.fromString "Lcall2"

      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = lMain,
         statements = Vector.fromList [s1, s2, s3, sx1, sy1, sx2, sy2],
         transfer = Transfer.Call {
            args = Vector.fromList [x1, y1],
            func = fName,
            return = Return.NonTail {cont = lCall2, handler = Handler.Caller}
         }
      }
      val call2Block = Block.T {
         args = Vector.new0 (),
         label = lCall2,
         statements = Vector.new0 (),
         transfer = Transfer.Call {
            args = Vector.fromList [x2, y2],
            func = fName,
            return = Return.Tail
         }
      }

      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock, call2Block],
         mayInline = true,
         name = mainName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = lMain
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }

      val p' = (case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                   SOME p' => p'
                 | NONE => printFail "Test 21: flattenOnce returned NONE")
      val Program.T {functions, ...} = p'

      (* Should have 4 functions: f, main, f_flat1 (for x1), f_flat2 (for y2) *)
      val _ = assert (List.length functions = 4, "Expected 4 functions in flattened program")

      val _ = let
         val origFunc = List.peek (functions, fn f => Func.equals (Function.name f, fName))
         val otherFuncs = List.keepAll (functions, fn f =>
            let val name = Function.name f in
               not (Func.equals (name, fName)) andalso not (Func.equals (name, mainName))
            end)
      in
         case (origFunc, otherFuncs) of
            (SOME fOrig, [fA, fB]) => let
               val {args = argsOrig, ...} = Function.dest fOrig
               val _ = assert (Vector.length argsOrig = 2, "Original should have 2 args")
               
               fun check (f, expected) = let
                  val {args, ...} = Function.dest f
               in
                  Vector.length args = 3
                  andalso Type.equals (#2 (Vector.sub (args, 0)), List.nth (expected, 0))
                  andalso Type.equals (#2 (Vector.sub (args, 1)), List.nth (expected, 1))
                  andalso Type.equals (#2 (Vector.sub (args, 2)), List.nth (expected, 2))
               end
               
               val sig1 = [tBool, tBool, tTup2]
               val sig2 = [tTup1, tBool, tBool]
               
               val match1 = check (fA, sig1) orelse check (fB, sig1)
               val match2 = check (fA, sig2) orelse check (fB, sig2)
               
               val _ = assert (match1, "Signature [bool, bool, tuple] not found")
               val _ = assert (match2, "Signature [tuple, bool, bool] not found")
            in () end
          | _ => printFail "Test 21: Original or new functions not found correctly"
      end

      val _ = print "Test 21 passed\n"
   in () end

   (* Test 22: Iterative flattening *)
   val _ = let
      val _ = print "Test 22: Iterative flattening\n"
      val fName = Func.fromString "f22"
      val tBool = Type.bool
      val tTupInner = Type.tuple (Vector.fromList [tBool, tBool])
      val tTupOuter = Type.tuple (Vector.fromList [tTupInner, tBool])
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tTupOuter)],
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

      val mainName = Func.fromString "main22"
      val b1 = Var.fromString "b1"
      val b2 = Var.fromString "b2"
      val b3 = Var.fromString "b3"
      val vInner = Var.fromString "vInner"
      val vOuter = Var.fromString "vOuter"

      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b2}
      val s3 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b3}

      val sInner = Statement.T {exp = Exp.Tuple (Vector.fromList [b1, b2]), ty = tTupInner, var = SOME vInner}
      val sOuter = Statement.T {exp = Exp.Tuple (Vector.fromList [vInner, b3]), ty = tTupOuter, var = SOME vOuter}

      val lMain = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = lMain,
         statements = Vector.fromList [s1, s2, s3, sInner, sOuter],
         transfer = Transfer.Call {
            args = Vector.fromList [vOuter],
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
         start = lMain
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }

      (* First, run with max-iters = 1 *)
      val _ = Control.preFlattenMaxIters := 1
      val p1 = PreFlatten.transform p
      val Program.T {functions = funcs1, ...} = p1
      
      (* Should have 3 functions: f, main, and f_flat (2 args) *)
      val _ = assert (List.length funcs1 = 3, "Expected 3 functions with max-iters=1")
      
      (* Second, run with max-iters = 2 *)
      val _ = Control.preFlattenMaxIters := 2
      val p2 = PreFlatten.transform p
      val Program.T {functions = funcs2, ...} = p2
      
      (* Should have 4 functions: f, main, f_flat, and f_flat_flat (3 args) *)
      val _ = assert (List.length funcs2 = 4, "Expected 4 functions with max-iters=2")
      
      val finalFunc = List.peek (funcs2, fn f =>
         let val {args, ...} = Function.dest f in
            Vector.length args = 3
         end)
      val _ = assert (Option.isSome finalFunc, "Expected a function with 3 arguments after 2 iterations")

      val _ = print "Test 22 passed\n"
   in () end

   val emptyProgram = let
      val dummyLabel = Func.fromString "dummy"
   in
      Program.T {datatypes=Vector.new0(),
                 functions=[],
                 globals=Vector.new0(),
                 main=dummyLabel}
   end
   (* Test 23: markConsumersInStatement and getVarConsumers *)
   val _ = let
      val _ = print "Test 23: markConsumersInStatement and getVarConsumers\n"
      val vm = PreFlatten.newVarConsumerManager emptyProgram
      val v = Var.fromString "v"
      val v_dest = Var.fromString "v_dest"
      val con = Con.fromString "C"
      
      val _ = print "Test 23a: AsUnpacked\n"
      val sSelect = Statement.T {
         exp = Exp.Select {offset = 0, tuple = v},
         ty = Type.unit,
         var = SOME v_dest
      }
      val _ = PreFlatten.markConsumersInStatement (vm, sSelect)
      val consumers1 = PreFlatten.getVarConsumers (vm, v)
      val _ = assert (List.length consumers1 = 1, "Expected 1 consumer after sSelect")
      val _ = case List.nth (consumers1, 0) of
                 PreFlatten.AsUnpacked => ()
               | _ => printFail "Expected AsUnpacked for Select"

      val _ = print "Test 23b: AsCurrent (ConApp)\n"
      val sCon = Statement.T {
         exp = Exp.ConApp {args = Vector.fromList [v], con = con},
         ty = Type.unit,
         var = SOME v_dest
      }
      val _ = PreFlatten.markConsumersInStatement (vm, sCon)
      val consumers2 = PreFlatten.getVarConsumers (vm, v)
      val _ = assert (List.length consumers2 = 2, "Expected 2 consumers after sCon")
      val _ = assert (List.exists (consumers2, fn PreFlatten.AsCurrent => true | _ => false),
                      "Expected AsCurrent in consumers")

      val _ = print "Test 23c: AsCurrent (PrimApp and Tuple)\n"
      val sPrim = Statement.T {
         exp = Exp.PrimApp {args = Vector.fromList [v], prim = Prim.MLton_equal, targs = Vector.new0 ()},
         ty = Type.unit,
         var = SOME v_dest
      }
      val _ = PreFlatten.markConsumersInStatement (vm, sPrim)
      
      val sTuple = Statement.T {
         exp = Exp.Tuple (Vector.fromList [v]),
         ty = Type.unit,
         var = SOME v_dest
      }
      val _ = PreFlatten.markConsumersInStatement (vm, sTuple)

      (* TODO(pscollins): Circle back and reconsider this test *)
      val _ = print "Test 23d: AsAlias\n"
      val v_alias = Var.fromString "v_alias"
      val sVar = Statement.T {
         exp = Exp.Var v,
         ty = Type.unit,
         var = SOME v_alias
      }
      val _ = PreFlatten.markConsumersInStatement (vm, sVar)
      val consumers3 = PreFlatten.getVarConsumers (vm, v)
      val _ = assert (List.length consumers3 = 5, "Expected 5 consumers in total")
      val _ = assert (List.exists (consumers3, fn PreFlatten.AsAlias v' => Var.equals (v', v_alias) | _ => false),
                      "Expected AsAlias v_alias in consumers")

      val _ = PreFlatten.destroyVarConsumerManager vm
      val _ = print "Test 23 passed\n"
   in () end

   (* Test 24: markConsumersInTransfer *)
   val _ = let
      val _ = print "Test 24: markConsumersInTransfer\n"
      fun mkProgramForFunc f =
         Program.T {datatypes=Vector.new0(),
                    functions=[f],
                    globals=Vector.new0(),
                    main=Function.name f}
      fun mkProgramForBlocks blocks = let
         val first = Vector.first blocks
         val func = Function.new {args = Vector.new0(),
                                  blocks = blocks,
                                  mayInline = true,
                                  name = Func.newString "test",
                                  raises = NONE,
                                  returns = NONE,
                                  start = Block.label first}
      in
         mkProgramForFunc func
      end
      fun getUniqueFunc p = let
         val Program.T {functions, ...} = p
      in
         case functions of
             [f] => f
           | _ => Error.bug "bad program"
      end
      fun getFirstBlock f = let
         val blocks = Function.blocks f
      in
         Vector.first blocks
      end
      val _ = let
         val _ = print "Test 24a: Goto alias\n"
         val v1 = Var.fromString "v1"
         val v2 = Var.fromString "v2"
         val v_formal1 = Var.fromString "vf1"
         val v_formal2 = Var.fromString "vf2"
         val b1Label = Label.newString "gotoer"
         val b2Label = Label.newString "gotoee"
         val b1 = Block.T {args = Vector.new0(),
                           label = b1Label,
                           statements = Vector.new0(),
                           transfer = Transfer.Goto {args = Vector.new2 (v1, v2),
                                                     dst = b2Label}}
         val b2 = Block.T {args = Vector.new2 ((v_formal1, Type.unit),
                                             (v_formal2, Type.unit)),
                           label = b2Label,
                           statements = Vector.new0(),
                           transfer = Transfer.Bug}
         val p1 = mkProgramForBlocks (Vector.new2 (b1, b2))

         val vm1 = PreFlatten.newVarConsumerManager p1

         val tGoto = (Block.transfer b1)
         (* We expect this to record v1 -> AsAlias vf1 and v2 -> AsAlias vf2
         if L has arguments [vf1, vf2]. *)
         val _ = PreFlatten.markConsumersInTransfer (vm1, tGoto)
         val consumers1 = PreFlatten.getVarConsumers (vm1, v1)
         val _ = assert (List.exists (consumers1,
                                      fn PreFlatten.AsAlias v' => Var.equals (v', v_formal1) | _ => false),
                         "Expected AsAlias vf1 for v1 in Goto")
         val consumers2 = PreFlatten.getVarConsumers (vm1, v2)
         val _ = assert (List.exists (consumers2, fn PreFlatten.AsAlias v' => Var.equals (v', v_formal2) | _ => false),
                         "Expected AsAlias vf2 for v2 in Goto")
         val _ = PreFlatten.destroyVarConsumerManager vm1
      in
         ()
      end

      val _ = let
         val _ = print "Test 24b: Call alias\n"
         val v1 = Var.fromString "v1"
         val vf1 = Var.fromString "vf1"
         val fName = Func.newString "callee"
         val callee = Function.new {args = Vector.new1 (vf1, Type.unit),
                                   blocks = Vector.new1 (Block.T {args = Vector.new0(),
                                                                label = Label.newString "L",
                                                                statements = Vector.new0(),
                                                                transfer = Transfer.Return (Vector.new0())}),
                                   mayInline = true,
                                   name = fName,
                                   raises = NONE,
                                   returns = NONE,
                                   start = Label.newString "L"}
         val callerL = Label.newString "caller"
         val callerBlock = Block.T {args = Vector.new0(),
                                    label = callerL,
                                    statements = Vector.new0(),
                                    transfer = Transfer.Call {args = Vector.new1 v1,
                                                              func = fName,
                                                              return = Return.Tail}}
         val caller = Function.new {args = Vector.new0(),
                                   blocks = Vector.new1 callerBlock,
                                   mayInline = true,
                                   name = Func.newString "caller",
                                   raises = NONE,
                                   returns = NONE,
                                   start = callerL}
         val p = Program.T {datatypes = Vector.new0(),
                            functions = [callee, caller],
                            globals = Vector.new0(),
                            main = Func.newString "caller"}
         val vm = PreFlatten.newVarConsumerManager p
         val tCall = Block.transfer callerBlock
         val _ = PreFlatten.markConsumersInTransfer (vm, tCall)
         val consumers = PreFlatten.getVarConsumers (vm, v1)
         val _ = assert (List.exists (consumers, fn PreFlatten.AsAlias v' => Var.equals (v', vf1) | _ => false),
                         "Expected AsAlias vf1 for v1 in Call")
         val _ = PreFlatten.destroyVarConsumerManager vm
      in
         ()
      end

      val _ = let
         val _ = print "Test 24c: Return alias\n"
         val v1 = Var.fromString "v1"
         val r1 = Var.fromString "r1"
         val fName = Func.newString "callee"
         val calleeL = Label.newString "L"
         val returnT = Transfer.Return (Vector.new1 v1)
         val callee = Function.new {args = Vector.new0(),
                                   blocks = Vector.new1 (Block.T {args = Vector.new0(),
                                                                label = calleeL,
                                                                statements = Vector.new0(),
                                                                transfer = returnT}),
                                   mayInline = true,
                                   name = fName,
                                   raises = NONE,
                                   returns = SOME (Vector.new1 Type.unit),
                                   start = calleeL}
         val contL = Label.newString "cont"
         val contBlock = Block.T {args = Vector.new1 (r1, Type.unit),
                                  label = contL,
                                  statements = Vector.new0(),
                                  transfer = Transfer.Return (Vector.new0())}
         val callerL = Label.newString "caller"
         val callerBlock = Block.T {args = Vector.new0(),
                                    label = callerL,
                                    statements = Vector.new0(),
                                    transfer = Transfer.Call {args = Vector.new0(),
                                                              func = fName,
                                                              return = Return.NonTail {cont = contL, handler = Handler.Caller}}}
         val caller = Function.new {args = Vector.new0(),
                                   blocks = Vector.new2 (callerBlock, contBlock),
                                   mayInline = true,
                                   name = Func.newString "caller",
                                   raises = NONE,
                                   returns = NONE,
                                   start = callerL}
         val p = Program.T {datatypes = Vector.new0(),
                            functions = [callee, caller],
                            globals = Vector.new0(),
                            main = Func.newString "caller"}
         val vm = PreFlatten.newVarConsumerManager p
         val tCall = Block.transfer callerBlock
         val _ = PreFlatten.markConsumersInTransfer (vm, tCall)
         val consumers = PreFlatten.getVarConsumers (vm, v1)
         val _ = assert (List.exists (consumers, fn PreFlatten.AsAlias v' => Var.equals (v', r1) | _ => false),
                         "Expected AsAlias r1 for v1 in Return")
         val _ = PreFlatten.destroyVarConsumerManager vm
      in
         ()
      end

      val _ = let
         val _ = print "Test 24d: Multiple Return alias\n"
         val v1 = Var.fromString "v1"
         val v2 = Var.fromString "v2"
         val r1 = Var.fromString "r1"
         val fName = Func.newString "callee"
         val calleeL1 = Label.newString "L1"
         val calleeL2 = Label.newString "L2"
         val callee = Function.new {args = Vector.new0(),
                                   blocks = Vector.fromList [Block.T {args = Vector.new0(),
                                                                label = calleeL1,
                                                                statements = Vector.new0(),
                                                                transfer = Transfer.Return (Vector.new1 v1)},
                                                       Block.T {args = Vector.new0(),
                                                                label = calleeL2,
                                                                statements = Vector.new0(),
                                                                transfer = Transfer.Return (Vector.new1 v2)}],
                                   mayInline = true,
                                   name = fName,
                                   raises = NONE,
                                   returns = SOME (Vector.new1 Type.unit),
                                   start = calleeL1}
         val contL = Label.newString "cont"
         val contBlock = Block.T {args = Vector.new1 (r1, Type.unit),
                                  label = contL,
                                  statements = Vector.new0(),
                                  transfer = Transfer.Return (Vector.new0())}
         val callerL = Label.newString "caller"
         val callerBlock = Block.T {args = Vector.new0(),
                                    label = callerL,
                                    statements = Vector.new0(),
                                    transfer = Transfer.Call {args = Vector.new0(),
                                                              func = fName,
                                                              return = Return.NonTail {cont = contL, handler = Handler.Caller}}}
         val caller = Function.new {args = Vector.new0(),
                                   blocks = Vector.fromList [callerBlock, contBlock],
                                   mayInline = true,
                                   name = Func.newString "caller",
                                   raises = NONE,
                                   returns = NONE,
                                   start = callerL}
         val p = Program.T {datatypes = Vector.new0(),
                            functions = [callee, caller],
                            globals = Vector.new0(),
                            main = Func.newString "caller"}
         val vm = PreFlatten.newVarConsumerManager p
         val tCall = Block.transfer callerBlock
         val _ = PreFlatten.markConsumersInTransfer (vm, tCall)
         
         val consumers1 = PreFlatten.getVarConsumers (vm, v1)
         val _ = assert (List.exists (consumers1, fn PreFlatten.AsAlias v' => Var.equals (v', r1) | _ => false),
                         "Expected AsAlias r1 for v1")
         
         val consumers2 = PreFlatten.getVarConsumers (vm, v2)
         val _ = assert (List.exists (consumers2, fn PreFlatten.AsAlias v' => Var.equals (v', r1) | _ => false),
                         "Expected AsAlias r1 for v2")
                         
         val _ = PreFlatten.destroyVarConsumerManager vm
      in
         ()
      end
      val _ = print "Test 24 passed\n"
   in () end

   (* Test 25: updateChoiceForPolicy *)
   val _ = let
      val _ = print "Test 25: updateChoiceForPolicy\n"
      val xs = Vector.fromList [Var.fromString "x1", Var.fromString "x2"]
      
      val _ = print "Test 25a: PreserveVar always preserved (with consumers)\n"
      val res1 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenAlways 
                   (PreFlatten.PreserveVar, [PreFlatten.AsUnpacked, PreFlatten.AsCurrent])
      val _ = case res1 of PreFlatten.PreserveVar => () | _ => printFail "25a failed"
      
      val _ = print "Test 25b: FlattenAlways flattens if possible (with consumers)\n"
      val res2 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenAlways 
                   (PreFlatten.FlattenTupleVar xs, [PreFlatten.AsCurrent])
      val _ = case res2 of PreFlatten.FlattenTupleVar _ => () | _ => printFail "25b failed"

      val _ = print "Test 25c: FlattenForAnyUnpack flattens if any AsUnpacked\n"
      val res3 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAnyUnpack 
                   (PreFlatten.FlattenTupleVar xs, [PreFlatten.AsCurrent, PreFlatten.AsUnpacked])
      val _ = case res3 of PreFlatten.FlattenTupleVar _ => () | _ => printFail "25c failed"

      val _ = print "Test 25d: FlattenForAnyUnpack preserves if only AsCurrent\n"
      val res4 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAnyUnpack 
                   (PreFlatten.FlattenTupleVar xs, [PreFlatten.AsCurrent, PreFlatten.AsCurrent])
      val _ = case res4 of PreFlatten.PreserveVar => () | _ => printFail "25d failed"

      val _ = print "Test 25e: FlattenForAnyUnpack does not flatten for empty\n"
      val res4 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAnyUnpack 
                   (PreFlatten.FlattenTupleVar xs, [])
      val _ = case res4 of PreFlatten.PreserveVar => () | _ => printFail "25d failed"

      val _ = print "Test 25 passed\n"
   in () end

   (* Test 26: newVarConsumersForProgram with all consumer types *)
end
