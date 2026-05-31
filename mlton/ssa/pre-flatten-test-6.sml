local
   open Ssa
in
   val _ = let
      val _ = print "Test 46: flattenableTypesPolicy - FlattenOnlyConApp with ConApps\n"
      val conName = Con.fromString "Ty"
      val tBool = Type.bool
      val tyconName = Tycon.fromString "T"
      val datatypes = Vector.new1 (Datatype.T {
         cons = Vector.new1 {con = conName, args = Vector.new1 tBool},
         tycon = tyconName
      })
      val tData = Type.datatypee tyconName

      val fName = Func.fromString "f46"
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.new1 (Var.fromString "arg1", tData),
         blocks = Vector.new1 (Block.T {
            args = Vector.new0 (),
            label = fLf,
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }),
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fLf
      }

      val mainName = Func.fromString "main46"
      val b1 = Var.fromString "b1"
      val x = Var.fromString "x"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b1}
      val s2 = Statement.T {exp = Exp.ConApp {args = Vector.new1 b1, con = conName}, ty = tData, var = SOME x}
      
      val mainL = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainL,
         statements = Vector.fromList [s1, s2],
         transfer = Transfer.Call {
            args = Vector.new1 x,
            func = fName,
            return = Return.Tail
         }
      }
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.new1 mainBlock,
         mayInline = true,
         name = mainName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = mainL
      }
      val p = Program.T {
         datatypes = datatypes,
         functions = [fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }

      (* Should flatten ConApps when policy is FlattenOnlyConApp *)
      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenOnlyConApp, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                 SOME _ => ()
               | NONE => printFail "Test 46: expected SOME (ConApps should not flatten under FlattenOnlyConApp), got SOME"

      val _ = print "Test 46 passed\n"
   in () end

   (* Test 47: markTypeForArgs *)
   val _ = let
      val _ = print "Test 47: markTypeForArgs\n"
      val vcm = PreFlatten.newVarChoiceManager ()
      
      val fArg = Var.fromString "fArg"
      val tBool = Type.bool
      val bArg = Var.fromString "bArg"
      val tUnit = Type.unit
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(fArg, tBool)],
         blocks = Vector.fromList [Block.T {
            args = Vector.fromList [(bArg, tUnit)],
            label = fLf,
            statements = Vector.new0 (),
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = Func.fromString "f47",
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fLf
      }
      
      val _ = PreFlatten.markTypeForArgs (vcm, fFunction)
      
      (* Test function argument via ConApp *)
      val vConF = Var.fromString "vConF"
      val conF = Con.fromString "CF"
      val sConF = Statement.T {
         exp = Exp.ConApp {args = Vector.new1 fArg, con = conF},
         ty = Type.unit,
         var = SOME vConF
      }
      val _ = PreFlatten.chooseVarsInStatement (vcm, sConF)
      val choiceF = PreFlatten.getVarChoice (vcm, vConF)
      val _ = case choiceF of
                 PreFlatten.FlattenConVar {args, ...} =>
                    let val (_, t) = Vector.sub (args, 0) in
                       assert (Type.equals (t, tBool), "fArg type mismatch")
                    end
               | _ => printFail "Expected FlattenConVar for vConF"

      (* Test block argument via ConApp *)
      val vConB = Var.fromString "vConB"
      val conB = Con.fromString "CB"
      val sConB = Statement.T {
         exp = Exp.ConApp {args = Vector.new1 bArg, con = conB},
         ty = Type.unit,
         var = SOME vConB
      }
      val _ = PreFlatten.chooseVarsInStatement (vcm, sConB)
      val choiceB = PreFlatten.getVarChoice (vcm, vConB)
      val _ = case choiceB of
                 PreFlatten.FlattenConVar {args, ...} =>
                    let val (_, t) = Vector.sub (args, 0) in
                       assert (Type.equals (t, tUnit), "bArg type mismatch")
                    end
               | _ => printFail "Expected FlattenConVar for vConB"
      
      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 47 passed\n"
   in () end

   (* Test 48: newVarChoicesForProgram ensures globals are visited *)
   val _ = let
      val _ = print "Test 48: newVarChoicesForProgram ensures globals are visited\n"
      val g1 = Var.fromString "g1"
      val tBool = Type.bool
      val gs1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME g1}
      
      val vc = Var.fromString "vc"
      val con = Con.fromString "C"
      val gs2 = Statement.T {
         exp = Exp.ConApp {args = Vector.new1 g1, con = con},
         ty = Type.unit,
         var = SOME vc
      }
      
      val mainFunc = Func.fromString "main48"
      val mainLabel = Label.fromString "L48"
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
         globals = Vector.fromList [gs1, gs2],
         main = mainFunc
      }

      val vcm = PreFlatten.newVarChoicesForProgram p
      val choice = PreFlatten.getVarChoice (vcm, vc)
      val _ = case choice of
                 PreFlatten.FlattenConVar {args, ...} =>
                    let val (_, t) = Vector.sub (args, 0) in
                       assert (Type.equals (t, tBool), "global g1 type mismatch")
                    end
               | _ => printFail "Expected FlattenConVar for vc"
      
      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 48 passed\n"
   in () end

   (* Test 49: transform with postFlatten *)
   val _ = let
      val _ = print "Test 49: transform with postFlatten\n"
      val mainFunc = Func.fromString "main49"
      val mainLabel = Label.fromString "L49"
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

      val _ = Control.preFlattenPostSteps := [Control.PreFlattenPostStep.Flatten]
      val _ = PreFlatten.transform p
      val _ = print "Test 49 passed\n"
   in () end

   (* Test 50: Type.layout *)
   val _ = let
      val _ = print "Test 50: Type.layout\n"
      val w32 = WordSize.word32
      val t1 = Type.word w32
      val t2 = Type.array t1
      val t3 = Type.array t2

      fun check (t, expected) =
         let
            val actual = Layout.toString (Type.layout t)
         in
            if actual = expected then ()
            else (print ("Type layout mismatch\n");
                  print ("Expected: " ^ expected ^ "\n");
                  print ("Actual:   " ^ actual ^ "\n");
                  OS.Process.exit OS.Process.failure)
         end

      val _ = check (t1, "word32")
      val _ = check (t2, "(word32) array")
      val _ = check (t3, "((word32) array) array")
      val _ = print "Test 50 passed\n"
   in () end

   (* Test 51: Type.layout with maxTypePrintDepth *)
   val _ = let
      val _ = print "Test 51: Type.layout with maxTypePrintDepth\n"
      val tBool = Type.bool
      val tTupInner = Type.tuple (Vector.fromList [tBool, tBool])
      val tTupOuter = Type.tuple (Vector.fromList [tTupInner, tBool])
      val tDeep = Type.tuple (Vector.fromList [tTupOuter, tBool])

      fun check (t, depth, expected) =
         let
            val oldDepth = !Control.maxTypePrintDepth
            val _ = Control.maxTypePrintDepth := depth
            val actual = Layout.toString (Type.layout t)
            val _ = Control.maxTypePrintDepth := oldDepth
         in
            if actual = expected then ()
            else (print ("Type layout mismatch at depth " ^ (Int.toString depth) ^ "\n");
                  print ("Expected: " ^ expected ^ "\n");
                  print ("Actual:   " ^ actual ^ "\n");
                  OS.Process.exit OS.Process.failure)
         end

      (* tDeep is (((bool, bool) tuple, bool) tuple, bool) tuple *)
      
      val _ = print "Testing depth 0 (print fully)\n"
      val _ = check (tDeep, 0, "(((bool, bool) tuple, bool) tuple, bool) tuple")

      val _ = print "Testing depth 1\n"
      val _ = check (tDeep, 1, "(...) tuple")

      val _ = print "Testing depth 2\n"
      val _ = check (tDeep, 2, "((...) tuple, bool) tuple")

      val _ = print "Testing depth 3\n"
      val _ = check (tDeep, 3, "(((...) tuple, bool) tuple, bool) tuple")

      val _ = print "Testing depth 4\n"
      val _ = check (tDeep, 4, "(((bool, bool) tuple, bool) tuple, bool) tuple")

      val _ = print "Test 51 passed\n"
   in () end

   (* Test 52: buildFlattenedBlock with Preserve and FlattenTuple *)
   val _ = let
      val _ = print "Test 52: buildFlattenedBlock with Preserve and FlattenTuple\n"
      val l1 = Label.fromString "L1"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])
      
      val b1 = Block.T {
         args = Vector.fromList [(v1, t1), (v2, tTuple)],
         label = l1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.fromList [v1])
      }

      val res = PreFlatten.buildFlattenedBlock
                    (b1, Vector.fromList [PreFlatten.Preserve,
                                          PreFlatten.FlattenTuple])
      val Block.T {args, label = resLabel, statements, ...} = res
      
      val _ = assert (not (Label.equals (l1, resLabel)), "new block must have a fresh label")
      val _ = assert (Vector.length args = 3, "Flattened block should have 3 args")
      
      val (_, rt0) = Vector.sub (args, 0)
      val (_, rt1) = Vector.sub (args, 1)
      val (_, rt2) = Vector.sub (args, 2)
      val _ = assert (Type.equals (rt0, t1), "Arg 0 type mismatch")
      val _ = assert (Type.equals (rt1, t1), "Arg 1 type mismatch")
      val _ = assert (Type.equals (rt2, t2), "Arg 2 type mismatch")
      
      (* Check that original vars are bound in statements *)
      val foundV2 = Vector.exists (statements, fn Statement.T {var, ...} =>
         case var of
            SOME v => Var.originalName v = "v2"
          | NONE => false)
      val _ = assert (foundV2, "Original var v2 not found in statements")

      val _ = print "Test 52 passed\n"
   in () end

   (* Test 53: buildFlattenedBlock with FlattenCon *)
   val _ = let
      val _ = print "Test 53: buildFlattenedBlock with FlattenCon\n"
      val l1 = Label.fromString "L1"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val t2 = Type.unit
      val con = Con.fromString "MyCon"
      
      val tCon = Type.datatypee (Tycon.fromString "MyCon")
      
      val b1 = Block.T {
         args = Vector.new1 (v1, tCon),
         label = l1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      
      val res = PreFlatten.buildFlattenedBlock
                    (b1, Vector.new1 (PreFlatten.FlattenCon {argTys = Vector.new2 (t1, t2), con = con}))
      
      val Block.T {args, statements, ...} = res
      val _ = assert (Vector.length args = 2, "Flattened block should have 2 args")
      val _ = assert (Vector.length statements = 1, "Should have 1 statement to bind the constructor")
      
      val _ = print "Test 53 passed\n"
   in () end

   (* Test 54: buildFlattenedBlock with mixed arguments *)
   val _ = let
      val _ = print "Test 54: buildFlattenedBlock with mixed arguments\n"
      val l1 = Label.fromString "L1"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])
      val v3 = Var.fromString "v3"
      val con = Con.fromString "MyCon"
      val tCon = Type.datatypee (Tycon.fromString "MyCon")

      val b1 = Block.T {
         args = Vector.fromList [(v1, t1), (v2, tTuple), (v3, tCon)],
         label = l1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }

      val res = PreFlatten.buildFlattenedBlock
                    (b1, Vector.fromList [PreFlatten.Preserve,
                                          PreFlatten.FlattenTuple,
                                          PreFlatten.FlattenCon {argTys = Vector.new2 (t1, t2), con = con}])
      val Block.T {args, statements, ...} = res
      
      val _ = assert (Vector.length args = 5, "Flattened block should have 5 args (1 + 2 + 2)")
      
      val _ = print "Test 54 passed\n"
   in () end

   (* Test 55: blockManager *)
   val _ = let
      val _ = print "Test 55: blockManager\n"
      val fName = Func.fromString "f55"
      val l1 = Label.fromString "L1"
      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val tTuple = Type.tuple (Vector.fromList [t1, t2])

      val b1 = Block.T {
         args = Vector.fromList [(v1, t1), (v2, tTuple)],
         label = l1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      val f = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [b1],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = l1
      }

      val bm = PreFlatten.newBlockManager f

      val _ = print "Test 55a: getOrCreateBlock NoOp\n"
      val lNoOp = PreFlatten.getOrCreateBlock (bm, l1, Vector.fromList [PreFlatten.Preserve, PreFlatten.Preserve])
      val _ = assert (Label.equals (lNoOp, l1), "NoOp should return original Label.t")

      val _ = print "Test 55b: extractNewBlocks after NoOp\n"
      val newBlocks0 = PreFlatten.extractNewBlocks bm
      val _ = assert (List.length newBlocks0 = 0, "No new blocks should be extracted after NoOp")

      val _ = print "Test 55c: getOrCreateBlock Valid\n"
      val lFlattened = PreFlatten.getOrCreateBlock (bm, l1, Vector.fromList [PreFlatten.Preserve, PreFlatten.FlattenTuple])
      val _ = assert (not (Label.equals (lFlattened, l1)), "Valid flattening should return new Label.t")

      val _ = print "Test 55d: extractNewBlocks after Valid\n"
      val newBlocks1 = PreFlatten.extractNewBlocks bm
      val _ = assert (List.length newBlocks1 = 1, "One new block should be extracted")
      val b_res = case newBlocks1 of (x::_) => x | _ => (print "Expected non-empty list\n"; OS.Process.exit OS.Process.failure)
      val _ = assert (Label.equals (Block.label b_res, lFlattened), "Extracted block label mismatch")

      val _ = print "Test 55e: extractNewBlocks should clear pending\n"
      val newBlocks2 = PreFlatten.extractNewBlocks bm
      val _ = assert (List.length newBlocks2 = 0, "extractNewBlocks should clear pending list")

      val _ = PreFlatten.destroyBlockManager bm
      val _ = print "Test 55 passed\n"
   in () end

   (* Test 56: blockManager destroy error if pending *)

   (* Test 57: Type.layout with 5-element tuple at depth 1 *)
   val _ = let
      val _ = print "Test 57: Type.layout with 5-element tuple at depth 1\n"
      val tBool = Type.bool
      val tW32 = Type.word WordSize.word32
      val tTup = Type.tuple (Vector.fromList [tBool, tBool, tBool, tBool, tW32])
      
      val oldDepth = !Control.maxTypePrintDepth
      val _ = Control.maxTypePrintDepth := 1
      val actual = Layout.toString (Type.layout tTup)
      val _ = Control.maxTypePrintDepth := oldDepth
      
      val expected = "(...) tuple"
   in
      if actual = expected then
         print "Test 57 passed\n"
      else
         (print ("Test 57 failed\n");
          print ("Expected: " ^ expected ^ "\n");
          print ("Actual:   " ^ actual ^ "\n");
          OS.Process.exit OS.Process.failure)
   end
end
