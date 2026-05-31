local
   open Ssa
in
   val _ = let
      val _ = print "Test 36: buildFlattenedFunctionWithDatatype (nullary)\n"
      val fName = Func.fromString "f36"
      val l1 = Label.fromString "L36"
      val v1 = Var.fromString "v1"
      val tCon = Type.datatypee (Tycon.fromString "T0")
      val b1 = Block.T {
         args = Vector.new0 (),
         label = l1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      val f = Function.new {
         args = Vector.fromList [(v1, tCon)],
         blocks = Vector.fromList [b1],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = l1
      }

      val con = Con.fromString "C0"
      val choice = PreFlatten.FlattenCon {argTys = Vector.new0 (), con = con}

      val _ = print "Checking checkFlatteningChoice for datatype (nullary)\n"
      val choiceRes = PreFlatten.checkFlatteningChoice (f, Vector.fromList [choice])
      val _ = assert (choiceRes = PreFlatten.Valid, "Expected Valid for datatype flattening (nullary), got " ^ (choiceResToString choiceRes))

      val _ = print "Attempting buildFlattenedFunction for datatype (nullary)\n"
      val res = PreFlatten.buildFlattenedFunction (f, Vector.fromList [choice])
      val _ = print "Test 36 passed\n"
   in () end

   (* Test 37: chooseVarsInStatement with FlattenConVar (nullary) *)
   val _ = let
      val _ = print "Test 37: chooseVarsInStatement with FlattenConVar (nullary)\n"
      val vcm = PreFlatten.newVarChoiceManager ()

      val vCon = Var.fromString "vc"
      val con = Con.fromString "C0"

      val sCon = Statement.T {
         exp = Exp.ConApp {args = Vector.new0 (), con = con},
         ty = Type.unit,
         var = SOME vCon
      }

      val _ = PreFlatten.chooseVarsInStatement (vcm, sCon)

      val choiceCon = PreFlatten.getVarChoice (vcm, vCon)
      val _ =
         case choiceCon of
            PreFlatten.FlattenConVar {args, con = con'} =>
               let
                  val _ = assert (Vector.length args = 0, "FlattenConVar should have 0 vars")
                  val _ = assert (Con.equals (con, con'), "FlattenConVar con mismatch")
               in () end
          | _ => (print "vc should be FlattenConVar\n"; OS.Process.exit OS.Process.failure)

      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 37 passed\n"
   in () end

   (* Test 38: choiceEqual *)
   val _ = let
      val _ = print "Test 38: choiceEqual\n"
      open PreFlatten
      val c1 = Con.fromString "C1"
      val c2 = Con.fromString "C2"
      val tys1 = Vector.fromList [Type.bool]
      val tys2 = Vector.fromList [Type.unit]

      val fc1a = FlattenCon {argTys = tys1, con = c1}
      val fc1b = FlattenCon {argTys = tys1, con = c1}
      val fc1c = FlattenCon {argTys = tys2, con = c1}
      val fc2 = FlattenCon {argTys = tys1, con = c2}

      val _ = assert (choiceEqual (Preserve, Preserve), "Preserve = Preserve")
      val _ = assert (choiceEqual (FlattenTuple, FlattenTuple), "FlattenTuple = FlattenTuple")
      val _ = assert (choiceEqual (fc1a, fc1b), "FlattenCon equal case")
      
      val _ = assert (not (choiceEqual (Preserve, FlattenTuple)), "Preserve != FlattenTuple")
      val _ = assert (not (choiceEqual (FlattenTuple, fc1a)), "FlattenTuple != FlattenCon")
      val _ = assert (not (choiceEqual (fc1a, fc2)), "FlattenCon != FlattenCon (diff con)")
      val _ = assert (not (choiceEqual (fc1a, fc1c)), "FlattenCon != FlattenCon (diff tys)")

      val _ = print "Test 38 passed\n"
   in () end

   (* Test 39: updateChoiceForAllowedTypes *)
   val _ = let
      val _ = print "Test 39: updateChoiceForAllowedTypes\n"
      val xs = Vector.fromList [Var.fromString "x1", Var.fromString "x2"]
      val args = Vector.fromList [(Var.fromString "v1", Type.bool)]
      val con = Con.fromString "C"
      
      val vcTuple = PreFlatten.FlattenTupleVar xs
      val vcCon = PreFlatten.FlattenConVar {args = args, con = con}
      val vcPreserve = PreFlatten.PreserveVar

      fun isPreserve vc = case vc of PreFlatten.PreserveVar => true | _ => false
      fun isTuple vc = case vc of PreFlatten.FlattenTupleVar _ => true | _ => false
      fun isCon vc = case vc of PreFlatten.FlattenConVar _ => true | _ => false

      val _ = print "Test 39a: FlattenAnyType\n"
      val _ = assert (isTuple (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenAnyType vcTuple), "Any: tuple should be preserved")
      val _ = assert (isCon (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenAnyType vcCon), "Any: con should be preserved")
      val _ = assert (isPreserve (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenAnyType vcPreserve), "Any: preserve should be preserved")

      val _ = print "Test 39b: FlattenOnlyTuple\n"
      val _ = assert (isTuple (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenOnlyTuple vcTuple), "OnlyTuple: tuple should be preserved")
      val _ = assert (isPreserve (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenOnlyTuple vcCon), "OnlyTuple: con should be dropped")
      val _ = assert (isPreserve (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenOnlyTuple vcPreserve), "OnlyTuple: preserve should be preserved")

      val _ = print "Test 39c: FlattenOnlyConApp\n"
      val _ = assert (isPreserve (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenOnlyConApp vcTuple), "OnlyCon: tuple should be dropped")
      val _ = assert (isCon (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenOnlyConApp vcCon), "OnlyCon: con should be preserved")
      val _ = assert (isPreserve (PreFlatten.updateChoiceForAllowedTypes PreFlatten.FlattenOnlyConApp vcPreserve), "OnlyCon: preserve should be preserved")

      val _ = print "Test 39 passed\n"
   in () end

   (* Test 40: markConsumersInTransfer with Case *)
   val _ = let
      val _ = print "Test 40: markConsumersInTransfer with Case\n"
      val vTest = Var.fromString "vTest"
      val lDefault = Label.fromString "Ldefault"
      val caseTransfer = Transfer.Case {
         cases = Cases.Con (Vector.new0 ()),
         default = SOME lDefault,
         test = vTest
      }
      
      val dummyLabel = Func.fromString "dummy"
      val p = Program.T {datatypes=Vector.new0(),
                         functions=[],
                         globals=Vector.new0(),
                         main=dummyLabel}
      val vm = PreFlatten.newVarConsumerManager p
      val _ = PreFlatten.markConsumersInTransfer (vm, caseTransfer)
      val consumers = PreFlatten.getVarConsumers (vm, vTest)
      val _ = assert (List.exists (consumers, fn PreFlatten.AsUnpacked => true | _ => false),
                      "Expected AsUnpacked for test variable in Case")
      val _ = PreFlatten.destroyVarConsumerManager vm
      val _ = print "Test 40 passed\n"
   in () end

   (* Test 41: ConApp flattening (2-ary) via flattenOnce *)
   val _ = let
      val _ = print "Test 41: ConApp flattening (2-ary) via flattenOnce\n"
      val fName = Func.fromString "f41"
      val tBool = Type.bool
      val tUnit = Type.unit
      val tCon = Type.datatypee (Tycon.fromString "T41")
      val con = Con.fromString "C41"
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tCon)],
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

      val mainName = Func.fromString "main41"
      val b1 = Var.fromString "b1"
      val u1 = Var.fromString "u1"
      val x = Var.fromString "x"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME b1}
      val s2 = Statement.T {exp = Exp.unit, ty = tUnit, var = SOME u1}
      val s3 = Statement.T {exp = Exp.ConApp {args = Vector.fromList [b1, u1], con = con}, 
                            ty = tCon, var = SOME x}
      
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
                 | NONE => printFail "Test 41: flattenOnce returned NONE")
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
               val _ = assert (Type.equals (t1, tUnit), "Arg 1 should be unit")
            in () end
          | NONE => printFail "Test 41: Flattened function not found"
      end

      val _ = print "Test 41 passed\n"
   in () end

   (* Test 42: ConApp flattening (nullary) via flattenOnce *)
   val _ = let
      val _ = print "Test 42: ConApp flattening (nullary) via flattenOnce\n"
      val fName = Func.fromString "f42"
      val tCon = Type.datatypee (Tycon.fromString "T42")
      val con = Con.fromString "C42"
      
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(Var.fromString "arg1", tCon)],
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

      val mainName = Func.fromString "main42"
      val x = Var.fromString "x"
      val s1 = Statement.T {exp = Exp.ConApp {args = Vector.new0 (), con = con}, 
                            ty = tCon, var = SOME x}
      
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

      val p' = (case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                   SOME p' => p'
                 | NONE => printFail "Test 42: flattenOnce returned NONE")
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
               val _ = assert (Vector.length args = 0, "Expected 0 args in flattened function")
            in () end
          | NONE => printFail "Test 42: Flattened function not found"
      end

      val _ = print "Test 42 passed\n"
   in () end

   (* Test 43: flattenableTypesPolicy - FlattenOnlyConApp with Tuples *)
   val _ = let
      val _ = print "Test 43: flattenableTypesPolicy - FlattenOnlyConApp with Tuples\n"
      val fName = Func.fromString "f43"
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

      val mainName = Func.fromString "main43"
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

      (* Should NOT flatten tuples when policy is FlattenOnlyConApp *)
      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenOnlyConApp, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                 SOME _ => printFail "Test 43: expected NONE (tuples should not flatten under FlattenOnlyConApp), got SOME"
               | NONE => ()

      val _ = print "Test 43 passed\n"
   in () end

   (* Test 44: flattenableTypesPolicy - FlattenOnlyTuple with ConApps *)
   val _ = let
      val _ = print "Test 44: flattenableTypesPolicy - FlattenOnlyTuple with ConApps\n"
      val conName = Con.fromString "Ty"
      val tBool = Type.bool
      val tyconName = Tycon.fromString "T"
      val datatypes = Vector.new1 (Datatype.T {
         cons = Vector.new1 {con = conName, args = Vector.new1 tBool},
         tycon = tyconName
      })
      val tData = Type.datatypee tyconName

      val fName = Func.fromString "f44"
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

      val mainName = Func.fromString "main44"
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

      (* Should NOT flatten ConApps when policy is FlattenOnlyTuple *)
      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenOnlyTuple, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                 SOME _ => printFail "Test 44: expected NONE (ConApps should not flatten under FlattenOnlyTuple), got SOME"
               | NONE => ()

      val _ = print "Test 44 passed\n"
   in () end

   (* Test 45: flattenableTypesPolicy - FlattenOnlyTuple with Tuples *)
   val _ = let
      val _ = print "Test 45: flattenableTypesPolicy - FlattenOnlyTuple with Tuples\n"
      val fName = Func.fromString "f45"
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

      val mainName = Func.fromString "main45"
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

      (* Should flatten tuples when policy is FlattenOnlyTuple *)
      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenOnlyTuple, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                 SOME _ => ()
               | NONE => printFail "Test 45: expected SOME (tuples should flatten under FlattenOnlyTuple), got NONE"

      val _ = print "Test 45 passed\n"
   in () end

   (* Test 46: flattenableTypesPolicy - FlattenOnlyConApp with ConApps *)
end
