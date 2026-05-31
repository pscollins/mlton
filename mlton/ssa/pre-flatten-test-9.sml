local
   open Ssa
in
   (* Test 57: transform with preFlattenLevelSteps = [Function] *)
   val _ = let
      val _ = print "Test 57: transform with preFlattenLevelSteps = [Function]\n"
      val mainFunc = Func.fromString "main57"
      val mainLabel = Label.fromString "L57"
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

      val _ = Control.preFlattenLevelSteps := [Control.PreFlattenLevelStep.Function]
      val _ = PreFlatten.transform p
      val _ = print "Test 57 passed\n"
   in () end

   (* Test 58: transform with preFlattenLevelSteps = [Block] *)
   val _ = let
      val _ = print "Test 58: transform with preFlattenLevelSteps = [Block]\n"
      val mainFunc = Func.fromString "main58"
      val mainLabel = Label.fromString "L58"
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

      val _ = Control.preFlattenLevelSteps := [Control.PreFlattenLevelStep.Block]
      val _ = PreFlatten.transform p
      val _ = print "Test 58 passed\n"
   in () end

   (* Test 59: transform with preFlattenLevelSteps = [Function, Block] *)
   val _ = let
      val _ = print "Test 59: transform with preFlattenLevelSteps = [Function, Block]\n"
      val mainFunc = Func.fromString "main59"
      val mainLabel = Label.fromString "L59"
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

      val _ = Control.preFlattenLevelSteps := [Control.PreFlattenLevelStep.Function, Control.PreFlattenLevelStep.Block]
      val _ = PreFlatten.transform p
      val _ = print "Test 59 passed\n"
   in () end

   (* Test 60-62: recursiveFlattenPolicy *)
   val _ = let
      val _ = print "Test 60-62: recursiveFlattenPolicy\n"
      
      val fRecName = Func.fromString "f_rec"
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])
      
      val fLf = Label.fromString "Lf"
      
      val arg1 = Var.fromString "arg1"
      val u1 = Var.fromString "u1"
      val s_unpack = Statement.T {
         exp = Exp.Select {offset = 0, tuple = arg1},
         ty = tBool,
         var = SOME u1
      }
      val t1 = Var.fromString "t1"
      val t2 = Var.fromString "t2"
      val x = Var.fromString "x"
      val s1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t1}
      val s2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME t2}
      val s3 = Statement.T {exp = Exp.Tuple (Vector.fromList [t1, t2]), ty = tTuple, var = SOME x}

      val fFunction = Function.new {
         args = Vector.fromList [(arg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = fLf,
            statements = Vector.fromList [s_unpack, s1, s2, s3],
            transfer = Transfer.Call {
               args = Vector.fromList [x],
               func = fRecName,
               return = Return.Tail
            }
         }],
         mayInline = true,
         name = fRecName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fLf
      }

      val mainRecName = Func.fromString "main_rec"
      val mainL = Label.fromString "Lmain"
      val m1 = Var.fromString "m1"
      val m2 = Var.fromString "m2"
      val mx = Var.fromString "mx"
      val ms1 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME m1}
      val ms2 = Statement.T {exp = Exp.unit, ty = tBool, var = SOME m2}
      val ms3 = Statement.T {exp = Exp.Tuple (Vector.fromList [m1, m2]), ty = tTuple, var = SOME mx}
      
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainL,
         statements = Vector.fromList [ms1, ms2, ms3],
         transfer = Transfer.Call {
            args = Vector.fromList [mx],
            func = fRecName,
            return = Return.Tail
         }
      }
      val mainFunction = Function.new {
         args = Vector.new0 (),
         blocks = Vector.fromList [mainBlock],
         mayInline = true,
         name = mainRecName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = mainL
      }
      val p = Program.T {
         datatypes = Vector.new0 (),
         functions = [fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainRecName
      }

      (* Test 60: noRecursiveFlatten *)
      val _ = let
         val _ = print "Running Test 60 (noRecursiveFlatten)...\n"
         val _ = printProgram ("60 (before)", p)
         val res = PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p
         val p' = (case res of
                      SOME p' => p'
                    | NONE => (printFail "Test 60 failed: flattenOnce returned NONE"))
         val _ = printProgram ("60 (after)", p')
         
         val Program.T {functions, ...} = p'
         
         fun findF name = List.peek (functions, fn f => Func.equals (Function.name f, name))
         
         val _ = case findF fRecName of
                    SOME _ => ()
                  | NONE => printFail "Test 60 failed: original f_rec not found"
         
         val fFlat = case List.peek (functions, fn f => not (Func.equals (Function.name f, fRecName)) andalso not (Func.equals (Function.name f, mainRecName))) of
                        SOME f => f
                      | NONE => printFail "Test 60 failed: flattened function not found"
         
         fun findCall f =
            let
               val blocks = Function.blocks f
               fun loop i =
                  if i >= Vector.length blocks then NONE
                  else
                     case Vector.sub (blocks, i) of
                        Block.T {transfer = Transfer.Call {func, ...}, ...} => SOME func
                      | _ => loop (i + 1)
            in
               loop 0
            end

         val target = case findCall fFlat of
                         SOME func => func
                       | NONE => printFail "Test 60 failed: fFlat does not have a call transfer"
         
         val _ = if Func.equals (target, fRecName) then ()
                 else printFail ("Test 60 failed: fFlat calls " ^ (Func.toString target) ^ " instead of " ^ (Func.toString fRecName))
         
         val _ = print "Test 60 passed\n"
      in () end

      (* Test 61: recursiveFlattenSteps 1 *)
      val _ = let
         val _ = print "Running Test 61 (recursiveFlattenSteps 1)...\n"
         val _ = printProgram ("61 (before)", p)
         val res = PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.recursiveFlattenSteps 1) p
         val p'' = (case res of
                      SOME p'' => p''
                    | NONE => (printFail "Test 61 failed: flattenOnce returned NONE"))
         val _ = printProgram ("61 (after)", p'')
         
         val Program.T {functions, ...} = p''
         
         val fFlat = case List.peek (functions, fn f => not (Func.equals (Function.name f, fRecName)) andalso not (Func.equals (Function.name f, mainRecName))) of
                        SOME f => f
                      | NONE => printFail "Test 61 failed: flattened function not found"
         
         fun findCall f =
            let
               val blocks = Function.blocks f
               fun loop i =
                  if i >= Vector.length blocks then NONE
                  else
                     case Vector.sub (blocks, i) of
                        Block.T {transfer = Transfer.Call {func, ...}, ...} => SOME func
                      | _ => loop (i + 1)
            in
               loop 0
            end

         val target = case findCall fFlat of
                         SOME func => func
                       | NONE => printFail "Test 61 failed: fFlat does not have a call transfer"
         
         val _ = if Func.equals (target, Function.name fFlat) then ()
                 else printFail ("Test 61 failed: fFlat calls " ^ (Func.toString target) ^ " instead of itself " ^ (Func.toString (Function.name fFlat)))
         
         val _ = print "Test 61 passed\n"
      in () end

      (* Test 62: recursiveFlattenSteps 0 *)
      val _ = let
         val _ = print "Running Test 62 (recursiveFlattenSteps 0)...\n"
         val _ = printProgram ("62 (before)", p)
         val _ = (PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.recursiveFlattenSteps 0) p;
                  printFail "Test 62 failed: expected flattenOnce to return an error for recursiveFlattenSteps 0, but it returned normally")
                 handle _ => print "Test 62 passed (caught expected error)\n"
      in () end

      (* Test 63: pre-flatten-recursive-steps = 1 via transform *)
      val _ = let
         val _ = print "Running Test 63 (pre-flatten-recursive-steps = 1 via transform)...\n"
         val _ = Control.preFlattenRecursiveSteps := 1
         val _ = Control.preFlattenMaxIters := 1
         val _ = Control.preFlattenLevelSteps := [Control.PreFlattenLevelStep.Function]
         
         val p' = PreFlatten.transform p
         val _ = printProgram ("63 (after)", p')
         
         val Program.T {functions, ...} = p'
         val fFlat = case List.peek (functions, fn f => not (Func.equals (Function.name f, fRecName)) andalso not (Func.equals (Function.name f, mainRecName))) of
                        SOME f => f
                      | NONE => printFail "Test 63 failed: flattened function not found"
         
         fun findCall f =
            let
               val blocks = Function.blocks f
               fun loop i =
                  if i >= Vector.length blocks then NONE
                  else
                     case Vector.sub (blocks, i) of
                        Block.T {transfer = Transfer.Call {func, ...}, ...} => SOME func
                      | _ => loop (i + 1)
            in
               loop 0
            end

         val target = case findCall fFlat of
                         SOME func => func
                       | NONE => printFail "Test 63 failed: fFlat does not have a call transfer"
         
         val _ = if Func.equals (target, Function.name fFlat) then ()
                 else printFail ("Test 63 failed: fFlat calls " ^ (Func.toString target) ^ " instead of itself " ^ (Func.toString (Function.name fFlat)))
         
         val _ = print "Test 63 passed\n"
      in () end

      (* Test 64: pre-flatten-recursive-steps = 0 via transform (no recursive flatten) *)
      val _ = let
         val _ = print "Running Test 64 (pre-flatten-recursive-steps = 0 via transform)...\n"
         val _ = Control.preFlattenRecursiveSteps := 0
         val _ = Control.preFlattenMaxIters := 1
         val _ = Control.preFlattenLevelSteps := [Control.PreFlattenLevelStep.Function]
         
         val p' = PreFlatten.transform p
         val _ = printProgram ("64 (after)", p')
         
         val Program.T {functions, ...} = p'
         val fFlat = case List.peek (functions, fn f => not (Func.equals (Function.name f, fRecName)) andalso not (Func.equals (Function.name f, mainRecName))) of
                        SOME f => f
                      | NONE => printFail "Test 64 failed: flattened function not found"
         
         fun findCall f =
            let
               val blocks = Function.blocks f
               fun loop i =
                  if i >= Vector.length blocks then NONE
                  else
                     case Vector.sub (blocks, i) of
                        Block.T {transfer = Transfer.Call {func, ...}, ...} => SOME func
                      | _ => loop (i + 1)
            in
               loop 0
            end

         val target = case findCall fFlat of
                         SOME func => func
                       | NONE => printFail "Test 64 failed: fFlat does not have a call transfer"
         
         val _ = if Func.equals (target, fRecName) then ()
                 else printFail ("Test 64 failed: fFlat calls " ^ (Func.toString target) ^ " instead of " ^ (Func.toString fRecName))
         
         val _ = print "Test 64 passed\n"
      in () end

   in () end
end
