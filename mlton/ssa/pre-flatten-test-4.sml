local
   open Ssa
in
   val _ = let
      val _ = print "Test 26: newVarConsumersForProgram with all consumer types\n"
      
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])
      
      val fName = Func.fromString "f26"
      val fArg = Var.fromString "fArg"
      val fLf = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(fArg, tBool)],
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

      val mainName = Func.fromString "main26"
      val vUnpacked = Var.fromString "vUnpacked"
      val vCurrent = Var.fromString "vCurrent"
      val vAlias = Var.fromString "vAlias"
      
      val s1 = Statement.T {exp = Exp.Select {offset = 0, tuple = vUnpacked}, 
                            ty = tBool, var = SOME (Var.fromString "tmp1")}
      val s2 = Statement.T {exp = Exp.Tuple (Vector.fromList [vCurrent]), 
                            ty = tTuple, var = SOME (Var.fromString "tmp2")}
      
      val lMain = Label.fromString "Lmain"
      val mainBlock = Block.T {
         args = Vector.fromList [(vUnpacked, tTuple), (vCurrent, tBool), (vAlias, tBool)],
         label = lMain,
         statements = Vector.fromList [s1, s2],
         transfer = Transfer.Call {
            args = Vector.fromList [vAlias],
            func = fName,
            return = Return.Tail
         }
      }

      val mainFunction = Function.new {
         args = Vector.fromList [(vUnpacked, tTuple), (vCurrent, tBool), (vAlias, tBool)],
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

      val vm = PreFlatten.newVarConsumersForProgram p
      
      val consumersUnpacked = PreFlatten.getVarConsumers (vm, vUnpacked)
      val _ = assert (List.exists (consumersUnpacked, fn PreFlatten.AsUnpacked => true | _ => false),
                      "vUnpacked should have AsUnpacked consumer")
      
      val consumersCurrent = PreFlatten.getVarConsumers (vm, vCurrent)
      val _ = assert (List.exists (consumersCurrent, fn PreFlatten.AsCurrent => true | _ => false),
                      "vCurrent should have AsCurrent consumer")
      
      val consumersAlias = PreFlatten.getVarConsumers (vm, vAlias)
      val _ = assert (List.exists (consumersAlias, fn PreFlatten.AsAlias v' => Var.equals (v', fArg) | _ => false),
                      "vAlias should have AsAlias fArg consumer")
      
      val _ = PreFlatten.destroyVarConsumerManager vm
      val _ = print "Test 26 passed\n"
   in () end

   (* Test 27: transform with preFlattenConsumerPolicy *)
   val _ = let
      val _ = print "Test 27: transform with preFlattenConsumerPolicy\n"
      val fName = Func.fromString "f27"
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

      val mainName = Func.fromString "main27"
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

      (* Case 1: preFlattenConsumerPolicy = Always *)
      val _ = Control.preFlattenMaxIters := 1
      val _ = Control.preFlattenConsumerPolicy := Control.PreFlattenConsumerPolicy.Always
      val p1 = PreFlatten.transform p
      val Program.T {functions = funcs1, ...} = p1
      val _ = assert (List.length funcs1 = 3, "Expected 3 functions with policy Always")

      (* Case 2: preFlattenConsumerPolicy = AnyUnpack *)
      val _ = Control.preFlattenConsumerPolicy := Control.PreFlattenConsumerPolicy.AnyUnpack
      val p2 = PreFlatten.transform p
      val Program.T {functions = funcs2, ...} = p2
      (* AnyUnpack should NOT flatten here because arg1 is not unpacked in fFunction *)
      val _ = assert (List.length funcs2 = 2, "Expected 2 functions with policy AnyUnpack")

      (* Case 3: preFlattenConsumerPolicy = AllUnpack *)
      val _ = Control.preFlattenConsumerPolicy := Control.PreFlattenConsumerPolicy.AllUnpack
      val p3 = PreFlatten.transform p
      val Program.T {functions = funcs3, ...} = p3
      (* AllUnpack should flatten here because arg1 has NO consumers in fFunction (vacuously all unpacked) *)
      val _ = assert (List.length funcs3 = 3, "Expected 3 functions with policy AllUnpack")

      val _ = print "Test 27 passed\n"
   in () end

   (* Test 32: transform with preFlattenResolvePolicy *)
   val _ = let
      val _ = print "Test 32: transform with preFlattenResolvePolicy\n"
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])

      (* g(arg1: tuple) = #0 arg1 *)
      val gName = Func.fromString "g32"
      val gArg1 = Var.fromString "gArg1"
      val gL = Label.fromString "Lg"
      val gS = Statement.T {exp = Exp.Select {offset = 0, tuple = gArg1},
                            ty = tBool, var = SOME (Var.fromString "gTmp")}
      val gFunction = Function.new {
         args = Vector.fromList [(gArg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = gL,
            statements = Vector.fromList [gS],
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = gName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = gL
      }

      (* f(arg1: tuple) = g(arg1) *)
      val fName = Func.fromString "f32"
      val fArg1 = Var.fromString "fArg1"
      val fL = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(fArg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = fL,
            statements = Vector.new0 (),
            transfer = Transfer.Call {
               args = Vector.fromList [fArg1],
               func = gName,
               return = Return.Tail
            }
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fL
      }

      (* main() = f((true, true)) *)
      val mainName = Func.fromString "main32"
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
         functions = [gFunction, fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }

      val _ = Control.preFlattenMaxIters := 1
      val _ = Control.preFlattenConsumerPolicy := Control.PreFlattenConsumerPolicy.AnyUnpack

      val _ = print "Case 1: preFlattenResolvePolicy = Local (should NOT flatten f)\n"
      val _ = Control.preFlattenResolvePolicy := Control.PreFlattenResolvePolicy.Local
      val pLocal = PreFlatten.transform p
      val Program.T {functions = funcsLocal, ...} = pLocal
      val fIsFlattenedLocal = List.exists (funcsLocal, fn f => 
          let val name = Func.toString (Function.name f) in
             String.hasPrefix (name, {prefix = "f32_flat"})
          end)
      val _ = assert (not fIsFlattenedLocal, "f should NOT be flattened with Local")

      val _ = print "Case 2: preFlattenResolvePolicy = Global (SHOULD flatten f)\n"
      val _ = Control.preFlattenResolvePolicy := Control.PreFlattenResolvePolicy.Global
      val pGlobal = PreFlatten.transform p
      val Program.T {functions = funcsGlobal, ...} = pGlobal
      val fIsFlattenedGlobal = List.exists (funcsGlobal, fn f => 
          let val name = Func.toString (Function.name f) in
             String.hasPrefix (name, {prefix = "f32_flat"})
          end)
      val _ = assert (fIsFlattenedGlobal, "f SHOULD be flattened with Global")

      val _ = print "Test 32 passed\n"
   in () end

   (* Test 28: resolveAliases *)
   val _ = let
      val _ = print "Test 28: resolveAliases\n"
      val v1 = Var.fromString "v1"
      val v2 = Var.fromString "v2"
      val v3 = Var.fromString "v3"
      
      val assignments = [
         (v1, [PreFlatten.AsUnpacked, PreFlatten.AsAlias v2]),
         (v2, [PreFlatten.AsCurrent, PreFlatten.AsAlias v3]),
         (v3, [PreFlatten.AsCurrent])
      ]
      val vm = PreFlatten.newVarConsumerManagerFromAssignments assignments
      
      fun hasUnpacked l = List.exists (l, fn PreFlatten.AsUnpacked => true | _ => false)
      fun hasCurrent l = List.exists (l, fn PreFlatten.AsCurrent => true | _ => false)
      fun hasAlias l = List.exists (l, fn PreFlatten.AsAlias _ => true | _ => false)
      fun countCurrent l = List.length (List.keepAll (l, fn PreFlatten.AsCurrent => true | _ => false))

      val _ = print "Test 28a: DropAlias\n"
      val res1 = PreFlatten.resolveAliases (PreFlatten.DropAlias, vm) (PreFlatten.getVarConsumers (vm, v1))
      val _ = assert (hasUnpacked res1, "res1 should have AsUnpacked")
      val _ = assert (not (hasAlias res1), "res1 should not have AsAlias")
      val _ = assert (not (hasCurrent res1), "res1 should not have AsCurrent")

      val _ = print "Test 28b: UnionAlias (1-deep)\n"
      val res2 = PreFlatten.resolveAliases (PreFlatten.UnionAlias, vm) (PreFlatten.getVarConsumers (vm, v3))
      val _ = assert (hasCurrent res2, "res2 should have AsCurrent")
      val _ = assert (not (hasAlias res2), "res2 should not have AsAlias")

      val _ = print "Test 28c: UnionAlias (2-deep chain)\n"
      val res3 = PreFlatten.resolveAliases (PreFlatten.UnionAlias, vm) (PreFlatten.getVarConsumers (vm, v1))
      (* v1 -> {AsUnpacked, AsAlias v2}
         v2 -> {AsCurrent, AsAlias v3}
         v3 -> {AsCurrent}
         Result should be {AsUnpacked, AsCurrent, AsCurrent}
       *)
      val _ = assert (hasUnpacked res3, "res3 should have AsUnpacked")
      val _ = assert (countCurrent res3 = 2, "res3 should have 2 AsCurrent")
      val _ = assert (not (hasAlias res3), "res3 should not have AsAlias")

      val _ = print "Test 28d: UnionAlias (cycle)\n"
      val vCycle = Var.fromString "vCycle"
      val assignmentsCycle = [
         (vCycle, [PreFlatten.AsCurrent, PreFlatten.AsAlias vCycle])
      ]
      val vmCycle = PreFlatten.newVarConsumerManagerFromAssignments assignmentsCycle
      val resCycle = PreFlatten.resolveAliases (PreFlatten.UnionAlias, vmCycle) (PreFlatten.getVarConsumers (vmCycle, vCycle))
      val _ = assert (hasCurrent resCycle, "resCycle should have AsCurrent")
      val _ = assert (not (hasAlias resCycle), "resCycle should not have AsAlias")

      val _ = PreFlatten.destroyVarConsumerManager vm
      val _ = PreFlatten.destroyVarConsumerManager vmCycle
      val _ = print "Test 28 passed\n"
   in () end

   (* Test 29: UnionAlias in flattenOnce *)
   val _ = let
      val _ = print "Test 29: UnionAlias in flattenOnce\n"
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])

      (* g(arg1: tuple) = #0 arg1 *)
      val gName = Func.fromString "g29"
      val gArg1 = Var.fromString "gArg1"
      val gL = Label.fromString "Lg"
      val gS = Statement.T {exp = Exp.Select {offset = 0, tuple = gArg1},
                            ty = tBool, var = SOME (Var.fromString "gTmp")}
      val gFunction = Function.new {
         args = Vector.fromList [(gArg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = gL,
            statements = Vector.fromList [gS],
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = gName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = gL
      }

      (* f(arg1: tuple) = g(arg1) *)
      val fName = Func.fromString "f29"
      val fArg1 = Var.fromString "fArg1"
      val fL = Label.fromString "Lf"
      val fFunction = Function.new {
         args = Vector.fromList [(fArg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = fL,
            statements = Vector.new0 (),
            transfer = Transfer.Call {
               args = Vector.fromList [fArg1],
               func = gName,
               return = Return.Tail
            }
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fL
      }

      (* main() = f((true, true)) *)
      val mainName = Func.fromString "main29"
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
         functions = [gFunction, fFunction, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }

      val _ = print "Test 29a: DropAlias (should NOT flatten f)\n"
      val pDrop = (case PreFlatten.flattenOnce (PreFlatten.FlattenForAnyUnpack, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                      SOME p' => p'
                    | NONE => p)
      val Program.T {functions = funcsDrop, ...} = pDrop
      (* Should have 3 functions (g, f, main) + maybe g_flat if g was reachable and flattened.
         Wait, f calls g. If we flatten g, f's call to g changes.
         Actually, g unpacks its arg, so g will ALWAYS be flattened if its arg is a tuple from a tuple exp.
         But f's arg in main is a tuple from a tuple exp.
         If resolvePolicy = DropAlias, f's arg has only AsAlias(gArg1). So f won't be flattened.
       *)
      val fIsFlattenedDrop = List.exists (funcsDrop, fn f => 
          let val name = Func.toString (Function.name f) in
             String.hasPrefix (name, {prefix = "f29_flat"})
          end)
      val _ = assert (not fIsFlattenedDrop, "f should NOT be flattened with DropAlias")

      val _ = print "Test 29a passed\n"

      val _ = print "Test 29b: UnionAlias (SHOULD flatten f)\n"
      val pUnion = (case PreFlatten.flattenOnce (PreFlatten.FlattenForAnyUnpack, PreFlatten.UnionAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                       SOME p' => p'
                     | NONE => printFail "Test 29b: expected SOME, got NONE")
      val Program.T {functions = funcsUnion, ...} = pUnion
      val fIsFlattenedUnion = List.exists (funcsUnion, fn f => 
          let val name = Func.toString (Function.name f) in
             String.hasPrefix (name, {prefix = "f29_flat"})
          end)
      val _ = assert (fIsFlattenedUnion, "f SHOULD be flattened with UnionAlias")

      val _ = print "Test 29 passed\n"
   in () end

   (* Test 30: updateChoiceForPolicy with FlattenForAllUnpack *)
   val _ = let
      val _ = print "Test 30: updateChoiceForPolicy with FlattenForAllUnpack\n"
      val xs = Vector.fromList [Var.fromString "x1", Var.fromString "x2"]
      
      val _ = print "Test 30a: FlattenForAllUnpack flattens if all are AsUnpacked\n"
      val res1 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAllUnpack 
                   (PreFlatten.FlattenTupleVar xs, [PreFlatten.AsUnpacked, PreFlatten.AsUnpacked])
      val _ = case res1 of PreFlatten.FlattenTupleVar _ => () | _ => printFail "30a failed"

      val _ = print "Test 30a2: FlattenForAllUnpack flattens for empty users\n"
      val res1 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAllUnpack 
                   (PreFlatten.FlattenTupleVar xs, [])
      val _ = case res1 of PreFlatten.FlattenTupleVar _ => () | _ => printFail "30a failed"

      val _ = print "Test 30b: FlattenForAllUnpack flattens if empty\n"
      val res2 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAllUnpack 
                   (PreFlatten.FlattenTupleVar xs, [])
      val _ = case res2 of PreFlatten.FlattenTupleVar _ => () | _ => printFail "30b failed"

      val _ = print "Test 30c: FlattenForAllUnpack preserves if there is AsCurrent\n"
      val res3 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAllUnpack 
                   (PreFlatten.FlattenTupleVar xs, [PreFlatten.AsUnpacked, PreFlatten.AsCurrent])
      val _ = case res3 of PreFlatten.PreserveVar => () | _ => printFail "30c failed"

      val _ = print "Test 30d: FlattenForAllUnpack preserves if only AsCurrent\n"
      val res4 = PreFlatten.updateChoiceForPolicy PreFlatten.FlattenForAllUnpack 
                   (PreFlatten.FlattenTupleVar xs, [PreFlatten.AsCurrent])
      val _ = case res4 of PreFlatten.PreserveVar => () | _ => printFail "30d failed"

      val _ = print "Test 30 passed\n"
   in () end

   (* Test 31: flattenOnce with FlattenForAllUnpack *)
   val _ = let
      val _ = print "Test 31: flattenOnce with FlattenForAllUnpack\n"
      val tBool = Type.bool
      val tTuple = Type.tuple (Vector.fromList [tBool, tBool])

      (* f(arg1: tuple) = #0 arg1 *)
      val fName = Func.fromString "f31"
      val fArg1 = Var.fromString "fArg1"
      val fL = Label.fromString "Lf"
      val fS = Statement.T {exp = Exp.Select {offset = 0, tuple = fArg1},
                            ty = tBool, var = SOME (Var.fromString "fTmp")}
      val fFunction = Function.new {
         args = Vector.fromList [(fArg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = fL,
            statements = Vector.fromList [fS],
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fL
      }

      (* main() = f((true, true)) *)
      val mainName = Func.fromString "main31"
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

      (* Case 31a: Only AsUnpacked consumer in f. Should flatten. *)
      val _ = print "Test 31a: Only AsUnpacked consumer (should flatten)\n"
      val pFlat = (case PreFlatten.flattenOnce (PreFlatten.FlattenForAllUnpack, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.functionOnly, PreFlatten.noRecursiveFlatten) p of
                      SOME p' => p'
                    | NONE => printFail "Test 31a: expected SOME, got NONE")
      val Program.T {functions = funcsFlat, ...} = pFlat
      val fIsFlattened = List.exists (funcsFlat, fn f => 
          let val name = Func.toString (Function.name f) in
             String.hasPrefix (name, {prefix = "f31_flat"})
          end)
      val _ = assert (fIsFlattened, "f SHOULD be flattened in 31a")

      (* Case 31b: AsCurrent consumer in f. Should NOT flatten. *)
      val _ = print "Test 31b: AsCurrent consumer (should NOT flatten)\n"
      (* Modify f to have an AsCurrent consumer *)
      val fS2 = Statement.T {exp = Exp.Tuple (Vector.fromList [fArg1]),
                             ty = tTuple, var = SOME (Var.fromString "fTmp2")}
      val fFunction2 = Function.new {
         args = Vector.fromList [(fArg1, tTuple)],
         blocks = Vector.fromList [Block.T {
            args = Vector.new0 (),
            label = fL,
            statements = Vector.fromList [fS, fS2],
            transfer = Transfer.Return (Vector.new0 ())
         }],
         mayInline = true,
         name = fName,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = fL
      }
      val p2 = Program.T {
         datatypes = Vector.new0 (),
         functions = [fFunction2, mainFunction],
         globals = Vector.new0 (),
         main = mainName
      }
      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenForAllUnpack,
                                           PreFlatten.UnionAlias,
                                           PreFlatten.FlattenAnyType,
                                           PreFlatten.functionOnly,
                                           PreFlatten.noRecursiveFlatten) p2 of
                 SOME _ => printFail "Test 31b: expected NONE, got SOME"
               | NONE => ()

      val _ = print "Test 31 passed\n"
   in () end

   (* Test 33: chooseVarsInStatement with FlattenConVar *)
   val _ = let
      val _ = print "Test 33: chooseVarsInStatement with FlattenConVar\n"
      val vcm = PreFlatten.newVarChoiceManager ()

      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val s1 = Statement.T {exp = Exp.unit, ty = t1, var = SOME v1}
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val s2 = Statement.T {exp = Exp.unit, ty = t2, var = SOME v2}
      val _ = PreFlatten.markTypeForBinding (vcm, s1)
      val _ = PreFlatten.markTypeForBinding (vcm, s2)
      val vCon = Var.fromString "vc"
      val con = Con.fromString "C"

      val sCon = Statement.T {
         exp = Exp.ConApp {args = Vector.fromList [v1, v2], con = con},
         ty = Type.unit,
         var = SOME vCon
      }

      val _ = PreFlatten.chooseVarsInStatement (vcm, s1)
      val _ = PreFlatten.chooseVarsInStatement (vcm, s2)
      val _ = PreFlatten.chooseVarsInStatement (vcm, sCon)

      val choiceCon = PreFlatten.getVarChoice (vcm, vCon)
      val _ =
         case choiceCon of
            PreFlatten.FlattenConVar {args, con = con'} =>
               let
                  val _ = assert (Vector.length args = 2, "FlattenConVar should have 2 vars")
                  val (v1', t1') = Vector.sub (args, 0)
                  val (v2', t2') = Vector.sub (args, 1)
                  val _ = assert (Var.equals (v1', v1), "FlattenConVar var 0 mismatch")
                  val _ = assert (Type.equals (t1', t1), "FlattenConVar type 0 mismatch")
                  val _ = assert (Var.equals (v2', v2), "FlattenConVar var 1 mismatch")
                  val _ = assert (Type.equals (t2', t2), "FlattenConVar type 1 mismatch")
                  val _ = assert (Con.equals (con, con'), "FlattenConVar con mismatch")
               in () end
          | _ => (print "vc should be FlattenConVar\n"; OS.Process.exit OS.Process.failure)

      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 33 passed\n"
   in () end

   (* Test 34: newVarChoicesForProgram with FlattenConVar *)
   val _ = let
      val _ = print "Test 34: newVarChoicesForProgram with FlattenConVar\n"

      val v1 = Var.fromString "v1"
      val t1 = Type.bool
      val s1 = Statement.T {exp = Exp.unit, ty = t1, var = SOME v1}
      val v2 = Var.fromString "v2"
      val t2 = Type.unit
      val s2 = Statement.T {exp = Exp.unit, ty = t2, var = SOME v2}
      val vCon = Var.fromString "vc"
      val con = Con.fromString "C"

      val sCon = Statement.T {
         exp = Exp.ConApp {args = Vector.fromList [v1, v2], con = con},
         ty = Type.unit,
         var = SOME vCon
      }

      val mainFunc = Func.fromString "main34"
      val mainLabel = Label.fromString "L34"
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = mainLabel,
         statements = Vector.fromList [s1, s2, sCon],
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

      val vcm = PreFlatten.newVarChoicesForProgram p
      val choiceCon = PreFlatten.getVarChoice (vcm, vCon)
      val _ =
         case choiceCon of
            PreFlatten.FlattenConVar {args, ...} =>
               let
                  val _ = assert (Vector.length args = 2, "FlattenConVar should have 2 vars")
                  val (v1', t1') = Vector.sub (args, 0)
                  val (v2', t2') = Vector.sub (args, 1)
                  val _ = assert (Var.equals (v1', v1), "v1 mismatch")
                  val _ = assert (Type.equals (t1', t1), "t1 mismatch")
                  val _ = assert (Var.equals (v2', v2), "v2 mismatch")
                  val _ = assert (Type.equals (t2', t2), "t2 mismatch")
               in () end
          | _ => (print "vc should be FlattenConVar\n"; OS.Process.exit OS.Process.failure)

      val _ = PreFlatten.destroyVarChoiceManager vcm
      val _ = print "Test 34 passed\n"
   in () end

   (* Test 35: buildFlattenedFunctionWithDatatype (2-args) *)
   val _ = let
      val _ = print "Test 35: buildFlattenedFunctionWithDatatype (2-args)\n"
      val fName = Func.fromString "f35"
      val l1 = Label.fromString "L35"
      val v1 = Var.fromString "v1"
      val tCon = Type.datatypee (Tycon.fromString "T2")
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

      val con = Con.fromString "C2"
      val argTys = Vector.fromList [Type.bool, Type.unit]
      val choice = PreFlatten.FlattenCon {argTys = argTys, con = con}

      val _ = print "Checking checkFlatteningChoice for datatype (2-args)\n"
      val choiceRes = PreFlatten.checkFlatteningChoice (f, Vector.fromList [choice])
      val _ = assert (choiceRes = PreFlatten.Valid, "Expected Valid for datatype flattening (2-args), got " ^ (choiceResToString choiceRes))

      val _ = print "Attempting buildFlattenedFunction for datatype (2-args)\n"
      val res = PreFlatten.buildFlattenedFunction (f, Vector.fromList [choice])
      val _ = print "Test 35 passed\n"
   in () end

   (* Test 36: buildFlattenedFunctionWithDatatype (nullary) *)
end
