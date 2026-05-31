local
   open Ssa
in
   (* Test 47: blockOnly flattening - No-op *)
   val _ = let
      val _ = print "Test 47: blockOnly flattening (No-op)\n"
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

      val _ = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.blockOnly, PreFlatten.noRecursiveFlatten) p of
                 SOME _ => printFail "Test 47: expected NONE (No-Op), got SOME"
               | NONE => print "Test 47: OK (got NONE)\n"
   in () end

   (* Test 48: blockOnly flattening - Tuple *)
   val _ = let
      val _ = print "Test 48: blockOnly flattening (Tuple)\n"
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val L1 = Label.fromString "L1"
      val a = Var.newNoname ()
      val b = Var.newNoname ()
      val v0 = Var.newNoname ()
      val x = Var.newNoname ()
      
      val boolTy = Type.bool
      val tupleTy = Type.tuple (Vector.fromList [boolTy, boolTy])
      
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = L0,
         statements = Vector.fromList [
            Statement.T {exp = Exp.Tuple (Vector.fromList [a, b]), ty = tupleTy, var = SOME v0}
         ],
         transfer = Transfer.Goto {args = Vector.fromList [v0], dst = L1}
      }
      val L1Block = Block.T {
         args = Vector.fromList [(x, tupleTy)],
         label = L1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      
      val mainFunction = Function.new {
         args = Vector.fromList [(a, boolTy), (b, boolTy)],
         blocks = Vector.fromList [mainBlock, L1Block],
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

      val p' = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.blockOnly, PreFlatten.noRecursiveFlatten) p of
                  SOME p' => p'
                | NONE => printFail "Test 48: expected SOME, got NONE"
      
      val mainFunction' = Program.mainFunction p'
      val blocks = Function.blocks mainFunction'
      
      val _ = assert (Vector.length blocks = 3, "Test 48: expected 3 blocks (L0, L1, L1_flat)")
      
      (* Verify L0 jumps to a new label *)
      val L0' = Vector.sub (blocks, 0)
      val transfer = Block.transfer L0'
      val _ = case transfer of
                 Transfer.Goto {args, dst} => 
                    if Label.equals (dst, L1) then printFail "Test 48: L0 still jumps to L1"
                    else (assert (Vector.length args = 2, "Test 48: L0 jump should have 2 args");
                          print "Test 48: L0 jumps to new label with 2 args\n")
               | _ => printFail "Test 48: L0 does not have a Goto transfer"
      
      (* Verify the new block has 2 args *)
      val flattenedBlock = Vector.sub (blocks, 2)
      val _ = assert (Vector.length (Block.args flattenedBlock) = 2, "Test 48: flattened block should have 2 args")
      val _ = print "Test 48: OK\n"
   in () end

   (* Test 49: blockOnly flattening - ConApp *)
   val _ = let
      val _ = print "Test 49: blockOnly flattening (ConApp)\n"
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val L1 = Label.fromString "L1"
      val a = Var.newNoname ()
      val b = Var.newNoname ()
      val v0 = Var.newNoname ()
      val x = Var.newNoname ()
      
      val boolTy = Type.bool
      val tycon = Tycon.newNoname ()
      val con = Con.newNoname ()
      val datatype1 = Datatype.T {
         cons = Vector.fromList [{args = Vector.fromList [boolTy, boolTy], con = con}],
         tycon = tycon
      }
      val ty = Type.datatypee tycon
      
      val mainBlock = Block.T {
         args = Vector.new0 (),
         label = L0,
         statements = Vector.fromList [
            Statement.T {exp = Exp.ConApp {args = Vector.fromList [a, b], con = con}, ty = ty, var = SOME v0}
         ],
         transfer = Transfer.Goto {args = Vector.fromList [v0], dst = L1}
      }
      val L1Block = Block.T {
         args = Vector.fromList [(x, ty)],
         label = L1,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      
      val mainFunction = Function.new {
         args = Vector.fromList [(a, boolTy), (b, boolTy)],
         blocks = Vector.fromList [mainBlock, L1Block],
         mayInline = true,
         name = mainFunc,
         raises = NONE,
         returns = SOME (Vector.new0 ()),
         start = L0
      }
      val p = Program.T {
         datatypes = Vector.fromList [datatype1],
         functions = [mainFunction],
         globals = Vector.new0 (),
         main = mainFunc
      }

      val p' = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.blockOnly, PreFlatten.noRecursiveFlatten) p of
                  SOME p' => p'
                | NONE => printFail "Test 49: expected SOME, got NONE"
      
      val mainFunction' = Program.mainFunction p'
      val blocks = Function.blocks mainFunction'
      
      val _ = assert (Vector.length blocks = 3, "Test 49: expected 3 blocks (L0, L1, L1_flat)")
      
      (* Verify L0 jumps to a new label *)
      val L0' = Vector.sub (blocks, 0)
      val transfer = Block.transfer L0'
      val _ = case transfer of
                 Transfer.Goto {args, dst} => 
                    if Label.equals (dst, L1) then printFail "Test 49: L0 still jumps to L1"
                    else (assert (Vector.length args = 2, "Test 49: L0 jump should have 2 args");
                          print "Test 49: L0 jumps to new label with 2 args\n")
               | _ => printFail "Test 49: L0 does not have a Goto transfer"

      val _ = print "Test 49: OK\n"
   in () end

   (* Test 50: blockOnly flattening - Mixed callsites *)
   val _ = let
      val _ = print "Test 50: blockOnly flattening (Mixed callsites)\n"
      val mainFunc = Func.fromString "main"
      val L0 = Label.fromString "L0"
      val L1 = Label.fromString "L1"
      val L2 = Label.fromString "L2"
      val a = Var.newNoname ()
      val b = Var.newNoname ()
      val v0 = Var.newNoname ()
      val v1 = Var.newNoname ()
      val x = Var.newNoname ()
      
      val boolTy = Type.bool
      val tupleTy = Type.tuple (Vector.fromList [boolTy, boolTy])
      
      val L0Block = Block.T {
         args = Vector.new0 (),
         label = L0,
         statements = Vector.fromList [
            Statement.T {exp = Exp.Tuple (Vector.fromList [a, b]), ty = tupleTy, var = SOME v0}
         ],
         transfer = Transfer.Goto {args = Vector.fromList [v0], dst = L2}
      }
      val L1Block = Block.T {
         args = Vector.fromList [(v1, tupleTy)],
         label = L1,
         statements = Vector.new0 (),
         transfer = Transfer.Goto {args = Vector.fromList [v1], dst = L2}
      }
      val L2Block = Block.T {
         args = Vector.fromList [(x, tupleTy)],
         label = L2,
         statements = Vector.new0 (),
         transfer = Transfer.Return (Vector.new0 ())
      }
      
      val mainFunction = Function.new {
         args = Vector.fromList [(a, boolTy), (b, boolTy)],
         blocks = Vector.fromList [L0Block, L1Block, L2Block],
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

      val p' = case PreFlatten.flattenOnce (PreFlatten.FlattenAlways, PreFlatten.DropAlias, PreFlatten.FlattenAnyType, PreFlatten.blockOnly, PreFlatten.noRecursiveFlatten) p of
                  SOME p' => p'
                | NONE => printFail "Test 50: expected SOME, got NONE"
      
      val mainFunction' = Program.mainFunction p'
      val blocks = Function.blocks mainFunction'
      
      val _ = assert (Vector.length blocks = 4, "Test 50: expected 4 blocks (L0, L1, L2, L2_flat)")
      
      fun findBlock l = 
         let
            val n = Vector.length blocks
            fun loop i =
               if i = n then printFail ("Test 50: could not find block " ^ Label.toString l)
               else
                  let val b = Vector.sub (blocks, i)
                  in if Label.equals (Block.label b, l) then b
                     else loop (i + 1)
                  end
         in
            loop 0
         end

      val L0' = findBlock L0
      val L1' = findBlock L1
      
      val _ = case Block.transfer L0' of
                 Transfer.Goto {args, dst} => 
                    if Label.equals (dst, L2) then printFail "Test 50: L0 still jumps to L2"
                    else assert (Vector.length args = 2, "Test 50: L0 jump should have 2 args")
               | _ => printFail "Test 50: L0 does not have a Goto transfer"

      val _ = case Block.transfer L1' of
                 Transfer.Goto {args, dst} => 
                    if Label.equals (dst, L2) then ()
                    else printFail "Test 50: L1 does not jump to L2 anymore"
               | _ => printFail "Test 50: L1 does not have a Goto transfer"

      val _ = print "Test 50: OK\n"
   in () end
end
