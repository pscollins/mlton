local
   open Ssa
in
   val _ = let
      val _ = print "Test 56: blockManager destroy error if pending\n"
      val fName = Func.fromString "f56"
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
      val _ = PreFlatten.getOrCreateBlock (bm, l1, Vector.fromList [PreFlatten.Preserve, PreFlatten.FlattenTuple])
      
      val caught = ref false
      val _ = (PreFlatten.destroyBlockManager bm) handle _ => caught := true
      val _ = assert (!caught, "destroyBlockManager should fail if pending blocks exist")
      
      (* Clean up properly for real this time *)
      val _ = PreFlatten.extractNewBlocks bm
      val _ = PreFlatten.destroyBlockManager bm
      
      val _ = print "Test 56 passed\n"
   in () end
end
