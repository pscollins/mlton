structure Atoms = Atoms ()
structure BackendAtoms = BackendAtoms (open Atoms)
structure Machine = Machine (open BackendAtoms)
structure EmitDiagnostics = EmitDiagnostics (structure Machine = Machine)

local
   open Machine

   fun allDiagnostics (Program.T {chunks, ...}) =
      List.fold (chunks, [], fn (Chunk.T {blocks, ...}, acc) =>
         Vector.fold (blocks, acc, fn (Block.T {statements, ...}, acc) =>
            Vector.fold (statements, acc, fn (s, acc) =>
               case s of
                  Statement.Diagnostic m => m :: acc
                | _ => acc
            )
         )
      )

   fun hasDiagnostic (p, msg) =
      List.exists (allDiagnostics p, fn m => m = msg)

   fun assert (cond, msg) =
      if cond then () else (print ("Assertion failed: " ^ msg ^ "\n"); raise Fail msg)

   fun assertHasDiagnostic (p, msg, testMsg) =
      if hasDiagnostic (p, msg)
      then ()
      else
         let
            val ds = allDiagnostics p
            val _ = print ("Assertion failed: " ^ testMsg ^ "\n")
            val _ = print ("Diagnostics found:\n")
            val _ = List.foreach (ds, fn d => print ("  " ^ d ^ "\n"))
         in
            raise Fail testMsg
         end

   fun assertHasNoDiagnostic (p, msg, testMsg) =
      if not (hasDiagnostic (p, msg))
      then ()
      else
         let
            val ds = allDiagnostics p
            val _ = print ("Assertion failed: " ^ testMsg ^ "\n")
            val _ = print ("Diagnostics found:\n")
            val _ = List.foreach (ds, fn d => print ("  " ^ d ^ "\n"))
         in
            raise Fail testMsg
         end

   val _ = print "Running Statement.layout test...\n"
   val _ = assert (Layout.toString (Statement.layout (Statement.Diagnostic "foo")) = "Diagnostic(foo)",
                   "Statement.layout test failed")

   val label = Label.newNoname ()
   val chunkLabel = ChunkLabel.newNoname ()
   
   val b1 = Block.T {
       kind = Kind.Jump,
       label = label,
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [Statement.Diagnostic "original"],
       transfer = Transfer.Goto label
   }

   val c1 = Chunk.T {
       blocks = Vector.fromList [b1],
       chunkLabel = chunkLabel,
       tempsMax = fn _ => 0
   }

   val p1 = Program.T {
       chunks = [c1],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val _ = print "Running mapStatements test 1...\n"
   val p1' = EmitDiagnostics.mapStatements (p1, fn s =>
      case s of
         Statement.Diagnostic "original" => SOME (Statement.Diagnostic "transformed")
       | _ => NONE)

   val _ = assertHasDiagnostic (p1', "transformed", "Test 1: 'transformed' diagnostic not found")
   val _ = assertHasNoDiagnostic (p1', "original", "Test 1: 'original' diagnostic still found")

   (* Test 2: multiple statements in one block *)
   val b2 = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [Statement.Diagnostic "a", 
                                     Statement.Diagnostic "b",
                                     Statement.Diagnostic "c"],
       transfer = Transfer.Goto label
   }
   val c2 = Chunk.T {
       blocks = Vector.fromList [b2],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val p2 = Program.T {
       chunks = [c2],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val _ = print "Running mapStatements test 2...\n"
   val p2' = EmitDiagnostics.mapStatements (p2, fn s =>
      case s of
         Statement.Diagnostic "a" => SOME (Statement.Diagnostic "A")
       | Statement.Diagnostic "c" => SOME (Statement.Diagnostic "C")
       | _ => NONE)

   val _ = assertHasDiagnostic (p2', "A", "Test 2: 'A' not found")
   val _ = assertHasDiagnostic (p2', "b", "Test 2: 'b' not found")
   val _ = assertHasDiagnostic (p2', "C", "Test 2: 'C' not found")
   val _ = assertHasNoDiagnostic (p2', "a", "Test 2: 'a' still found")
   val _ = assertHasNoDiagnostic (p2', "c", "Test 2: 'c' still found")

   (* Test 3: multiple chunks and blocks *)
   val b3a = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [Statement.Diagnostic "3a"],
       transfer = Transfer.Goto label
   }
   val b3b = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [Statement.Diagnostic "3b"],
       transfer = Transfer.Goto label
   }
   val c3a = Chunk.T {
       blocks = Vector.fromList [b3a],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val c3b = Chunk.T {
       blocks = Vector.fromList [b3b],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val p3 = Program.T {
       chunks = [c3a, c3b],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val _ = print "Running mapStatements test 3...\n"
   val p3' = EmitDiagnostics.mapStatements (p3, fn s =>
      case s of
         Statement.Diagnostic "3a" => SOME (Statement.Diagnostic "3A")
       | Statement.Diagnostic "3b" => SOME (Statement.Diagnostic "3B")
       | _ => NONE)

   val _ = assertHasDiagnostic (p3', "3A", "Test 3: '3A' not found")
   val _ = assertHasDiagnostic (p3', "3B", "Test 3: '3B' not found")
   val _ = assertHasNoDiagnostic (p3', "3a", "Test 3: '3a' still found")
   val _ = assertHasNoDiagnostic (p3', "3b", "Test 3: '3b' still found")

   (* emitDiagnostics tests *)

   val _ = print "Running emitDiagnostics test 1...\n"
   val b4 = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [
         Statement.PrimApp {
            args = Vector.new0 (),
            dst = NONE,
            prim = Prim.Trace_staticSourceMark "mark1"
         }
       ],
       transfer = Transfer.Goto label
   }
   val c4 = Chunk.T {
       blocks = Vector.fromList [b4],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val p4 = Program.T {
       chunks = [c4],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val p4' = EmitDiagnostics.emitDiagnostics p4
   val _ = assertHasDiagnostic (p4', "Trace_staticSourceMark:mark1 ()", 
                                "EmitDiagnostics test 1: 'Trace_staticSourceMark:mark1' diagnostic not found")

   val _ = print "Running emitDiagnostics test 2...\n"
   (* Test mixed statements *)
   val b5 = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [
         Statement.Diagnostic "pre-existing",
         Statement.PrimApp {
            args = Vector.new0 (),
            dst = NONE,
            prim = Prim.Trace_staticSourceMark "mark2"
         },
         Statement.Move {dst = Operand.GCState, src = Operand.GCState} (* dummy move *)
       ],
       transfer = Transfer.Goto label
   }
   val c5 = Chunk.T {
       blocks = Vector.fromList [b5],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val p5 = Program.T {
       chunks = [c5],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val p5' = EmitDiagnostics.emitDiagnostics p5
   val _ = assertHasDiagnostic (p5', "pre-existing", "EmitDiagnostics test 2: 'pre-existing' lost")
   val _ = assertHasDiagnostic (p5', "Trace_staticSourceMark:mark2 ()", 
                                "EmitDiagnostics test 2: 'Trace_staticSourceMark:mark2' diagnostic not found")

   val _ = print "Running emitDiagnostics test 3...\n"
   val b6 = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [
         Statement.PrimApp {
            args = Vector.fromList [Operand.word (WordX.fromInt (1, WordSize.word32))],
            dst = NONE,
            prim = Prim.Trace_staticSourceMarkValue "markX"
         }
       ],
       transfer = Transfer.Goto label
   }
   val c6 = Chunk.T {
       blocks = Vector.fromList [b6],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val p6 = Program.T {
       chunks = [c6],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val p6' = EmitDiagnostics.emitDiagnostics p6
   val _ = assertHasDiagnostic (p6', "Trace_staticSourceMarkValue:markX (0x1:w32)", 
                                "EmitDiagnostics test 3: 'Trace_staticSourceMarkValue:markX (0x1:w32)' diagnostic not found")

   val _ = print "Running emitDiagnostics test 4...\n"
   val b7 = Block.T {
       kind = Kind.Jump,
       label = Label.newNoname (),
       live = Vector.new0 (),
       raises = NONE,
       returns = NONE,
       statements = Vector.fromList [
         Statement.PrimApp {
            args = Vector.fromList [Operand.word (WordX.fromInt (123, WordSize.word32))],
            dst = NONE,
            prim = Prim.Trace_staticSourceMarkValue "markY"
         }
       ],
       transfer = Transfer.Goto label
   }
   val c7 = Chunk.T {
       blocks = Vector.fromList [b7],
       chunkLabel = ChunkLabel.newNoname (),
       tempsMax = fn _ => 0
   }
   val p7 = Program.T {
       chunks = [c7],
       frameInfos = Vector.new0 (),
       frameOffsets = Vector.new0 (),
       globals = {objptrs = [], reals = []},
       handlesSignals = false,
       main = {chunkLabel = chunkLabel,
               label = label},
       maxFrameSize = Bytes.zero,
       objectTypes = Vector.new0 (),
       sourceMaps = NONE,
       staticHeaps = fn _ => Vector.new0 ()
   }

   val p7' = EmitDiagnostics.emitDiagnostics p7
   val _ = assertHasDiagnostic (p7', "Trace_staticSourceMarkValue:markY (0x7B:w32)", 
                                "EmitDiagnostics test 4: 'Trace_staticSourceMarkValue:markY (0x7B:w32)' diagnostic not found")

   val _ = print "EmitDiagnostics tests passed.\n"
in
end
