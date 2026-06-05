functor EmitDiagnostics (S: EMIT_DIAGNOSTICS_STRUCTS): EMIT_DIAGNOSTICS =
struct

open S
open Machine

fun mapStatements (program: Program.t, rewrite: Statement.t -> Statement.t option):
    Program.t = let
   fun doStatement (s: Statement.t): Statement.t =
       case rewrite s of
          SOME s'  => s'
        | NONE => s
   fun doBlock (Block.T {kind, label, live, raises, returns, statements,
                         transfer}): Block.t =
       Block.T {kind = kind, label = label, live = live,
                raises = raises, returns = returns,
                statements = Vector.map (statements, doStatement),
                transfer = transfer}
   fun doChunk (Chunk.T {blocks, chunkLabel, tempsMax}): Chunk.t =
       Chunk.T {blocks = Vector.map (blocks, doBlock),
                chunkLabel = chunkLabel,
                tempsMax = tempsMax}
   val Program.T {chunks, frameInfos, frameOffsets, globals,
                  handlesSignals, main, maxFrameSize, objectTypes,
                  sourceMaps, staticHeaps} = program
in
   Program.T {chunks = List.map (chunks, doChunk),
              frameInfos = frameInfos,
              frameOffsets = frameOffsets,
              globals = globals,
              handlesSignals = handlesSignals,
              main = main,
              maxFrameSize = maxFrameSize,
              objectTypes = objectTypes,
              sourceMaps = sourceMaps,
              staticHeaps = staticHeaps}
end

fun emitDiagnostics (program: Program.t): Program.t = let
   fun toString (s: Statement.t) =
       Layout.toString (Statement.layout s)
   fun wantPrim prim =
       case prim of
          Prim.Trace_staticSourceMark _ =>  true
        |  Prim.Trace_staticSourceMarkValue _ =>  true
        | _ => false
   fun maybeDiagnostic (prim, s) =
       if wantPrim prim then
          SOME (Statement.Diagnostic (toString s))
       else NONE
   fun rewrite (s: Statement.t): Statement.t option =
       case s of
           Statement.PrimApp {prim, ...} => maybeDiagnostic (prim, s)
        | _ => NONE
in
   mapStatements (program, rewrite)
end

end
