signature EMIT_DIAGNOSTICS_STRUCTS =
   sig
      structure Machine: MACHINE
   end

signature EMIT_DIAGNOSTICS =
   sig
      include EMIT_DIAGNOSTICS_STRUCTS

      (* Exposed for testing: given a `Machine.Program.t` and a partial
      transformation on `Machine.Statement.t`s, applies the transformation to
      each `Statement.t` in the `Program.t` and, for any applications that return
      SOME, replaces the existing `Statement.t`. *)
      val mapStatements: (Machine.Program.t *
                          (Machine.Statement.t -> Machine.Statement.t option)) ->
                         Machine.Program.t

      (* Finds instances `Trace_staticSourceMark` `Statement.PrimApp`'s like:

      1. Trace_staticSourceMark:mark1 ()
      2. Trace_staticSourceMarkValue:markX (0x1:w32)

      and replaces them with `Statement.Diagnostic`s like:

      1. Diagnostic "Trace_staticSourceMark:mark1"
      2. Diagnostic "Trace_staticSourceMarkValue:markX (0x1:w32)"

      i.e. a `Statement.Diagnostic` whose `string` payload is as described
      above.
       *)
      val emitDiagnostics: Machine.Program.t -> Machine.Program.t
   end
