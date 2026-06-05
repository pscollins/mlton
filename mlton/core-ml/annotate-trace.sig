(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ANNOTATE_TRACE_STRUCTS =
   sig
      structure CoreML: CORE_ML
   end

signature ANNOTATE_TRACE =
   sig
      include ANNOTATE_TRACE_STRUCTS
      (* Finds any instances of a pattern like....

       Trace_sourceMark ("mark1")
       Trace_sourceMark ("mark2")

      (i.e. an `Exp.PrimApp` that contains a `Trace_sourceMark` applied to an
      `Exp.Const` that resolves to a static string)

      ...and replaces the "dynamic" unary `Trace_sourceMark` with a
      `Trace_staticSourceMark (constName)`, i.e. a "static," nullary
      `Trace_staticSourceMark` whose argument is the `Const` name, i.e.:

      (Trace_staticSourceMark "mark1" ())
      (Trace_staticSourceMark "mark2" ())
      *)
      val annotateTrace:
         {prog: CoreML.Dec.t list vector} ->
         {prog: CoreML.Dec.t list vector}
   end
