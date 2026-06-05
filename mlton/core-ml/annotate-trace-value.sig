(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ANNOTATE_TRACE_VALUE_STRUCTS =
   sig
      structure CoreML: CORE_ML
   end

signature ANNOTATE_TRACE_VALUE =
   sig
      include ANNOTATE_TRACE_VALUE_STRUCTS

      (* Converts instances of `Trace_sourceMarkValue` into instances of
      `Trace_staticSourceMarkValue`. Unlike `InlineTrace` + `AnnotateTrace`
      (which do the corresponding `Trace_sourceMark -> Trace_staticSourceMark`
      transformation of in two steps, first inline and then convert), this pass
      is a single step conversion, i.e. given IR like:

     ```
      val 'a_51 sourceMarkValue_0: 'a_51 * string -> unit =
       (fn x_1048: 'a_51 * string =>
        case x_1048 of
          (x_1050: 'a_51, x_1049: string) =>
          Trace_sourceMarkValue['a_51] (x_1050, x_1049))
      ...
      val 'a_3341 sourceMarkValue_1: 'a_3341 * string -> unit =
        sourceMarkValue_0 ('a_3341)
      ...
      val 'a_3613 sourceMarkValue_2: 'a_3613 * string -> unit =
         sourceMarkValue_1 ('a_3613)
      ...
      val 'a_3999 sourceMarkValue_3: 'a_3999 * string -> unit =
         sourceMarkValue_2 ('a_3999)
      ...
      val _ =
         let val kX_0: int32 = 0x1:w32
             val kY_0: int32 = 0x2:w32
             val _ = (sourceMarkValue_3 (int32) (kX_0, "markX"))
             val _ = (sourceMarkValue_3 (int32) (kY_0, "markY"))
             val z_26: int32 = (+_6 (kX_0, kY_0))
         in
            (print_1 (toString_4 z_26))
         end
      ```

      This pass rewrites the final `val _ = ...` statement as:
      ```
      val _ =
         let val kX_0: int32 = 0x1:w32
             val kY_0: int32 = 0x2:w32
             val _ = (Trace_staticSourceMark:markX[int32] kX_0)
             val _ = (Trace_staticSourceMark:markY[int32] kY_0)
             val z_26: int32 = (+_6 (kX_0, kY_0))
         in
            (print_1 (toString_4 z_26))
         end
      ```

      i.e. it replaces any calls that correspond to a single invocation of
      `Trace_sourceMarkValue` with a direct `Trace_staticSourceMarkValue`
      `PrimApp` whose payload is the tag associated with the source mark and
      whose argument matches the original non-tag argument.

      (We implement this as one pass rather than two because inlining/matching
      on the wrapper `Exp.Case` expression that handles the tuple would be complicated)
       *)
      val annotateTraceValue:
         {prog: CoreML.Dec.t list vector} ->
         {prog: CoreML.Dec.t list vector}
   end
