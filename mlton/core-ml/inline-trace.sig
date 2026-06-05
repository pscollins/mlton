(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INLINE_TRACE_STRUCTS =
   sig
      structure CoreML: CORE_ML
   end

signature INLINE_TRACE =
   sig
      include INLINE_TRACE_STRUCTS

      (* Finds any instances of a pattern like...

           val sourceMark_0: string -> unit =
                 (fn x_1047: string =>
                  Trace_sourceMark (x_1047))
....
           val _ =
              let val kConst_0: int32 = 0x75BCD15:w32
                  val m1_0: unit = (sourceMark_0 "mark1")
                  val kConst2_0: int32 = (+_6 (kConst_0, 0x1:w32))
                  val m2_0: unit = (sourceMark_0 "mark2")
              in
                 (print_1 (toString_4 kConst2_0))
              end

        (i.e. a `Dec.Val` binding corresponding to a singleton `Var` bound to
        `Lambda` wrapping a `Trace_sourceMark` call, followed by one or more
        usages of the `Var`)

        ... and replaces the usages of the `Var` with direct `PrimApp`
        invocations, i.e.:

           val _ =
              let val kConst_0: int32 = 0x75BCD15:w32
                  val m1_0: unit = (Trace_sourceMark ("mark1"))
                  val kConst2_0: int32 = (+_6 (kConst_0, 0x1:w32))
                  val m2_0: unit = (Trace_sourceMark ("mark2"))
              in
                 (print_1 (toString_4 kConst2_0))
              end

       leaving the original `Val` binding intact.
       *)

      val inlineTrace:
         {prog: CoreML.Dec.t list vector} ->
         {prog: CoreML.Dec.t list vector}
   end
