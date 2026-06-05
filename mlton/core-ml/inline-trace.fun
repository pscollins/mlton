(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor InlineTrace (S: INLINE_TRACE_STRUCTS): INLINE_TRACE =
struct

open S
open CoreML
structure CoreMLUtil = CoreMLUtil (structure CoreML = CoreML)
open CoreMLUtil

fun inlineTrace {prog} =
   let
      fun isTargetLambda {body, ...} =
          case Exp.node body of
              Exp.PrimApp {prim, ...} => Prim.equals (prim, Prim.Trace_sourceMark)
            | _ => false
      fun isTargetExp (exp: Exp.t) =
          case Exp.node exp of
              Exp.Lambda lambda => isTargetLambda (Lambda.dest lambda)
           |  _ => false

      (* Find the LHSes of the `Val` bindings to `Lambda`s that wrap a single
      `Trace_sourceMark`. There may be bindings that we miss, but that's OK:
      this is just best-effort info for debugging. *)
      val targetVars = collectVarsBoundToPred (prog, isTargetExp)
   in
      (* Inline the `Trace_sourceMark` application to all of the uses of the
      `Var`s that we found above *)
      {prog = mapExps (prog, inlineSourceMarkCall targetVars)}
   end
end
