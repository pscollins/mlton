(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AnnotateTraceValue (S: ANNOTATE_TRACE_VALUE_STRUCTS): ANNOTATE_TRACE_VALUE =
struct

open S
open CoreML
structure CoreMLUtil = CoreMLUtil (structure CoreML = CoreML)
open CoreMLUtil

fun annotateTraceValue {prog} = let
   (* Find all of the `Var.t`s that correspond to a `Trace_sourceMarkValue`
   call, recursing through aliases. *)
   val targetVars = recursiveCollectVarsBoundToPred
                        (prog, isSourceMarkValueExp)
in
   (* Rewrite all invocations of matching `Var.t`s into our target format *)
   {prog = mapExps (prog, convertSourceMarkValueToStatic targetVars)}
end

end
