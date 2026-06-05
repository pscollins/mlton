(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AnnotateTrace (S: ANNOTATE_TRACE_STRUCTS): ANNOTATE_TRACE =
struct

open S
open CoreML
structure CoreMLUtil = CoreMLUtil (structure CoreML = CoreML)
open CoreMLUtil

fun annotateTrace {prog} =
    {prog = mapExps (prog, convertSourceMarkToStatic)}

end
