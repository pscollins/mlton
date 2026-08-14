(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    -shallow-flatten-mechanism aos \
   RUN:    -shallow-flatten-max-iters 1 \
   RUN:    %s %t
  
   Test that iterative flattening works for `((real * real) array) array` in AoS mode.
   It should flatten the inner array of tuples in the first pass to an array of reals, and then
   the outer array of arrays will remain as `real array array` in the second pass.

   RUN: ! grep -F 'Array_alloc[((real64, real64) tuple) array]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_alloc[(real64, real64) tuple]'        %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_alloc[real64]'                          %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_alloc[(real64) array]'                  %t/*shallowFlatten*.post.ssa
  *)

val n = List.length (CommandLine.arguments ()) + 10
val r = Real.fromInt n

(* Create array *)
val inner = Array.array (n, (r, r + 1.0))
val arr: ((real * real) array) array = Array.array (n, inner)

(* Sub and Update *)
val sub_arr = Array.sub (arr, 0)
val (x, y) = Array.sub (sub_arr, 0)

val _ = print (Real.toString x ^ " " ^ Real.toString y ^ "\n")
