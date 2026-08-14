(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    -shallow-flatten-mechanism aos \
   RUN:    -shallow-flatten-max-iters 1 \
   RUN:    -inline 0 \
   RUN:    %s %t
  
   Test that function/block arguments of tuple-array types are flattened in AoS mode.

   RUN: ! grep -F 'Array_alloc[(real64, real64) tuple]'  %t/*shallowFlatten*.post.ssa
   RUN: grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.post.ssa
  *)

fun process (arr: (real * real) array) = let
   val (x, y) = Array.sub (arr, 0)
in
   x + y
end

val n = List.length (CommandLine.arguments ()) + 10
val r = Real.fromInt n

val arr = Array.array (n, (r, r + 1.0))
val res = process arr
val _ = print (Real.toString res ^ "\n")
