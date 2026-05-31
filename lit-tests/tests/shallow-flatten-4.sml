(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    %s %t
  
   Test that when we have a nested type, the inner array of 2-tuples is flattened
   (since maxWidth:2) but the outer array of 4-tuples is NOT flattened.

   RUN: ! grep -F 'Array_alloc[((real64, real64) tuple) array]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_alloc[(real64, real64) tuple]'        %t/*shallowFlatten*.post.ssa
   RUN: grep -F  'Array_alloc[real64]'                         %t/*shallowFlatten*.post.ssa
   RUN: grep -F  'Array_alloc[(((real64) array, (real64) array) tuple,' %t/*shallowFlatten*.post.ssa
 *)

val n = List.length (CommandLine.arguments ()) + 10
val r = Real.fromInt n

(* Allocate inner arrays *)
val inner1: (real * real) array = Array.array (n, (r, r + 1.0))
val inner2: (real * real) array = Array.array (n, (r + 2.0, r + 3.0))

(* Create the outer array of 4-tuples *)
val outer: ((real * real) array * real * real * real) array =
    Array.array (n, (inner1, r + 4.0, r + 5.0, r + 6.0))

(* Update an element in the outer array *)
val _ = Array.update (outer, 1, (inner2, r + 7.0, r + 8.0, r + 9.0))

(* Retrieve and access *)
val (inner, a, b, c) = Array.sub (outer, 0)
val (x, y) = Array.sub (inner, 0)

val _ = print (Real.toString x ^ " " ^ Real.toString y ^ " " ^
               Real.toString a ^ " " ^ Real.toString b ^ " " ^ Real.toString c ^ "\n")
