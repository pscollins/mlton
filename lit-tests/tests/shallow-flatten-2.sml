(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    -shallow-flatten-max-iters 1 \
   RUN:    %s %t

   Test that `shallowFlatten` does not flatten an array of 3-tuples (for maxWidth:2)

   Non-flat version in 'pre'
   RUN: grep -F 'Array_alloc[(real64, real64, real64) tuple]'  %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_length[(real64, real64, real64) tuple]' %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_sub[(real64, real64, real64) tuple]'    %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_update[(real64, real64, real64) tuple]' %t/*shallowFlatten*.pre.ssa
   This particular program contains no real64 arrays pre-flattening
   RUN: ! grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_length[real64]' %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_update[real64]' %t/*shallowFlatten*.pre.ssa

   Non-flat version is still present in `post`
   RUN: grep -F 'Array_alloc[(real64, real64, real64) tuple]'  %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_length[(real64, real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_sub[(real64, real64, real64) tuple]'    %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_update[(real64, real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   Flat version is not present
   RUN: ! grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_length[real64]' %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_update[real64]' %t/*shallowFlatten*.pre.ssa
 *)

fun addTriple (x, y, z) = x + y + z

val _ = let
   val arr: (real * real * real) array = Array.fromList [(1.0, 2.0, 2.5), (3.0, 4.0, 4.5)]
in
   print (Real.toString (addTriple (Array.sub (arr, 1))))
end
