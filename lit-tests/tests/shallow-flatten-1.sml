(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    %s %t

   Test that `shallowFlatten` flattens an array of 2-tuples (for maxWidth:2)

   Non-flat version in 'pre'
   RUN: grep -F 'Array_alloc[(real64, real64) tuple]'  %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_length[(real64, real64) tuple]' %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_sub[(real64, real64) tuple]'    %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_update[(real64, real64) tuple]' %t/*shallowFlatten*.pre.ssa
   This particular program contains no real64 arrays pre-flattening
   RUN: ! grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_length[real64]' %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_update[real64]' %t/*shallowFlatten*.pre.ssa

   Non-flat version is gone from `post`
   RUN: ! grep -F 'Array_alloc[(real64, real64) tuple]'  %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_length[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_sub[(real64, real64) tuple]'    %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_update[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   Flat version is now present
   RUN: grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_length[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_update[real64]' %t/*shallowFlatten*.post.ssa
 *)

fun addPair (l, r) = l + r

val _ = let
   val arr: (real * real) array = Array.fromList [(1.0, 2.0), (3.0, 4.0)]
in
   print (Real.toString (addPair (Array.sub (arr, 1))))
end
