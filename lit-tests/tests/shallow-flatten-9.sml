(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidthSameType:2 \
   RUN:    -shallow-flatten-max-iters 1 \
   RUN:    %s %t

   Test that `shallowFlatten` does NOT flatten an array of 2-tuples with mixed types (for maxWidthSameType:2)

   Non-flat version in 'pre'
   RUN: grep -F 'Array_alloc[(real64, word32) tuple]'  %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_length[(real64, word32) tuple]' %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_sub[(real64, word32) tuple]'    %t/*shallowFlatten*.pre.ssa
   RUN: grep -F 'Array_update[(real64, word32) tuple]' %t/*shallowFlatten*.pre.ssa
   This particular program contains no real64 or word32 arrays pre-flattening
   RUN: ! grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_length[real64]' %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_update[real64]' %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F  'Array_alloc[word32]'  %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_length[word32]' %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_sub[word32]'    %t/*shallowFlatten*.pre.ssa
   RUN: ! grep -F 'Array_update[word32]' %t/*shallowFlatten*.pre.ssa

   Non-flat version is still present in `post`
   RUN: grep -F 'Array_alloc[(real64, word32) tuple]'  %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_length[(real64, word32) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_sub[(real64, word32) tuple]'    %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_update[(real64, word32) tuple]' %t/*shallowFlatten*.post.ssa
   Flat version is not present
   RUN: ! grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_length[real64]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_update[real64]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F  'Array_alloc[word32]'  %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_length[word32]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_sub[word32]'    %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_update[word32]' %t/*shallowFlatten*.post.ssa
 *)

fun addPair (l, r) = l + Real.fromInt r

val _ = let
   val arr: (real * int) array = Array.fromList [(1.0, 2), (3.0, 4)]
in
   print (Real.toString (addPair (Array.sub (arr, 1))))
end
