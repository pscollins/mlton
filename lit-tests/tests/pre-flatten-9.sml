(* RUN: mpl-compile -ssa-passes preFlatten \
   RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy always \
   RUN:    -pre-flatten-resolve-policy local \
   RUN:    -pre-flatten-types-policy any \
   RUN:    %s %t

   Test that `preFlatten` builds a new flattened version and calls it. Since the
   consumer policy is `always`, it should succeed.

   Non-flat version in 'pre'
   RUN: egrep    'doAdd_. .*pairs_' %t/*preFlatten*.pre.ssa
   RUN: egrep    'call.*doAdd_. '  %t/*preFlatten*.pre.ssa

   Non-flat version no longer called in 'post'
   RUN: ! egrep 'call doAdd_. ' %t/*preFlatten*.post.ssa

   Flattened verisons are present
   RUN: egrep    'doAdd.*flat_0'  %t/*preFlatten*.post.ssa
   RUN: egrep    'doAdd.*flat_1'  %t/*preFlatten*.post.ssa

   RUN: egrep  'call.*doAdd.*flat_0'  %t/*preFlatten*.post.ssa
   RUN: egrep  'call.*doAdd.*flat_1'  %t/*preFlatten*.post.ssa

 *)

datatype pairs = pairs1 of int * int
               | pairs2 of int * int

fun  doAdd (pairs) = let
   val (x, y) =
       case pairs of
           pairs1 p => p
         | pairs2 p => p
in
   x + y
end

val res1 = doAdd (pairs1 (1, 1))
val res2 = let
   val inputs = Array.array (1, (1, 1))
in
   doAdd (pairs2 (Array.sub (inputs, 1)))
end

val _ = print (Int.toString (res1 + res2))
