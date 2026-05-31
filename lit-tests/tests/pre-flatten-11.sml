(* RUN: mpl-compile -ssa-passes preFlatten \
   RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy any_unpack \
   RUN:    -pre-flatten-resolve-policy global \
   RUN:    -pre-flatten-types-policy any \
   RUN:    %s %t

   Test that `preFlatten` builds a new flattened version and calls it.

   The structure of the IR is:

   fun doAdd (env, arg: pairs)
     case pairs of pairs1 => ... | pairs2  => ...

   so both `any_unpack` and `all_unpack` should succeed

   TODO(pscollins): This is intended to be the case where `local` kicks in, but
   it doesn't. Investigate.

   Non-flat version in 'pre'
   RUN: egrep    'doAdd.*pairs_' %t/*preFlatten*.pre.ssa
   RUN: egrep    'call.*doAdd'  %t/*preFlatten*.pre.ssa

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
