(* RUN: mpl-compile -ssa-passes preFlatten \
   RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy all_unpack \
   RUN:    -pre-flatten-resolve-policy global \
   RUN:    %s %t

   Test that `preFlatten` follows the supplied policy.

   The structure of the IR is:

   fun doAdd (env, arg: int * int):
     call tail implFn (env, arg)

   where all of the transitive uses of `arg` are `Select`s: `global` +
   `all_unpack` should succeed.

   Non-flat version in 'pre'
   RUN: egrep    'doAdd.*tuple' %t/*preFlatten*.pre.ssa
   RUN: egrep    'call.*doAdd'  %t/*preFlatten*.pre.ssa

   Non-flat version still in 'post'
   RUN: egrep    'doAdd.*tuple' %t/*preFlatten*.post.ssa
   RUN: egrep    'call.*doAdd'  %t/*preFlatten*.post.ssa

   Flattened verison is additionally present

   RUN: egrep    'doAdd.*flat' %t/*preFlatten*.post.ssa
   RUN: egrep    'call.*doAdd.*flat'  %t/*preFlatten*.post.ssa
 *)


fun  doAdd (args: int * int) = let
   val (x, y) = args
in
   x + y
end

val res1 = doAdd((1, 1))
val res2 = let
   val inputs = Array.array (1, (1, 1))
in
   doAdd (Array.sub (inputs, 1))
end

val _ = print (Int.toString (res1 + res2))
