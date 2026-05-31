(* RUN: mpl-compile -ssa-passes preFlatten \
   RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy always \
   RUN:    -pre-flatten-resolve-policy global \
   RUN:    -pre-flatten-recursive-steps 1 \
   RUN:    %s %t

   Test that `preFlatten` handles a recursive call

   The structure of the IR is:

   fun doAdd (env, arg: int * int):
     call tail doAdd (env, arg)

   Non-flat version in 'pre'
   RUN: egrep    'doAdd_. \(' %t/*preFlatten*.pre.ssa
   RUN: egrep    'call.*doAdd_. '  %t/*preFlatten*.pre.ssa

   Non-flat version no longer called in 'post'
   RUN: ! egrep 'call.*doAdd_. '  %t/*preFlatten*.post.ssa

   Flattened verison is present instead
   RUN: egrep    'doAdd_._flat.*\('       %t/*preFlatten*.post.ssa
   RUN: egrep    'call.*doAdd_._flat.* '  %t/*preFlatten*.post.ssa
 *)


fun  doAdd (args: int * int, count: int) = let
   val (x, y) = args
in
   if count = 0 then
      x + y
   else doAdd ((y, x), count - 1)
end

val kCount = 3
val res  = doAdd((1, 1), kCount)

val _ = print (Int.toString (res))
