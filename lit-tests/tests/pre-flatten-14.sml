(* RUN: mpl-compile -ssa-passes preFlatten:flatten \
 *    RUN:    -keep-pass 'preFlatten.*|flatten.*' -stop-pass 'preFlatten.*' \
 *    RUN:    -pre-flatten-phase early \
 *    RUN:    -pre-flatten-max-iters 1 \
 *    RUN:    -pre-flatten-consumer-policy always \
 *    RUN:    -pre-flatten-resolve-policy local \
 *    RUN:    %s %t.early
 *
 *    RUN: ls %t.early/*.preFlatten*.post.ssa
 *    RUN: ! ls %t.early/*.flatten*.post.ssa
 *
 *    RUN: mpl-compile -ssa-passes preFlatten:flatten \
 *    RUN:    -keep-pass 'preFlatten.*|flatten.*' -stop-pass 'preFlatten.*' \
 *    RUN:    -pre-flatten-phase late \
 *    RUN:    -pre-flatten-max-iters 1 \
 *    RUN:    -pre-flatten-consumer-policy always \
 *    RUN:    -pre-flatten-resolve-policy local \
 *    RUN:    %s %t.late
 *
 *    RUN: ls %t.late/*.preFlatten*.post.ssa
 *    RUN: ls %t.late/*.flatten*.post.ssa
 *)

fun doAdd (args: int * int) = let
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
