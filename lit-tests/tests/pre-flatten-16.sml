(* RUN: mpl-compile -ssa-passes preFlatten \
 *    RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
 *    RUN:    -pre-flatten-max-iters 1 \
 *    RUN:    -pre-flatten-consumer-policy always \
 *    RUN:    -pre-flatten-resolve-policy local \
 *    RUN:    -pre-flatten-post-steps-only false \
 *    RUN:    %s %t.default
 *
 *    Test that with -pre-flatten-post-steps-only false (the default),
 *    preFlatten creates flattened functions and callsites.
 *    RUN: egrep    'doAdd.*tuple' %t.default/*preFlatten*.pre.ssa
 *    RUN: egrep    'call.*doAdd'  %t.default/*preFlatten*.pre.ssa
 *    RUN: egrep    'doAdd.*flat' %t.default/*preFlatten*.post.ssa
 *    RUN: egrep    'call.*doAdd.*flat'  %t.default/*preFlatten*.post.ssa
 *
 *    RUN: mpl-compile -ssa-passes preFlatten \
 *    RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
 *    RUN:    -pre-flatten-max-iters 1 \
 *    RUN:    -pre-flatten-consumer-policy always \
 *    RUN:    -pre-flatten-resolve-policy local \
 *    RUN:    -pre-flatten-post-steps-only true \
 *    RUN:    %s %t.postonly
 *
 *    Test that with -pre-flatten-post-steps-only true,
 *    the actual preFlatten pass is skipped (only post-steps run),
 *    so no flattened function or callsites are created.
 *    RUN: egrep    'doAdd.*tuple' %t.postonly/*preFlatten*.pre.ssa
 *    RUN: egrep    'call.*doAdd'  %t.postonly/*preFlatten*.pre.ssa
 *    RUN: egrep    'doAdd.*tuple' %t.postonly/*preFlatten*.post.ssa
 *    RUN: egrep    'call.*doAdd'  %t.postonly/*preFlatten*.post.ssa
 *    RUN: ! egrep  'doAdd.*flat' %t.postonly/*preFlatten*.post.ssa
 *    RUN: ! egrep  'call.*doAdd.*flat'  %t.postonly/*preFlatten*.post.ssa
 *)

fun doAdd (args: int * int) = let
   val (x, y) = args
in
   x + y
end

val res1 = doAdd ((1, 1))
val res2 = let
   val inputs = Array.array (1, (1, 1))
in
   doAdd (Array.sub (inputs, 1))
end

val _ = print (Int.toString (res1 + res2))
