(* RUN: mpl-compile \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy always \
   RUN:    -pre-flatten-resolve-policy global \
   RUN:    -pre-flatten-types-policy any \
   RUN:    -pre-flatten-post-steps shrink,flatten,shrink \
   RUN:    %s.mlb %t

   Test that `preFlatten` solves a `noTuple` annotation

   No assertions on the IR: we just want to test that it compiles

 *)

val x = Array.sub (ForkJoin.alloc 1: real array, 0)
fun f n = if n = 0 then () else (MLton.Trace.noTuple x; ForkJoin.par (fn _ => f (n-1), fn _ => ()); ())
val _ = f 1
