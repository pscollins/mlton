(* RUN: mpl-compile -keep-pass annotateTraceValue annotate-trace-value-5.mlb %t || true

   Test sourceMarkValue -> staticSourceMarkValue conversion

   (Test case for understanding `DeepFlatten`)

   Expect static version in `.post`
   RUN: grep 'Trace_staticSourceMarkValue:px1' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:py1' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:px2' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:py2' %t/*annotateTraceValue.post.core-ml
 *)
structure Seq:
sig
  type grain = int

  val tabulate: grain -> (int * int) -> (int -> 'a) -> 'a array
end =
struct

  type grain = int

  structure A = Array
  structure AS = ArraySlice

  fun upd a i x = A.update (a, i, x)
  fun nth a i   = A.sub (a, i)

  val parfor = ForkJoin.parfor
  val par = ForkJoin.par
  val allocate = ForkJoin.alloc

  fun tabulate grain (lo, hi) f =
    let
      val n = hi-lo
      val result = allocate n
    in
      if lo = 0 then
        parfor grain (0, n) (fn i => upd result i (f i))
      else
        parfor grain (0, n) (fn i => upd result i (f (lo+i)));

      result
    end

end

val _ = let
   val kGrain = 10
   fun mkElement idx = (Real.fromInt idx, Real.fromInt (idx + 1))
   val arr = Seq.tabulate kGrain (10, 1000) mkElement
   val p1 = Array.sub (arr, 0)
   val x1 = #1 p1
   val y1 = #2 p1
   val _ = MLton.Trace.sourceMarkValue (x1, "px1")
   val _ = MLton.Trace.sourceMarkValue (y1, "py1")

   val p2 = Array.sub (arr, 1)
   val x2 = #1 p2
   val y2 = #2 p2
   val _ = MLton.Trace.sourceMarkValue (x2, "px2")
   val _ = MLton.Trace.sourceMarkValue (y2, "py2")
in
   print (Real.toString (x1 + y1 + x2 + y2))
end
