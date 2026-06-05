(* RUN: mpl-compile -keep-pass annotateTrace %s %t

   Test sourceMark -> staticSourceMark conversion

   Dynamic version in `pre`
   RUN: grep    'Trace_sourceMark ("mark1")' %t/*annotateTrace.pre.core-ml
   RUN: grep    'Trace_sourceMark ("mark2")' %t/*annotateTrace.pre.core-ml

   Static version in `.post`
   RUN: grep 'Trace_staticSourceMark:mark1' %t/*annotateTrace.post.core-ml
   RUN: grep 'Trace_staticSourceMark:mark2' %t/*annotateTrace.post.core-ml
 *)

val _ = let
    val kConst = 123456789
    val m1 = MLton.Trace.sourceMark "mark1"
    val kConst2 = kConst + 1
    val m2 = MLton.Trace.sourceMark "mark2"
in
    print (Int.toString kConst2)
end
