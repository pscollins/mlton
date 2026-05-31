(* RUN: mlton-print-c %s > %t

   Test for `mlton-print-c`: verify that we can find a constant in the generated C

   RUN: grep 123456789 %t
   RUN: grep Stdio_print %t
 *)

val _ = let
    val kConst = 123456789;
in
    print (Int.toString kConst)
end
