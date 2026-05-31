structure Atoms = Atoms ()
structure Ssa = Ssa (open Atoms)
structure ShallowFlatten = ShallowFlatten (Ssa)
structure FlattenUtil = FlattenUtil (Ssa)

(* val _ = Control.diagnosticWriter := SOME (fn l => Layout.outputl (l, Out.standard)) *)

fun assert (cond, msg) =
   if cond then () else (print ("Assertion failed: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)
