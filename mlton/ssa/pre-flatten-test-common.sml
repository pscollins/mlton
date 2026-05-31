structure Atoms = Atoms ()
structure Ssa = Ssa (open Atoms)
structure PreFlatten = PreFlatten (Ssa)

(* val _ = Control.diagnosticWriter := SOME (fn l => Layout.outputl (l, Out.standard)) *)

(* Debug helper for printing *)
fun programToString program =
    let
       val segments = ref []
       (* Accumulate each layout part into the segments list *)
       val _ = Ssa.Program.layouts (program, fn l =>
                                                (Layout.print (l, fn s => segments := s :: !segments)
                                                ; segments := "\n" :: !segments))
    in
       String.concat (List.rev (!segments))
    end

fun printProgram (label, program) = let 
   val msgParts = [
      "Program ",
      label,
      "contents: \n",
      programToString program,
      "\n"
      ]
in
   print (String.concat msgParts)
end

fun printFail msg =
    (print (msg ^ "\n");
     OS.Process.exit OS.Process.failure)

fun assert (cond, msg) =
   if cond then () else (print ("Assertion failed: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)

fun assertEqualStrings (expected, actual, msg) =
   let
      fun join l =
         case l of
            [] => ""
          | [x] => x
          | x :: xs => x ^ ", " ^ (join xs)
      val e = join expected
      val a = join actual
   in
      if e = a then ()
      else (print (msg ^ "\n");
            print ("Expected: [" ^ e ^ "]\n");
            print ("Actual:   [" ^ a ^ "]\n");
            OS.Process.exit OS.Process.failure)
   end

fun choiceResToString res =
   case res of
      PreFlatten.NoOp => "NoOp"
    | PreFlatten.Valid => "Valid"
    | PreFlatten.Invalid => "Invalid"

val log = ref []
fun addLog s = log := s :: !log
fun getLog () = List.rev (!log)
fun clearLog () = log := []

val walker = {
   beforeFunc = fn f => addLog ("beforeFunc " ^ (Ssa.Func.toString (Ssa.Function.name f))),
   afterFunc = fn f => addLog ("afterFunc " ^ (Ssa.Func.toString (Ssa.Function.name f))),
   beforeBlock = fn b => addLog ("beforeBlock " ^ (Ssa.Label.toString (Ssa.Block.label b))),
   afterBlock = fn b => addLog ("afterBlock " ^ (Ssa.Label.toString (Ssa.Block.label b))),
   statement = fn s =>
      case Ssa.Statement.var s of
         SOME v => addLog ("statement " ^ (Ssa.Var.toString v))
       | NONE => addLog "statement <none>"
}

val emptyProgram = 
   Ssa.Program.T {datatypes=Vector.new0(),
                  functions=[],
                  globals=Vector.new0(),
                  main=Ssa.Func.fromString "dummy"}
