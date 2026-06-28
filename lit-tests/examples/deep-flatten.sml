(* Example to demonstrate the DeepFlatten transformation in MLton.
 *
 * To see the transformation, compile this program keeping the deepFlatten pass files:
 *   mlton -keep-pass 'deepFlatten.*' deep-flatten.sml
 *
 * Under the hood:
 * - Pre-deepFlatten (....pre.ssa): The array 'arr' is allocated as an array of tuples:
 *     Array_alloc[(int, int) tuple]
 * - Post-deepFlatten (....post.ssa): The tuple is flattened, transforming the array 
 *   into a sequence of two inline int fields. The tuple structure is completely 
 *   compiled away.
 *)

val _ = let
   val arr = Array.tabulate (10, fn i => (i, i + 1))
   val (x, y) = Array.sub (arr, 5)
in
   print (Int.toString (x + y) ^ "\n")
end
