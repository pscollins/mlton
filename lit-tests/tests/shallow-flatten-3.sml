(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    %s.mlb %t
  
   Test that all supported Array/Vector primitives are flattened.

   RUN: ! grep -F 'Array_alloc[(real64, real64) tuple]'  %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_length[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_sub[(real64, real64) tuple]'    %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_update[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_toVector[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_toArray[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Array_uninit[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Vector_length[(real64, real64) tuple]' %t/*shallowFlatten*.post.ssa
   RUN: ! grep -F 'Vector_sub[(real64, real64) tuple]'    %t/*shallowFlatten*.post.ssa

   RUN: grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_length[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_sub[real64]'    %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_update[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_toVector[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_toArray[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Array_uninit[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Vector_length[real64]' %t/*shallowFlatten*.post.ssa
   RUN: grep -F 'Vector_sub[real64]'    %t/*shallowFlatten*.post.ssa
 *)

val allocRaw = _prim "Array_allocRaw": Word64.word -> (real * real) array;
val toVector = _prim "Array_toVector": (real * real) array -> (real * real) vector;
val toArray = _prim "Array_toArray": (real * real) array -> (real * real) array;
val uninit = _prim "Array_uninit": (real * real) array * Word64.word -> unit;
val uninitIsNop = _prim "Array_uninitIsNop": (real * real) array -> bool;

val n = List.length (CommandLine.arguments ()) + 10

(* 1. Array_alloc (via Array.array), Array_update *)
val a1 = Array.array (n, (1.0, 2.0))
val _ = Array.update (a1, 0, (3.0, 4.0))

(* 2. Array_length & Array_sub *)
val len1 = Array.length a1
val (x1, y1) = Array.sub (a1, 0)

(* 3. Array_toVector, Vector_length, Vector_sub *)
val vec = toVector a1
val len2 = Vector.length vec
val (x2, y2) = Vector.sub (vec, 0)

(* 4. Array_alloc {raw = true} via allocRaw, Array_uninit, Array_uninitIsNop, Array_toArray *)
val a2 = allocRaw (Word64.fromInt n)
val _ = uninit (a2, 0w0)
val nop = uninitIsNop a2
val a3 = toArray a2

val (x3, y3) = Array.sub (a3, 0)

(* print to keep alive *)
val _ = print (Real.toString x1 ^ " " ^ Real.toString y1 ^ " " ^
               Real.toString x2 ^ " " ^ Real.toString y2 ^ " " ^
               Real.toString x3 ^ " " ^ Real.toString y3 ^ " " ^
               Int.toString len1 ^ " " ^ Int.toString len2 ^ " " ^
               Bool.toString nop ^ "\n")
