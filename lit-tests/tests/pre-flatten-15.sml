(* RUN: mpl-compile -ssa-passes preFlatten \
   RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy always \
   RUN:    -pre-flatten-resolve-policy local \
   RUN:    -pre-flatten-transfer-policy always \
   RUN:    %s %t.always

   With 'always' policy, all calls to doAdd should be flattened
   RUN: egrep 'call.*doAdd_[0-9]+_flat' %t.always/*preFlatten*.post.ssa
   RUN: ! egrep 'call.*\(doAdd_[0-9]+ '   %t.always/*preFlatten*.post.ssa

   RUN: mpl-compile -ssa-passes preFlatten \
   RUN:    -keep-pass 'preFlatten.*' -stop-pass 'preFlatten.*' \
   RUN:    -pre-flatten-max-iters 1 \
   RUN:    -pre-flatten-consumer-policy always \
   RUN:    -pre-flatten-resolve-policy local \
   RUN:    -pre-flatten-transfer-policy tail_only \
   RUN:    %s %t.tail

   With 'tail_only' policy:
   1. The tail call in runTest should call the flat version
   RUN: egrep 'call.*doAdd_[0-9]+_flat' %t.tail/*preFlatten*.post.ssa
   2. The non-tail calls in runTest should call the non-flat version
   RUN: egrep 'call.*\(doAdd_[0-9]+ '     %t.tail/*preFlatten*.post.ssa
*)

fun doAdd (args: Word32.word * Word32.word) = let
   val (x, y) = args
in
   Word32.+ (x, y)
end

fun runTest () = let
   val res1 = doAdd (0w1, 0w2)
   val res2 = Word32.+ (doAdd (0w3, 0w4), 0w1)
in
   doAdd (res1, res2)
end

val _ = print (Int.toString (Word32.toInt (Word32.+ (runTest (), runTest ()))))
