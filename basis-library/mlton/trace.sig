signature MLTON_TRACE = sig

   (* Emits a `// Diagnostic(...)` comment in the generated C, where the
   contents of the comment is the string argument. *)
    val sourceMark: string -> unit

    (* Like above, but consumes a value. The generated comment contains the name
    of the value in `Machine` IR. This is helpful for the sake of limiting code
    movement around the mark. *)
    val sourceMarkValue: 'a * string -> unit

    (* The call `noHeap (expr)` asserts that `expr` does not correspond to a
    pointer dereference, failing compilation if so. *)
    val noHeap: 'a -> 'a

    (* `heapOK` waives the error introduced by `noHeap`, i.e. `noHeap o heapOK`
    is guaranteed to be a noop that always succeeds. *)
    val heapOK: 'a -> 'a

    (* The call `noTuple (expr)` asserts that `expr` does not correspond to a
    tuple member access, failing compilation if so. *)
    val noTuple: 'a -> 'a
end
