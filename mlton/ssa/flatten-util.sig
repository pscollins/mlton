signature FLATTEN_UTIL =
sig
   include SSA_TRANSFORM_STRUCTS

   (* Applies an effectful expression to each `Function.t` in `p` *)
   val foreachFunction: Program.t * (Function.t -> unit) -> unit

   (* Manages Func.t -> Function + Label.t -> Block mappings *)
   type funcsMap = {
      (* Returns the `Function.t` for each `Func.t` in the program  *)
      getFunc: Func.t -> Function.t,
      (* Returns the `Block.t` corresponding to the provided `Label.t` *)
      getBlock: Label.t -> Block.t,
      (* Given a `Func.t`, returns all of the functions that it calls *)
      getCallees: Func.t -> Func.t vector,
      (* Cleans up state associated with this object *)
      destroyFuncsMap: unit -> unit
   }

   (* Builds a new `funcsMap` over the provided program *)
   val newFuncsMap: Program.t -> funcsMap

   (* Interface for applying a transformation to a specified program *)
   type rewriter = {
      (* Transform the given statements (block body or globals) *)
      doStatements: Statement.t vector -> Statement.t vector,
      (* Transform the given typed variables (block or function arguments) *)
      doArgs: (Var.t * Type.t) vector -> (Var.t * Type.t) vector,
      (* Transform the given transfer: the `Func.t` argument is the current
      function *)
      doTransfer: (Func.t * Transfer.t) -> Transfer.t
   }

   (* Applies `rewriter` to the `Program.t`

      Guarantees that all constructs are visited in BFS order, i.e. the
      definition of any `Var.t` is always visted before its use. Consequently:

        * Globals are visited before any `Function.t`
        * `Function.t`s are visited in some topological order
        * The args of a `Function.t` are visited before any block in the
          `Function.t`
        * `Block.t`s within a function are visited in some topological order
        * `Block.t` arguments are visited before any statement in the block body
        * Every program construct is visited, even if the program CFG is
          disconnected
        *)
   val rewriteBfs: rewriter -> Program.t -> Program.t

   (* Effectful version of the interface above *)
   type visitor = {
      foreachStatements: Statement.t vector -> unit,
      foreachArgs: (Var.t * Type.t) vector -> unit,
      foreachTransfer: Transfer.t -> unit
   }

   (* Like above, but for side-effecting expressions *)
   val foreachBfs: visitor -> Program.t -> unit
end
