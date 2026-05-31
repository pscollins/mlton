functor FlattenUtil (S: SSA_TRANSFORM_STRUCTS): FLATTEN_UTIL =
struct
open S

(* Applies an effectful expression to each `Function.t` in `p` *)
fun foreachFunction (p: Program.t, funcF: (Function.t -> unit)): unit = let
   val Program.T {functions, ...} = p
in
   List.foreach (functions, funcF)
end

(* Manages Func.t -> Function + Label.t -> Block mappings *)
type funcsMap = {
   getFunc: Func.t -> Function.t,
   getBlock: Label.t -> Block.t,
   getCallees: Func.t -> Func.t vector,
   destroyFuncsMap: unit -> unit
}

fun collectCallees (f: Function.t): Func.t vector = let
   fun maybeGetCallee b =
       case Block.transfer b of
           Transfer.Call {func, ...} => SOME func
         | _ => NONE
in
   Vector.keepAllMap (Function.blocks f, maybeGetCallee)
end

fun newFuncsMap (p: Program.t): funcsMap = let
   val {get=getFunc, set=setFunc, destroy=destroyFuncsMapFuncs} =
       Property.destGetSetOnce (Func.plist,
                                Property.initRaise ("function lookup", Func.layout))

   val {get=getBlock, set=setBlock, destroy=destroyFuncsMapBlocks} =
       Property.destGetSetOnce (Label.plist,
                                Property.initRaise ("block lookup", Label.layout))

   val {get=getCallees, set=setCallees, destroy=destroyCallees} =
       Property.destGetSetOnce (Func.plist,
                                Property.initRaise ("callees lookup", Func.layout))

   fun destroyFuncsMap() = let
      val _ = destroyFuncsMapFuncs()
      val _ = destroyFuncsMapBlocks()
      val _ = destroyCallees()
   in
      ()
   end

   fun addBlockToMapping (b: Block.t) = setBlock (Block.label b, b)
   fun addFuncToMapping (f: Function.t) = let
      val _ = Vector.foreach (Function.blocks f, addBlockToMapping)
      val _ = setCallees (Function.name f, collectCallees f)
   in
      setFunc (Function.name f, f)
   end
   (* Use foreachFunction rather than `walker` because the DFS traversal pattern
      doesn't reach disconnected functions, and so a program containing any such
      function hits the `initRaise` above. The ordering of our `addFuncToMapping`
      calls doesn't matter, so we might as well avoid the error by just setting up
      the mapping for all functions. *)
   val _ = foreachFunction (p, addFuncToMapping)
in
   {getFunc = getFunc,
    getBlock = getBlock, getCallees = getCallees, destroyFuncsMap = destroyFuncsMap}
end

type rewriter = {
   doStatements: Statement.t vector -> Statement.t vector,
   doArgs: (Var.t * Type.t) vector -> (Var.t * Type.t) vector,
   doTransfer: (Func.t * Transfer.t) -> Transfer.t
}

type 'a visited = {
   markVisited: 'a -> bool,
   destroyVisited: unit -> unit
}

fun mkVisited plist = let
   fun init _ = ref false
   val {get=getVisited, destroy=destroyVisited, ...} =
       Property.destGet (plist, Property.initFun init)
   fun markVisited p = let
      val seenRef = getVisited p
      val wasSeen = !seenRef
      val _ = seenRef := true
   in
      wasSeen
   end
in
   {markVisited = markVisited,
    destroyVisited = destroyVisited}
end

type 'a mutableQueue = {
   push: 'a -> unit,
   pop: unit -> 'a option
}
fun mkQueue inits = let
   val q = ref (Queue.empty())
   fun push f = q := Queue.enque (!q, f)
   fun pop () =
       case Queue.deque (!q) of
           SOME (q', el) => (q := q'; SOME el)
         | NONE => NONE
   val _ = List.foreach (inits, push)
in
   {push = push, pop = pop}
end

type ('a, 'label) bfsArg = {
   getLabel: 'a -> 'label,
   getPlist: 'label -> PropertyList.t,
   labelLayout: 'label -> Layout.t,
   getChildren: 'label -> 'label vector,
   rewriteElement: 'label -> 'a
}

fun applyRewrite (init: 'label, all: 'a list,
                  arg: ('a, 'label) bfsArg): 'a list = let
   val {getLabel, getPlist, labelLayout, getChildren,
        rewriteElement} = arg
   val allLabels = List.map (all, getLabel)
   val {pop=popRemaining, ...} : 'label mutableQueue
       = mkQueue allLabels
   val {push=pushFunc, pop=popFunc} : 'label mutableQueue
       = mkQueue [init]
   val {markVisited, destroyVisited} = mkVisited getPlist
   val {get=getRewrittenFunc: 'label -> 'a,
        set=setRewrittenFunc: 'label * 'a -> unit,
        destroy=destroyRewrittenFunc} =
       Property.destGetSetOnce (getPlist, Property.initRaise ("result lookup",
                                                              labelLayout))
   fun getNext(): 'label option =
       case popFunc() of
           NONE => pushLeftovers()
         | x => x
   and pushLeftovers(): 'label option =
       case (popRemaining(): 'label option) of
           NONE => NONE
         | SOME f  => (pushFunc f; getNext())

   fun visitFunc f = let
      val _ = Vector.foreach (getChildren f, pushFunc)
   in
      rewriteElement f
   end
   fun maybeVisit f =
       if markVisited f then ()
       else (setRewrittenFunc (f, visitFunc f))

   fun doVisit () =
       case getNext() of
           NONE => ()
         | SOME f => (maybeVisit f; doVisit())

   val _ = doVisit ()
   val results = List.map (allLabels, getRewrittenFunc)
   val _ = (destroyVisited();
            destroyRewrittenFunc())
in
   results
end

fun getBlockCallees (b: Block.t): Label.t vector = let
   fun getLabel (_, l) = l
   fun extractCases (c: (Con.t, Label.t) Cases.t): Label.t vector =
       case c of
           Cases.Con cons => Vector.map (cons, getLabel)
         | Cases.Word (_, cons) => Vector.map (cons, getLabel)
   fun extractDefault d =
       case d of
           SOME d' => Vector.new1 d'
         | NONE => Vector.new0()
in
   case Block.transfer b of
       Transfer.Goto {dst, ...} => Vector.new1 dst
     | Transfer.Case {cases, default, ...} =>
       Vector.concat [extractCases cases,
                      extractDefault default]
     | _ => Vector.new0()
end

fun rewriteBfs (r: rewriter) (p: Program.t): Program.t = let
   val {doStatements, doArgs, doTransfer} = r
   val Program.T {datatypes, functions, globals, main} = p
   val {getFunc, getBlock, destroyFuncsMap, getCallees} = newFuncsMap p
   fun getLabelCallees l = getBlockCallees (getBlock l)
   fun rewriteBlocks (fName, start, allVec) = let
      fun rewriteBlock l = let
         val Block.T {args, label, statements, transfer} = getBlock l
      in
         Block.T {args = doArgs args,
                  label = label,
                  statements = doStatements statements,
                  transfer = doTransfer (fName, transfer)}
      end
      val blockBfsArg: (Block.t, Label.t) bfsArg = {
         getLabel = Block.label,
         getPlist = Label.plist,
         labelLayout = Label.layout,
         getChildren = getLabelCallees,
         rewriteElement = rewriteBlock
      }
   in
      applyRewrite (start, allVec, blockBfsArg)
   end
   fun rewriteFuncs () = let
      fun doRewriteFunc fName = let
         val {args, blocks, mayInline, name, raises, returns, start} =
             Function.dest (getFunc fName)
         val newArgs = doArgs args
         val newBlocks = rewriteBlocks (fName, start, Vector.toList blocks)
      in
         Function.new {args = newArgs,
                       blocks = Vector.fromList newBlocks,
                       mayInline = mayInline,
                       name = name,
                       raises = raises,
                       returns = returns,
                       start = start}
      end
      val funcBfs: (Function.t, Func.t) bfsArg = {
         getLabel = Function.name,
         getPlist = Func.plist,
         labelLayout = Func.layout,
         getChildren = getCallees,
         rewriteElement = doRewriteFunc
      }
   in
      applyRewrite (main, functions,
                    funcBfs)
   end

in
   Program.T {datatypes = datatypes,
              globals = doStatements globals,
              functions = rewriteFuncs(),
              main = main}
end

type visitor = {
   foreachStatements: Statement.t vector -> unit,
   foreachArgs: (Var.t * Type.t) vector -> unit,
   foreachTransfer: Transfer.t -> unit
}

fun foreachBfs (v: visitor) (p: Program.t): unit = let
   val {foreachStatements, foreachArgs, foreachTransfer} = v
   fun ignore f x = (f x; x)
   val r: rewriter = {
      doStatements = ignore foreachStatements,
      doArgs = ignore foreachArgs,
      doTransfer = fn (_, transfer) => (foreachTransfer transfer; transfer)
   }
   val _ = rewriteBfs r p
in
   ()
end
end
