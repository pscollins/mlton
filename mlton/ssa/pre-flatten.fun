functor PreFlatten (S: SSA_TRANSFORM_STRUCTS): PRE_FLATTEN =
struct
open S
structure FlattenUtil = FlattenUtil (S)
open FlattenUtil

type walker = {
   (* Hook to execute before visiting a function body *)
   beforeFunc: Function.t -> unit,
   (* Hook to execute after visiting a function body *)
   afterFunc: Function.t -> unit,
   (* Hook to execute before visiting a block body *)
   beforeBlock: Block.t -> unit,
   (* Hook to execute after visiting a block body *)
   afterBlock: Block.t -> unit,
   (* Hook to execute when visiting a statement *)
   statement: Statement.t -> unit
}

val defaultWalker: walker = let
   fun noop (x: 'a) = ()
   (* Work around type system restriction *)
   val noopF: Function.t -> unit = noop
   val noopB: Block.t -> unit = noop
   val noopS: Statement.t -> unit = noop
in
   {beforeFunc = noopF,
    afterFunc = noopF,
    beforeBlock = noopB,
    afterBlock = noopB,
    statement = noopS}
end

fun statementWalker (statementF: Statement.t -> unit): walker = let
   val {beforeFunc, afterFunc, beforeBlock, afterBlock,
        ...} = defaultWalker
in
   {beforeFunc = beforeFunc,
    afterFunc = afterFunc,
    beforeBlock = beforeBlock,
    afterBlock = afterBlock,
    statement = statementF}
end

fun beforeFunctionWalker (beforeFunc: Function.t -> unit): walker = let
   val {afterFunc, beforeBlock, afterBlock, statement, ...} = defaultWalker
in
   {beforeFunc = beforeFunc,
    afterFunc = afterFunc,
    beforeBlock = beforeBlock,
    afterBlock = afterBlock,
    statement = statement}
end


fun doWalk (w: walker, p: Program.t) = let
   val {beforeFunc, afterFunc, beforeBlock, afterBlock, statement}
       = w
   val Program.T {globals, ...} = p
   fun noop() = ()
   fun doWalkBlock (b: Block.t) = let
      val _ = beforeBlock b
      val _ = Vector.foreach (Block.statements b, statement)
      val _ = afterBlock b
   in
      noop
   end
   fun doWalkFunc (f: Function.t) = let
      val _ = beforeFunc f
      val _ = Function.dfs (f, doWalkBlock)
      val _ = afterFunc f
   in
      noop
   end
in
   Vector.foreach (globals, statement);
   Program.dfs (p, doWalkFunc)
end


fun mapBlocksInFuncs (fs: Function.t list,
                      blockF: (Block.t -> Block.t option)): Function.t list = let
   fun doFunc (f: Function.t) = let
      val oldBlocks = Function.blocks f
      val maybeNewBlocks = Vector.map (oldBlocks, blockF)
      val allNone = Vector.forall (maybeNewBlocks, Option.isNone)
      fun buildNewF() = let
         fun selectBlock (oldBlock: Block.t, maybeNewBlock: Block.t option) =
             case maybeNewBlock of
                 SOME newBlock => newBlock
               | NONE => oldBlock
         val {args, mayInline, name, raises, returns, start, ...} =
             Function.dest f
      in
         Function.new {
            args = args,
            blocks = Vector.map2 (oldBlocks, maybeNewBlocks, selectBlock),
            mayInline = mayInline,
            name = name,
            raises = raises,
            returns = returns,
            start = start
         }
      end
   in
      if allNone then f
      else buildNewF()
   end
in
   List.map (fs, doFunc)
end

(* If `blockF` returns SOME for any `Block.t` in the `Function.t`, rewrites
   the `Function.t` to point to the modified block list. Otherwise, returns the
   original `Function.t` *)
fun mapBlocks (p: Program.t, blockF: (Block.t -> Block.t option)) = let
   val Program.T {datatypes, functions, globals, main} = p
in
   Program.T {datatypes = datatypes,
              functions = mapBlocksInFuncs (functions, blockF),
              globals = globals,
              main = main}
end

fun foldTransformation (steps: 'a list,
                        stepF: ('a * 'b -> 'b option),
                        init: 'b): 'b option = let
   val progress = ref false
   fun takeLhs (l, r) =
       case (l, r) of
           (SOME l', _) => (progress := true; l')
         | (NONE, _) => r
   fun apply (ss: 'a list, curr: 'b) =
       case ss of
           s::ss' => apply (ss', takeLhs (stepF (s, curr), curr))
        | [] => curr
   val result = apply (steps, init)
in
   if !progress then (SOME result)
   else NONE
end

(* Applies an effectful expression to each `Statment.t` in `p` *)
fun foreachStatement (p: Program.t, statementF: (Statement.t -> unit)): unit = let
   val Program.T {globals, ...} = p
   fun doBlock b = Vector.foreach (Block.statements b, statementF)
   fun doFunc f = Vector.foreach (Function.blocks f, doBlock)
   val _ = Vector.foreach (globals, statementF)
in
   foreachFunction (p, doFunc)
end


type typedVar = Var.t * Type.t

fun flattenTupleVar (var, ty): (typedVar vector) option = let
   fun buildVar (t: Type.t): typedVar = (Var.newString "flattenedVar", t)
   fun buildVars (ts: Type.t vector): typedVar vector =
       Vector.map (ts, buildVar)
in
   Option.map (Type.deTupleOpt ty, buildVars)
end

fun flattenConVar ((var, ty), argTys): (typedVar vector) option = let
   fun buildVar (t: Type.t): typedVar = (Var.newString "flattenedCon", t)
in
   case Type.dest ty of
       (* TODO: more validation? *)
       Type.Datatype _ =>
       SOME (Vector.map (argTys, buildVar))
     | _ => NONE
end


datatype bind = BindTuple of {to: typedVar, froms: Var.t vector}
              | BindCon of {to: typedVar, froms: Var.t vector, con: Con.t}

fun buildBindBlock (binds: bind vector, goto: Label.t): Block.t = let
   fun mkStmt (bind): Statement.t =
       case bind of
           BindTuple {to=(toVar, toType), froms=froms} =>
           Statement.T {exp = Exp.Tuple froms,
                        ty = toType,
                        var = SOME toVar}
        | BindCon {to=(toVar, toType), froms, con} =>
           Statement.T {exp = Exp.ConApp {args=froms, con=con},
                        ty = toType,
                        var = SOME toVar}

   (* We'll rely on the fact that the first block in a function can't have
      args  *)
   val gotoTransfer = Transfer.Goto {args = Vector.new0(), dst = goto}
in
   Block.T {args = Vector.new0(),
            label = Label.newString "forBind",
            statements = Vector.map (binds, mkStmt),
            transfer = gotoTransfer}
end

datatype argChoice =
            Preserve
          | FlattenTuple
          | FlattenCon of {argTys: Type.t vector, con: Con.t}

fun newFuncNamedLike (name: Func.t, suffix) = let
   val currName = Func.toString name
in
   Func.newString (concat [currName, "_", suffix])
end

fun newLabelNamedLike (name: Label.t, suffix) = let
   val currName = Label.toString name
in
   Label.newString (concat [currName, "_", suffix])
end

(* Implementation detail of `buildFlattenedFunction` + `buildFlattenedBlock`

   Applies `choices` to `args` and returns:

     * The new `args` for the flattened function
     * A `Block.t` that binds `newArgs` to `args` and then jumps to `goto`
 *)
fun applyChoicesToArgs (args: (Var.t * Type.t) vector,
                        goto: Label.t,
                        choices: argChoice vector):
    ((Var.t * Type.t) vector * Block.t)
    = let
   val needBinds: (bind list) ref = ref []
   fun addBind (bind) =
      List.push (needBinds, bind)
   fun extractVar (var, ty) = var
  (* Adds the 'reverse binding' `tupleVar = tuple(flattendVars)`
     to `needBinds` and returns `flattendVars` *)
   fun addFlattenedTupleBind (tupleVar, flattenedVars) = let
      val bind = BindTuple {to = tupleVar,
                            froms = Vector.map (flattenedVars, extractVar)}
      val _ = addBind bind
   in
      flattenedVars
   end
   (* Adds the 'reverse binding' `conVar = con (flattendVars)`
     to `needBinds` and returns `flattendVars` *)
   fun addFlattenedConBind (conVar, con, flattenedVars) = let
      val bind = BindCon {to = conVar,
                          froms = Vector.map (flattenedVars, extractVar),
                          con = con}
      val _ = addBind bind
   in
      flattenedVars
   end

   fun doFlattenTuple typedVar =
       case flattenTupleVar typedVar of
           SOME flattenedVars => addFlattenedTupleBind (typedVar, flattenedVars)
         | NONE => Error.bug "Tried to flatten non-tuple type!"
   fun doFlattenCon (typedVar: typedVar, {argTys: Type.t vector,
                                          con: Con.t}) =
       case flattenConVar (typedVar, argTys) of
           SOME flattenedVars => addFlattenedConBind (typedVar, con,
                                                      flattenedVars)
         | NONE => Error.bug "Tried to flaten non-con type!"
   fun applyChoice (typedVar, choice): typedVar vector =
       case choice of
           Preserve => Vector.new1 typedVar
         | FlattenTuple => doFlattenTuple typedVar
         | FlattenCon conInfo => doFlattenCon (typedVar, conInfo)
   val newArgs = Vector.concatV (Vector.map2 (args, choices, applyChoice))
   fun buildNewBlock binds =
      buildBindBlock (Vector.fromList binds, goto)
in
   case !needBinds of
       [] => Error.bug "No-op flattening decision"
    | binds => (newArgs, buildNewBlock binds)
end

fun buildFlattenedFunction (f: Function.t, choices: argChoice vector) = let
   val {args, blocks, mayInline, name, returns, raises, start} =
       (* Use the same variables in the clone: we'll `alphaRename` inside
       `transformOnce`, but this simplifies handling recursive flattening *)
       Function.dest f
   val (newArgs, bindBlock) = applyChoicesToArgs (args, start, choices)
in
   Function.new {args = newArgs,
                 blocks = Vector.concat [Vector.new1 bindBlock, blocks],
                 mayInline = mayInline,
                 name = newFuncNamedLike (name, "flat"),
                 raises = raises,
                 returns = returns,
                 start = Block.label bindBlock}
end

fun buildFlattenedBlock (b, choices) = let
   val Block.T {args, label, statements, transfer} = b
   (* TODO: need to rename?  *)
   val (newArgs, bindBlock) = applyChoicesToArgs (args,
                                                  Label.newString "dummyTemp",
                                                  choices)
in
   Block.T {args = newArgs,
            label = newLabelNamedLike (label, "flat"),
            statements = Vector.concat [Block.statements bindBlock, statements],
            transfer = transfer}
end

datatype flatteningChoiceType =
           NoOp
         | Valid
         | Invalid

fun checkArgsFlatteningChoice (args: (Var.t * Type.t) vector, choices: argChoice vector) = let
   fun checkFlatten (typedVar) =
       case flattenTupleVar typedVar of
           SOME _ => true
         | NONE => false
   fun checkChoice (typedVar, choice) =
       case choice of
           Preserve => true
         | FlattenTuple => checkFlatten (typedVar)
         | FlattenCon {argTys, ...} =>
           Option.isSome (flattenConVar (typedVar, argTys))
   fun isPreserve c =
       case c of
           Preserve => true
         | _ => false
   val isNoop = Vector.forall (choices, isPreserve)
   val validChoice = if isNoop then NoOp else Valid
in
   if (Vector.length args) = (Vector.length choices) andalso
      Vector.forall2 (args, choices, checkChoice) then
      validChoice
   else
      Invalid
end

fun checkFlatteningChoice (f: Function.t, choices: argChoice vector) = let
   val {args, ...} = Function.dest f
in
   checkArgsFlatteningChoice (args, choices)
end

fun checkBlockFlatteningChoice (b: Block.t, choices: argChoice vector) = let
   val Block.T {args, ...} = b
in
   checkArgsFlatteningChoice (args, choices)
end

datatype varChoice =
         PreserveVar
         | FlattenTupleVar of Var.t vector
         | FlattenConVar of {args: (Var.t * Type.t) vector, con: Con.t}

fun varChoiceLayout vc =
   case vc of
      PreserveVar => Layout.str "PreserveVar"
    | FlattenTupleVar vs =>
      Layout.seq [Layout.str "FlattenTupleVar ", Vector.layout Var.layout vs]
    | FlattenConVar {args, con} =>
      Layout.seq [Layout.str "FlattenConVar ",
           Layout.record [("con", Con.layout con),
                   ("args", Vector.layout (fn (v, _) => Var.layout v) args)]]

type varChoiceManager = {
   getVarChoiceProp: Var.t -> varChoice,
   setVarChoiceProp: Var.t * varChoice -> unit,
   getVarTypeProp: Var.t -> Type.t,
   setVarTypeProp: Var.t * Type.t -> unit,
   destroyVarChoiceManagerProps: unit -> unit
}

fun newVarChoiceManager () = let
   (* TODO(pscollins): Consider making "missing" into an error *)
   val {get=getChoice, set=setChoice, destroy=destroyChoice} =
       Property.destGetSetOnce (Var.plist,
                                Property.initConst PreserveVar)
   val {get=getType, set=setType, destroy=destroyType} =
       Property.destGetSetOnce (Var.plist,
                                Property.initRaise ("type lookup", Var.layout))
   fun destroyProps () =
       (destroyChoice(); destroyType())
in
   {getVarChoiceProp = getChoice,
    setVarChoiceProp = setChoice,
    getVarTypeProp = getType,
    setVarTypeProp = setType,
    destroyVarChoiceManagerProps = destroyProps}
end

fun chooseVarsInStatement (vt: varChoiceManager, s: Statement.t) = let
   val {setVarChoiceProp, getVarTypeProp, ...} = vt
   val Statement.T {exp, ty, var=maybeVar} = s
   fun buildLogStmt args =
       Layout.seq ([Layout.str "chooseVarsInStatement: for s=",
                    Statement.layout s,
                    Layout.str " made decision: "] @
                   args)
   fun logNonTupleResultThunk() =
       buildLogStmt ([Layout.str " do not flatten: not a tuple, or no dest"])
   fun addType v =
       (v, getVarTypeProp v)
   fun getTupleDecisionFromParents (parents: Var.t vector) = let
      fun logResultThunk() =
          buildLogStmt ([Layout.str " flatten tuple unless empty: ",
                         Vector.layout Var.layout parents])
      val _ = Control.diagnostic logResultThunk
   in
       (* `unit` is represented by an empty tuple that we don't want to flatten
       through *)
       if Vector.isEmpty parents then
          PreserveVar
       else
          FlattenTupleVar parents
   end
   fun getFlattenConFromParents (argcon as {args, con}) = let
      fun logResultThunk() =
          buildLogStmt ([Layout.str " flatten datatype: parents=(",
                         Vector.layout Var.layout args,
                         Layout.str "), con=",
                         Con.layout con])
      val _ = Control.diagnostic logResultThunk
   in
      FlattenConVar {args=Vector.map (args, addType), con=con}
   end
in
   case (exp, maybeVar) of
       (Exp.Tuple parents, SOME var) =>
       setVarChoiceProp (var, getTupleDecisionFromParents parents)
    | (Exp.ConApp argcon, SOME var) =>
       setVarChoiceProp (var, getFlattenConFromParents argcon)
    | _ => Control.diagnostic logNonTupleResultThunk
end

fun getVarChoice (vt: varChoiceManager, v: Var.t) = let
   val {getVarChoiceProp, ...} = vt
in
   getVarChoiceProp v
end

fun destroyVarChoiceManager (vt: varChoiceManager) = let
   val {destroyVarChoiceManagerProps, ...} = vt
in
   destroyVarChoiceManagerProps()
end

fun markTypeForBinding (vt: varChoiceManager, s: Statement.t) = let
   val {setVarTypeProp, ...} = vt
   val Statement.T {ty, var, ...} = s
in
   case var of
       SOME var => setVarTypeProp (var, ty)
     | _ => ()
end

fun markTypeForArgs (vt: varChoiceManager, func: Function.t) = let
   val {setVarTypeProp, ...} = vt
   val {args, ...} = Function.dest func
   fun doBlock b = Vector.foreach (Block.args b, setVarTypeProp)
in
   (Vector.foreach (args, setVarTypeProp);
    Vector.foreach (Function.blocks func, doBlock))
end


fun newVarChoicesForProgram (p: Program.t) = let
   val vcm = newVarChoiceManager ()
   fun markStatement (s: Statement.t) =
       markTypeForBinding (vcm, s)
   fun markFunction (f: Function.t) =
       markTypeForArgs (vcm, f)
   fun chooseStatement (s: Statement.t) =
       chooseVarsInStatement (vcm, s)
   (* First, record the types associated with every bound variable. Don't use
   the DFS order since it can miss statements. *)
   val _ = foreachFunction (p, markFunction)
   val _ = foreachStatement (p, markStatement)
   (* Next, in a separate pass, make the flattening choice. We do this in two
   passes since the DFS order might not guarantee us that we visit every def
   before its use. *)
   val _ = doWalk (statementWalker chooseStatement, p)
in
   vcm
end

datatype varConsumer =
            AsUnpacked
            | AsCurrent
            | AsAlias of Var.t

fun varConsumerLayout vc =
   case vc of
      AsUnpacked => Layout.str "AsUnpacked"
    | AsCurrent => Layout.str "AsCurrent"
    | AsAlias v => Layout.seq [Layout.str "AsAlias ", Var.layout v]

type varConsumerManager = {
   getVarConsumersProp: Var.t -> varConsumer list ref,
   destroyVarConsumersProps: unit -> unit,
   funcsMap: funcsMap
}


fun newVarConsumerManager (p: Program.t) = let
   fun newConsumers _ = ref []
   val {get=getConsumers, destroy=destroyConsumers, ...} =
       Property.destGetSetOnce (Var.plist,
                                Property.initFun newConsumers)
in
   {getVarConsumersProp = getConsumers,
    destroyVarConsumersProps = destroyConsumers,
    funcsMap = newFuncsMap p}
end

fun getVarConsumers (vm: varConsumerManager, v: Var.t) = let
   val {getVarConsumersProp, ...} = vm
in
   !(getVarConsumersProp v)
end

fun varConsumerToString varConsumer =
   case varConsumer of
      AsUnpacked => "AsUnpacked"
    | AsCurrent => "AsCurrent"
    | AsAlias v => concat ["AsAlias(", Var.toString v, ")"]

(* Add each `varConsumer` in `s` to `vm`

  * `_ := Select(..., v)` -> AsUnpacked
  * `_ := {ConApp,PrimApp,Tuple}(...v...)` -> AsCurrent
  * `v := Var(v')` -> AsAlias(v')
 *)
fun markConsumersInStatement (vm: varConsumerManager, s: Statement.t) = let
   val {getVarConsumersProp, ...} = vm
   val Statement.T {exp, var=maybeLhs, ...} = s
   fun buildLogStmtThunk (var, decision) = let
      fun thunk () =
          Layout.seq ([Layout.str "markConsumersInStatement: for s=",
                       Statement.layout s,
                       Layout.str " with var=",
                       Var.layout var,
                       Layout.str " added consumer ",
                       Layout.str (varConsumerToString decision)])
   in
      thunk
   end
   fun addConsumer consumer v = let
      val _ = Control.diagnostic (buildLogStmtThunk (v, consumer))
      val consumersRef = getVarConsumersProp v
   in
      List.push (consumersRef, consumer)
   end
   fun addConsumerTo consumer (vs: Var.t vector): unit =
       Vector.foreach (vs, addConsumer consumer)
   val addCurrentConsumer = addConsumerTo AsCurrent
   fun addAliasOf rhs =
      case maybeLhs of
          SOME lhs => addConsumer (AsAlias (lhs)) rhs
        | NONE =>  Error.bug ("Bad alias" ^
                              (Layout.toString (Statement.layout s)))
in
   case exp of
       Exp.ConApp {args, ...} => addCurrentConsumer args
     | Exp.PrimApp  {args, ...} => addCurrentConsumer args
     | Exp.Select {tuple, ...} => addConsumer AsUnpacked tuple
     | Exp.Tuple args => addCurrentConsumer args
     (* This shows up in an example program as

          global_2 := global_1

        and all later references are only to `global_2`, not `global_1`, so we
        have the alias "point upwards" from the new name to the old name, i.e.

          rhs := AsAlias(lhs)
      *)
     | Exp.Var rhs => addAliasOf rhs
     | _ => ()
end

(* Extract all of the `Return(...)`s from `f` *)
fun extractReturns (f: Function.t): (Var.t vector) vector = let
   fun extractInBlock b: (Var.t vector) option =
       case Block.transfer b of
           Transfer.Return rets => SOME rets
         | _ => NONE

in
   Vector.keepAllMap (Function.blocks f, extractInBlock)
end

(* Records the "same-layout-as" relationship induced by function calls and
   similar constructs, i.e. if we have a function definition `f(arg1)`, then the
   call `f(x)` means that `x` is consumed in the same layout as `arg1`.

   * `Goto(args, "label")` + `Block(args', "label", ...)` ->
      args[i] = AsAlias(args'[i])

   * `Call(args, "func", ...)` + `Function(args', "func")` ->
      args[i] = AsAlias(args'[i])

   * `Call(..., "callee", Return.NonTail {"block"}` + `Block(args, "block")` +
     `Function(..., "callee", ...) {
        ...
        Return (vs1)
        ...
        Return (vs2)
        ...
     }` ->
     vs1[i] = vs2[i] = AsAlias(args[i])


  * `Case(..., test="x")` ->
    x = AsUnpacked
 *)
fun markConsumersInTransfer (vm: varConsumerManager, transfer: Transfer.t) = let
   val {getVarConsumersProp, funcsMap, ...} = vm
   val {getFunc, getBlock, ...} = funcsMap
   fun logInputThunk () =
       Layout.seq [Layout.str "markConsumersInTransfer: ",
                   Transfer.layout transfer]
   val _ = Control.diagnostic logInputThunk
   fun markAsUnpacked from = let
      val consumersRef = getVarConsumersProp from
   in
      List.push (consumersRef, AsUnpacked)
   end
   fun markConsumer (from, to) = let
      val consumersRef = getVarConsumersProp from
   in
      List.push (consumersRef, AsAlias to)
   end
   fun markConsumers (froms, tos) =
       Vector.foreach2 (froms, tos, markConsumer)
   fun extractVar (var, _) = var
   fun extractVars typedVars = Vector.map (typedVars, extractVar)
   fun getBlockArgs (l: Label.t): Var.t vector = let
      val Block.T {args, ...} = getBlock l
   in
      extractVars args
   end
   fun getFuncArgs funcLabel = let
      val {args, ...} = Function.dest (getFunc funcLabel)
   in
      extractVars args
   end
   fun bindArgs (args, funcLabel) =
      markConsumers (args, getFuncArgs funcLabel)
   fun getAllReturns funcLabel =
       extractReturns (getFunc funcLabel)
   fun bindRetsToBlock (func, returnToLabel) = let
      val returnToBlockArgs = getBlockArgs returnToLabel
      val allReturns: (Var.t vector) vector =
          getAllReturns func
      fun bindReturns returns =
          markConsumers (returns, returnToBlockArgs)
   in
      Vector.foreach (allReturns, bindReturns)
   end
   fun maybeBindRets (func, return) =
       case return of
           Return.NonTail {cont=label, ...} =>
           bindRetsToBlock (func, label)
         (* `Dead` and `Tail` do not bind *)
         | _ => ()
in
   case transfer of
       Transfer.Goto {args, dst} =>
       markConsumers (args, getBlockArgs dst)
     | Transfer.Call {args, func, return, ...} =>
       (bindArgs (args, func);
        maybeBindRets (func, return))
     | Transfer.Case {test, ...} => markAsUnpacked test
     (* `Return` does not bind (it is handled in `Call`)
       `Raise` does not bind
        TODO(pscollins): Spork/spoin?
     *)
     | _ => ()
end


fun destroyVarConsumerManager (vc: varConsumerManager) = let
   val {destroyVarConsumersProps, funcsMap, ...} = vc
   val {destroyFuncsMap, ...} = funcsMap
in
   destroyVarConsumersProps();
   destroyFuncsMap()
end

fun newVarConsumersForProgram (p: Program.t): varConsumerManager = let
   val {beforeFunc, afterFunc, beforeBlock, ...} = defaultWalker 
   val vm = newVarConsumerManager p
   fun doBlock b = markConsumersInTransfer (vm, Block.transfer b)
   fun doStatement s = markConsumersInStatement (vm, s)
   val walker = {beforeFunc = beforeFunc,
                 afterFunc = afterFunc,
                 beforeBlock = beforeBlock,
                 afterBlock = doBlock,
                 statement = doStatement}
   val _ = doWalk (walker, p)
in
   vm
end

fun newVarConsumerManagerFromAssignments (assignments) = let
   val dummyLabel = Func.newString "dummy"
   val emptyProgram =
      Program.T {datatypes=Vector.new0(),
                 functions=[],
                 globals=Vector.new0(),
                 main=dummyLabel}
   val vm = newVarConsumerManager emptyProgram
   val {getVarConsumersProp, ...} = vm
   fun doAssignment (v, consumers) =
       getVarConsumersProp v := consumers
   val _ = List.foreach (assignments, doAssignment)
in
   vm
end

datatype varAliasPolicy =
         DropAlias
         | UnionAlias
fun resolveAliases (policy: varAliasPolicy, vc: varConsumerManager)
                   (consumers: varConsumer list) = let
   val {getVarConsumersProp, ...} = vc
   fun mkVisitedProp _ = ref false
   val {get=getVisitedProp, destroy=destroyVisitedProp, ...} =
       Property.destGetSetOnce (Var.plist,
                                Property.initFun mkVisitedProp)
   (* Marks v' as visited and returns previous state *)
   fun markSeen v' = let
      val seen = getVisitedProp v'
      val wasSeen = !seen
      val _ = seen := true
   in
      wasSeen
   end
   fun doResolve v = let
      val found = ref []
      fun visitConsumer consumer =
          case consumer of
              AsAlias v' => resolve v'
           | _ => List.push (found, consumer)
      and resolve v' =
          if markSeen v' then ()
          else
             List.foreach (!(getVarConsumersProp v'),
                           visitConsumer)
      val _ = resolve v
   in
      !found
   end
   fun processConsumer consumer =
       case (policy, consumer) of
           (DropAlias, AsAlias _) => []
         | (UnionAlias, AsAlias v) => doResolve v
         | _ => [consumer]
   val result =
       List.concatMap (consumers, processConsumer)
   val _ = destroyVisitedProp()
in
   result
end

type functionManager = {
   getOrCreateFlattenedFunc: (Func.t * argChoice vector) -> Func.t,
   pendingFuncs: Function.t list ref,
   destroyFunctionManagerState: unit -> unit
}

fun choiceLayout c =
   case c of
      Preserve => Layout.str "Preserve"
    | FlattenTuple => Layout.str "FlattenTuple"
    | FlattenCon {argTys, con}  =>
      Layout.seq [
         Layout.str "FlattenCon",
         Layout.record [
            ("con", Con.layout con),
            ("argTys", Vector.layout Type.layout argTys)
         ]
      ]

fun choiceEqual (l, r) = let
   fun compareArg ({argTys, con},
                   {argTys=argTys', con=con'}) =
       (Con.equals (con, con')) andalso
       Vector.equals (argTys, argTys', Type.equals)
in
    case (l, r) of
        (Preserve, Preserve) => true
      | (FlattenTuple, FlattenTuple) => true
      | (FlattenCon arg, FlattenCon arg') =>
        compareArg (arg, arg')
      | _ => false
end

fun buildPending() = let
   val pending = ref []
   fun doAppend x = List.push (pending, x)
in
   (pending, doAppend)
end

fun newFunctionManager (p: Program.t) = let
   (* TODO(pscollins): Since the scheme below doesn't 'follow through'
   already-flattened functions, we'll need to destroy and recreate it after each
   iteration of flattening, which will result in unnecessary flattened
   functions. Optimize in the future. *)

   val (pendingFuncs: Function.t list ref, appendFunc) = buildPending()

   (* First, collect Func.t -> Function mappings *)
   val {getFunc, destroyFuncsMap, ...} = newFuncsMap p

   (* Next, set up a hook to create new flattened functions when necessary

   For simplicity, we attach a list of `(flattening choice, func name)` to each
   unflattened function.

   TODO(pscollins): Optimize this representation. *)
   type flattenedFunc = (argChoice vector * Func.t)
   fun createNewFlattenedFuncList (_): flattenedFunc list ref = ref []

   val {get=getFlattenedFuncList, destroy=destroyFlattenedFuncs,
        ...} = Property.destGetSetOnce
                   (Func.plist,
                    Property.initFun createNewFlattenedFuncList)

   (* Creates a new flattened function for the specified choice *)
   fun createFlattenedFunc
           (originalName: Func.t, choices: argChoice vector): Function.t = let
      val original = getFunc originalName
      fun doBuildFlattenedFunction() = let
         val flattenedFunction = buildFlattenedFunction (original,
                                                         choices)
         val _ = appendFunc flattenedFunction
      in
         flattenedFunction
      end
   in
      case checkFlatteningChoice (original, choices) of
       NoOp => original
     | Valid => doBuildFlattenedFunction()
     | Invalid => Error.bug "Invalid flattening decision"
   end

   (* If we already have a flattened version of `f` for `choice`, returns it.
   Otherwise, builds a flattened function for `f` under `choice` and adds it to
   the list of for `f`. *)
   fun getOrCreateFlattenedFunc (f: Func.t, choices: argChoice vector): Func.t = let
      fun logInputThunk () = let
         open Layout
      in
         seq [str "getOrCreateFlattenedFunc: looking for ",
              Func.layout f,
              Layout.str " with choices ",
              Vector.layout choiceLayout choices]
      end
      fun doLogChoice (newF) = let
         open Layout
      in
         seq [str "getOrCreateFlattenedFunc: created new function ",
              Func.layout newF,
              str " from ",
              Func.layout f]
      end
      val _ = Control.diagnostic logInputThunk
      val flattenedFuncList: flattenedFunc list ref = getFlattenedFuncList f
      fun flattenedFuncMatches (choices', _) =
          Vector.equals (choices', choices,
                         choiceEqual)
      fun addNewFlattenedFunc () = let
         val newFunc = createFlattenedFunc (f, choices)
         val newF = Function.name newFunc
         val _ = Control.diagnostic (fn () => doLogChoice newF)
         val _ = List.push (flattenedFuncList, (choices, newF))
      in
         newF
      end
   in
      case List.peek (!flattenedFuncList,
                      flattenedFuncMatches)  of
          (* If we already have a flattened function for `choices`, return it
             here *)
          SOME (_, matchedFunc) => (
             Control.diagnostic (fn () => let open Layout in
                seq [str "getOrCreateFlattenedFunc: found matched function ",
                     Func.layout matchedFunc]
             end);
             matchedFunc
          )
        (* Otherwise, build a new one *)
        | _ => addNewFlattenedFunc()
   end

   fun destroyFunctionManagerState() = let
      val _ = destroyFlattenedFuncs()
      val _ = destroyFuncsMap()
   in
      ()
   end
in
   {getOrCreateFlattenedFunc = getOrCreateFlattenedFunc,
    pendingFuncs = pendingFuncs,
    destroyFunctionManagerState = destroyFunctionManagerState}
end

fun getOrCreateFunc
        (fm: functionManager,
         f: Func.t, choices: argChoice vector) = let
   val {getOrCreateFlattenedFunc, ...} = fm
in
   getOrCreateFlattenedFunc (f, choices)
end

fun extractList (xs: 'a list ref) = let
   val curr = !xs
   val _ = xs := []
in
   curr
end

fun extractNewFunctions (fm: functionManager) = let
   val {pendingFuncs, ...} = fm
in
   extractList pendingFuncs
end

fun validateAndDestroy {pending, doDestroy, name} =
    case !pending of
        [] => doDestroy()
      | _ => Error.bug ("Tried to destroy nonempty " ^ name)

fun destroyFunctionManager (fm: functionManager) = let
   val {pendingFuncs, destroyFunctionManagerState, ...} = fm
in
   validateAndDestroy {pending = pendingFuncs,
                       doDestroy = destroyFunctionManagerState,
                       name = "functionManager"}
end

type blockManager = {
   getOrCreateFlattenedBlock: (Label.t * argChoice vector) -> Label.t,
   pendingBlocks: Block.t list ref,
   destroyBlockManagerState: unit -> unit
}

fun newBlockManager (f: Function.t) = let
   (* TODO(pscollins): Deduplicate with `newFunctionManager` *)

   val (pendingBlocks: Block.t list ref, appendBlock) = buildPending()
   (* First, collect Label.t -> Block mappings

   Wrap `f` in a dummy program for easier reuse
    *)
   val {getBlock, destroyFuncsMap, ...} =
       newFuncsMap
           (Program.T {datatypes = Vector.new0(),
                       functions = [f],
                       globals = Vector.new0(),
                       main = Func.newString "dummyTemp"})

   (* Next, set up a hook to create new flattened blocks when necessary

   For simplicity, we attach a list of `(flattening choice, block name)` to each
   unflattened block.

   TODO(pscollins): Optimize this representation. *)
   type flattenedBlock = (argChoice vector * Label.t)
   fun createNewFlattenedBlockList (_): flattenedBlock list ref = ref []

   val {get=getFlattenedBlockList, destroy=destroyFlattenedBlocks,
        ...} = Property.destGetSetOnce
                   (Label.plist,
                    Property.initFun createNewFlattenedBlockList)

   (* Creates a new flattened block for the specified choice *)
   fun createFlattenedBlock
           (originalName: Label.t, choices: argChoice vector): Block.t = let
      val original = getBlock originalName
      fun doBuildFlattenedBlock() = let
         val flattenedBlock = buildFlattenedBlock (original,
                                                         choices)
         val _ = appendBlock flattenedBlock
      in
         flattenedBlock
      end
   in
      case checkBlockFlatteningChoice (original, choices) of
       NoOp => original
     | Valid => doBuildFlattenedBlock()
     | Invalid => Error.bug "Invalid flattening decision"
   end

   (* If we already have a flattened version of `b` for `choice`, returns it.
   Otherwise, builds a flattened function for `b` under `choice` and adds it to
   the list of for `b`. *)
   fun getOrCreateFlattenedBlock (b: Label.t, choices: argChoice vector): Label.t = let
      fun logInputThunk () = let
         open Layout
      in
         seq [str "getOrCreateFlattenedBlock: looking for ",
              Label.layout b,
              Layout.str " with choices ",
              Vector.layout choiceLayout choices]
      end
      fun doLogChoice (newB) = let
         open Layout
      in
         seq [str "getOrCreateFlattenedBlock: created new block ",
              Label.layout newB,
              str " from ",
              Label.layout b]
      end
      val _ = Control.diagnostic logInputThunk
      val flattenedBlockList: flattenedBlock list ref = getFlattenedBlockList b
      fun flattenedBlockMatches (choices', _) =
          Vector.equals (choices', choices,
                         choiceEqual)
      fun addNewFlattenedBlock () = let
         val newBlock = createFlattenedBlock (b, choices)
         val newB = Block.label newBlock
         val _ = Control.diagnostic (fn () => doLogChoice newB)
         val _ = List.push (flattenedBlockList, (choices, newB))
      in
         newB
      end
   in
      case List.peek (!flattenedBlockList,
                      flattenedBlockMatches)  of
          (* If we already have a flattened block for `choices`, return it
             here *)
          SOME (_, matchedBlock) => (
             Control.diagnostic (fn () => let open Layout in
                seq [str "getOrCreateFlattenedBlock: found matched block ",
                     Label.layout matchedBlock]
             end);
             matchedBlock
          )
        (* Otherwise, build a new one *)
        | _ => addNewFlattenedBlock()
   end

   fun destroyBlockManagerState() = let
      val _ = destroyFlattenedBlocks()
      val _ = destroyFuncsMap()
   in
      ()
   end
in
   {getOrCreateFlattenedBlock = getOrCreateFlattenedBlock,
    pendingBlocks = pendingBlocks,
    destroyBlockManagerState = destroyBlockManagerState}
end


fun getOrCreateBlock (bm: blockManager, l: Label.t,
                      choices: argChoice vector) = let
   val {getOrCreateFlattenedBlock, ...} = bm
in
   getOrCreateFlattenedBlock (l, choices)
end

fun extractNewBlocks (bm: blockManager) = let
   val {pendingBlocks, ...} = bm
in
   extractList pendingBlocks
end

fun destroyBlockManager (bm: blockManager) = let
   val {pendingBlocks, destroyBlockManagerState, ...} = bm
in
   validateAndDestroy {pending = pendingBlocks,
                       doDestroy = destroyBlockManagerState,
                       name = "blockManager"}
end


type blockManagerManager = {
   getBlockManagerForFuncProp: Func.t -> blockManager,
   getFuncForBlockProp: Label.t -> Func.t,
   destroyBlockManagerManagerData: unit -> unit
}

fun newBlockManagerManager (p: Program.t) = let
   val blockManagers = ref []
   val {get=getFuncForBlockProp, set=setFuncForBlockProp,
        destroy=destroyFuncForBlockProp} =
       Property.destGetSetOnce (Label.plist,
                                Property.initRaise ("containing func lookup",
                                                    Label.layout))
   val {get=getBlockManagerForFuncProp, set=setBlockManagerForFunc,
        destroy=destroyBlockManagerManagerProps} =
       Property.destGetSetOnce (Func.plist,
                                Property.initRaise ("blockManager lookup",
                                                    Func.layout))

   fun createBlockManagerForFunction (f: Function.t) = let
      val bm = newBlockManager f
      val _ = List.push (blockManagers, bm)
      fun setContainingFuncForBlock b =
          setFuncForBlockProp (Block.label b, Function.name f)
   in
      (setBlockManagerForFunc (Function.name f, bm);
       Vector.foreach (Function.blocks f, setContainingFuncForBlock))
   end

   val _ = foreachFunction (p, createBlockManagerForFunction)

   fun destroyBlockManagerManagerData() = let
      val _ = List.foreach (!blockManagers, destroyBlockManager)
      val _ = destroyBlockManagerManagerProps()
      val _ = destroyFuncForBlockProp()
   in
      ()
   end
in
   {getBlockManagerForFuncProp = getBlockManagerForFuncProp,
    getFuncForBlockProp = getFuncForBlockProp,
    destroyBlockManagerManagerData = destroyBlockManagerManagerData}
end

fun getBlockManagerForFunc (bmm: blockManagerManager, f: Func.t) = let
   val {getBlockManagerForFuncProp, ...} = bmm
in
   getBlockManagerForFuncProp f
end

fun getBlockManagerForBlock (bmm: blockManagerManager, l: Label.t) = let
   val {getFuncForBlockProp, ...} = bmm
in
    getBlockManagerForFunc (bmm, getFuncForBlockProp l)
end

fun destroyBlockManagerManager (bmm: blockManagerManager) = let
   val {destroyBlockManagerManagerData, ...} = bmm
in
   destroyBlockManagerManagerData()
end

datatype flatteningPolicy =
           FlattenAlways
         | FlattenForAnyUnpack
         | FlattenForAllUnpack

fun updateChoiceForPolicy policy (varChoice, varConsumers) = let
   fun isUnpacked consumer =
       case consumer of
           AsUnpacked => true
         | AsCurrent => false
         | AsAlias _ => Error.bug "Must filter AsAlias!"
   fun hasConsumerType wantType =
       List.exists (varConsumers, wantType)
   fun allConsumerType wantType =
       List.forall (varConsumers, wantType)
in
   case (policy, varChoice) of
       (FlattenAlways, _) => varChoice
     | (_, PreserveVar) => varChoice
     | (FlattenForAnyUnpack, _) =>
       if hasConsumerType isUnpacked then varChoice
       else PreserveVar
     | (FlattenForAllUnpack, _) =>
      if allConsumerType isUnpacked then varChoice
      else PreserveVar
     end

datatype flattenableTypesPolicy =
         FlattenAnyType
         | FlattenOnlyTuple
         | FlattenOnlyConApp

fun updateChoiceForAllowedTypes policy vc =
   case (policy, vc) of
       (FlattenOnlyTuple, FlattenConVar _) => PreserveVar
     | (FlattenOnlyConApp, FlattenTupleVar _) => PreserveVar
     | _ => vc

datatype transferFlatteningPolicy =
         FlattenAnyTransfer
       | FlattenOnlyTailCalls

fun updateChoiceForTransferPolicy (policy, transfer) vc =
    case (policy, transfer) of
        (FlattenAnyTransfer, _) => vc
      | (FlattenOnlyTailCalls, Transfer.Call {args, func,
                                              return=Return.Tail}) => vc
      | (FlattenOnlyTailCalls, _) => PreserveVar


fun varChoiceToArgChoice (vc: varChoice): argChoice = let
   fun extractTy (_, t) = t
in
   case vc of
       PreserveVar => Preserve
     | FlattenTupleVar _ => FlattenTuple
     | FlattenConVar {args, con} =>
       FlattenCon {argTys = Vector.map (args, extractTy),
                   con=con}
end


(* Given a flattening choice for the constituent vars of `originalArgs`, returns
the argument vector to pass to the flattened function *)
fun buildCallArgs (originalArgs: Var.t vector,
                   varChoices: varChoice vector) = let
   fun extractVar (v, _) = v
   fun buildCallArg (originalArg, varChoice) =
       case varChoice of
           PreserveVar => Vector.new1 originalArg
         | FlattenTupleVar parents => parents
         | FlattenConVar {args, ...} => Vector.map (args, extractVar)
in
   Vector.concatV (
   Vector.map2 (originalArgs, varChoices, buildCallArg))
end

datatype flattenLevel =
         blockOnly
         | functionOnly

fun flattenLevelToString l =
    case l of
        blockOnly => "blockOnly"
      | functionOnly => "functionOnly"

datatype recursiveFlattenPolicy =
            noRecursiveFlatten
            | recursiveFlattenSteps of int

fun recursiveFlattenPolicyToLayout p =
    case p of
        noRecursiveFlatten => Layout.str "noRecursiveFlatten"
      | recursiveFlattenSteps n =>
        Layout.seq [Layout.str "recursiveFlattenSteps ", Int.layout n]

fun flattenOnce (flattenPolicy, resolvePolicy,
                 allowedTypesPolicy, flattenLevel,
                 recursiveFlattenPolicy,
                 transferPolicy) (p: Program.t) = let
   (* TODO: support recursiveFlattenPolicy *)
   val vm = newVarChoicesForProgram p
   val vc = newVarConsumersForProgram p
   val fm = newFunctionManager p
   val bmm = newBlockManagerManager p
   val resolve = resolveAliases (resolvePolicy, vc)
   fun getChoice v = getVarChoice (vm, v)
   fun getConsumers v = resolve (getVarConsumers (vc, v))
   fun getFunc (original, argChoices) =
       getOrCreateFunc (fm, original, argChoices)
   fun getBlock (original, argChoices) = let
      val bm = getBlockManagerForBlock (bmm, original)
   in
      getOrCreateBlock (bm, original, argChoices)
   end
   val updateChoice = (updateChoiceForAllowedTypes allowedTypesPolicy)
                      o updateChoiceForPolicy flattenPolicy

   fun buildLogThunk (t, varChoices, varConsumers, varChoices') = let
      val name = concat ["rewriteTransfer (", flattenLevelToString flattenLevel,
                         "): "]
      fun thunk() = Layout.seq [
             Layout.str name, Transfer.layout t,
             Layout.indent (Layout.align
                                [Layout.seq [Layout.str "varChoices (before): ",
                                             Vector.layout varChoiceLayout varChoices],
                                 Layout.seq [Layout.str "varConsumers: ",
                                             Vector.layout
                                                 (List.layout varConsumerLayout)
                                                 varConsumers],
                                 Layout.seq [Layout.str "varChoices' (after): ",
                                             Vector.layout varChoiceLayout varChoices']],
                             3)]
   in
      thunk
   end

   fun rewriteTransfer (t: Transfer.t) = let
      val updateChoice' = updateChoiceForTransferPolicy (transferPolicy, t)
      fun buildTransferArgs args = let
         (* Make a flattening decision for each argument by collecting all of
         the tags for each concrete argument... *)
         val varChoices = Vector.map (args, getChoice)
         (* ...and the (resolved) usage info for each formal parameter... *)
         val varConsumers = Vector.map (args, getConsumers)
         (* ...and applying the policies *)
         val varChoices' = Vector.map (Vector.map2 (varChoices, varConsumers,
                                                    updateChoice),
                                       updateChoice')
         val _ = Control.diagnostic
                     (buildLogThunk (t, varChoices, varConsumers,
                                     varChoices'))
         (* Construct the call argument *)
         val args' = buildCallArgs (args, varChoices')
         (* Construct the flattening choice  *)
         val argChoices = Vector.map (varChoices', varChoiceToArgChoice)
      in
         (args', argChoices)
      end

      fun buildCall (args, func, return) = let
         (* Make the flattening decision *)
         val (args', argChoices) = buildTransferArgs args
         (* Build the flattened function *)
         val func' = getFunc (func, argChoices)
      in
         (* Return a call to the flattened function (perhaps unchanged) *)
         Transfer.Call {args=args',
                        func=func',
                        return=return}
      end

      fun buildGoto (args, dst) = let
         (* Make the flattening decision *)
         val (args', argChoices) = buildTransferArgs args
         (* Build the flattened block *)
         val dst' = getBlock (dst, argChoices)
      in
         (* Return a goto to the flattened block (perhaps unchanged) *)
         Transfer.Goto {args=args',
                        dst=dst'}
      end
   in
      case (t, flattenLevel) of
          (* For now, only Call is supported

           TODO(pscollins): Ideally we'd support goto-with-args as well
           *)
          (Transfer.Call {args, func, return}, functionOnly) =>
           SOME (buildCall (args, func, return))
        | (Transfer.Goto {args, dst}, blockOnly) =>
          SOME (buildGoto (args, dst))
        | _ => NONE
   end

   fun maybeRewriteBlock (b: Block.t): Block.t option = let
      val Block.T {args, label, statements, transfer} = b
   in
      case rewriteTransfer transfer of
          SOME transfer' => SOME (Block.T {args=args,
                                           label=label,
                                           statements=statements,
                                           transfer=transfer'})
        | NONE => NONE
   end

   (* Extracts new blocks from the appropriate `bm` and adds them to `f`, or
   else returns `NONE` *)
   fun maybeAppendNewBlocksForF (f: Function.t): Function.t option = let
      val bm = getBlockManagerForFunc (bmm, Function.name f)
      val blocks = extractNewBlocks bm
      fun buildNewFunc blockVec = let
         val {args, blocks, mayInline, name, raises, returns, start} =
             Function.dest f
      in
         Function.new {args = args,
          (* Append the new blocks (arbitrarily) at the end *)
          blocks = Vector.concat [blocks, blockVec],
          mayInline = mayInline,
          name = name,
          raises = raises,
          returns = returns,
          start = start}
      end
   in
      case blocks of
          [] => NONE
        | _ => SOME (buildNewFunc (Vector.fromList blocks))
   end

   (* Extracts new blocks from the `bmm` and adds them to the appropriate
   functions, or else returns `NONE` *)
   fun maybeAppendNewBlocks (p': Program.t): Program.t option = let
      val Program.T {datatypes, functions, globals, main} = p'
      val maybeNewFuncs = List.map (functions, maybeAppendNewBlocksForF)
      fun buildProgram() = let
         fun takeLhs (l: 'a option, r: 'a): 'a =
             case (l, r) of
                 (SOME l', _) => l'
               | (NONE, _) => r
         val newFuncs: Function.t list = List.map2 (maybeNewFuncs, functions, takeLhs)
      in
         Program.T {datatypes = datatypes,
                    functions = newFuncs,
                    globals = globals,
                    main = main}
      end
   in
      if List.forall (maybeNewFuncs, Option.isNone) then
         NONE
      else SOME (buildProgram())
   end

   (* Extracts pending functions and applies `recursiveFlattenPolicy` *)
   fun doExtractFunctions(): Function.t list = let
      fun buildLogThunk (state, cur) = let
         fun thunk() = Layout.seq [
                Layout.str "doExtractFunctions(): state=",
                recursiveFlattenPolicyToLayout state,
                Layout.str ", found count=",
                Layout.str (Int.toString (List.length cur))
             ]
      in
         thunk
      end

      fun apply newFns: Function.t list = mapBlocksInFuncs (newFns, maybeRewriteBlock)
      fun step (state: recursiveFlattenPolicy): Function.t list = let
         (* Grab newly produced functions from the previous iteration *)
         val cur: Function.t list = extractNewFunctions fm
         val _ = Control.diagnostic (buildLogThunk (state, cur))
      in
         case (List.isEmpty cur, state) of
             (* No recursive flatten requested: just return the new functions *)
             (_, noRecursiveFlatten) => cur
           (* Recursive flatten requested, but no new functions: we're done *)
           | (true, recursiveFlattenSteps _) => cur
           (* Recursive flatten finished, but new functions remain: error *)
           | (false, recursiveFlattenSteps 0) => Error.bug "Failed to converge"
           (* Recursive flatten steps remain: run the flattening transformation
           on the newly-produced functions and check for convergence on the next
           iteration *)
           | (false, recursiveFlattenSteps n) =>
             List.append (apply cur,
                          step (recursiveFlattenSteps (n-1)))
      end
   in

      (* After we've finished the recursive edits, we can apply `alphaRename` to
      the returned functions: we need fresh names in the final program, but
      waiting until this point means that we don't need to add the new functions
      to `vm`/`vc`/`fm` before doing this step *)
      List.map (step recursiveFlattenPolicy,
                Function.alphaRename)
   end

   (* Extracts new functions from `fm` and adds them to `p'`, or
      returns `NONE`

    *)
   fun maybeAppendNewFns (p': Program.t) = let
      val Program.T {datatypes, functions, globals, main} = p'
   in
      case doExtractFunctions() of
          [] => NONE
        | newFns => SOME (Program.T {datatypes=datatypes,
                                     functions=List.append (newFns,
                                                            functions),
                                     globals=globals,
                                     main = main})
   end
   (* Apply the transformation to all blocks *)
   val newP = mapBlocks (p, maybeRewriteBlock)
   (* Try to update the program by applying new functions *)
   val pNewFuncs = maybeAppendNewFns newP
   (* Try to update the program by applying new blocks *)
   val pNewBlocks = maybeAppendNewBlocks newP
   val _ = destroyFunctionManager fm
   val _ = destroyVarChoiceManager vm
   val _ = destroyVarConsumerManager vc
   val _ = destroyBlockManagerManager bmm
in
   case (pNewFuncs, pNewBlocks) of
       (SOME p', NONE) => SOME p'
    | (NONE, SOME p') => SOME p'
    | (NONE, NONE) => NONE
    | (SOME _, SOME _) => Error.bug "Can't add blocks and functions in one pass"
end

datatype postStep =
         postShrink
         | postFlatten

structure Flatten = Flatten (S)

fun doPostStep (step: postStep, p: Program.t) =
    case step of
        postShrink => shrink p
      | postFlatten => Flatten.transform p

fun doPostSteps (steps: postStep list, p: Program.t) =
    case steps of
        s::steps' => doPostSteps (steps', doPostStep (s, p))
      | [] => p

fun transform (p: Program.t): Program.t =
    let
       val policy =
           case !Control.preFlattenConsumerPolicy of
               Control.PreFlattenConsumerPolicy.Always => FlattenAlways
             | Control.PreFlattenConsumerPolicy.AnyUnpack => FlattenForAnyUnpack
             | Control.PreFlattenConsumerPolicy.AllUnpack => FlattenForAllUnpack
       val resolvePolicy =
          case !Control.preFlattenResolvePolicy of
             Control.PreFlattenResolvePolicy.Global => UnionAlias
           | Control.PreFlattenResolvePolicy.Local => DropAlias
       val typesPolicy =
          case !Control.preFlattenTypesPolicy of
             Control.PreFlattenTypesPolicy.Any => FlattenAnyType
           | Control.PreFlattenTypesPolicy.Tuple => FlattenOnlyTuple
           | Control.PreFlattenTypesPolicy.Con => FlattenOnlyConApp
       val postSteps =
          List.map (!Control.preFlattenPostSteps,
             fn Control.PreFlattenPostStep.Shrink => postShrink
              | Control.PreFlattenPostStep.Flatten => postFlatten)
       fun applyPostSteps p = doPostSteps (postSteps, p)
       val levelSteps =
          List.map (!Control.preFlattenLevelSteps,
             fn Control.PreFlattenLevelStep.Block => blockOnly
              | Control.PreFlattenLevelStep.Function => functionOnly)
       val recursiveFlatten =
          if !Control.preFlattenRecursiveSteps = 0
             then noRecursiveFlatten
          else recursiveFlattenSteps (!Control.preFlattenRecursiveSteps)
       val transferFlatten =
           case !Control.preFlattenTransferPolicy of
               Control.PreFlattenTransferPolicy.Always =>
               FlattenAnyTransfer
            |  Control.PreFlattenTransferPolicy.TailOnly =>
               FlattenOnlyTailCalls
       fun applyLevels (p) = let          fun apply (step, p') =
           flattenOnce (policy, resolvePolicy, typesPolicy, step,
                        recursiveFlatten, transferFlatten) p'
       in
          foldTransformation (levelSteps, apply, p)
       end
       fun loop (p, n) =
          if n >= !Control.preFlattenMaxIters
             then p
          else
             case applyLevels p of
                NONE => p
              | SOME p' => loop (applyPostSteps p', n + 1)
    in
       loop (p, 0)
    end
end
