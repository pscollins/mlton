functor ShallowFlatten (S: SSA_TRANSFORM_STRUCTS): SHALLOW_FLATTEN =
struct
open S
structure FlattenUtil = FlattenUtil (S)
open FlattenUtil

(* Toggle verbose logging *)
val kVerbose = false

fun verboseDiagnostic thunk =
    if kVerbose then Control.diagnostic thunk
    else ()

(* Returns the unique variable bound by `s`, else crash *)
fun extractBind (s: Statement.t): Var.t =
    case Statement.var s of
        SOME v => v
      | _ => Error.bug ("No bind in statement: " ^
                        Layout.toString (Statement.layout s))

(* Returns the type of the given statement *)
fun extractType (s: Statement.t): Type.t = let
   val Statement.T {ty, ...} = s
in
   ty
end


fun maybeFlattenType (t: Type.t) = let
   fun maybeFlattenArr xs =
       case Type.dest xs of
           Type.Tuple xs => SOME (Type.tuple (Vector.map (xs, Type.array)))
        |  _ => NONE
   fun maybeFlattenVec xs =
       case Type.dest xs of
           Type.Tuple xs => SOME (Type.tuple (Vector.map (xs, Type.vector)))
        |  _ => NONE
in
   case Type.dest t of
       Type.Array arr => maybeFlattenArr arr
     | Type.Vector vec => maybeFlattenVec vec
     | _ => NONE
end

fun deContainer (t: Type.t) =
    case Type.dest t of
        Type.Array arr => arr
      | Type.Vector vec => vec
      | _ => Error.bug "Bad deContainer"

(* ('a array * 'b array * ...)
   ->
   {'a, 'b, ...}

  (and likewise for `vector`)
*)
fun getFlattenedElementTypes (flatType: Type.t) =
    Vector.map (Type.deTuple flatType, deContainer)

(* Returns:

    * t == tuple? number of tuple elements
    * t != tuple? 0
*)
fun getTupleTypeWidth (t: Type.t): int =
    case Type.deTupleOpt t of
        SOME ts => Vector.length ts
      | _ => 0

(* Like above, but requires that `t` is `(...) array` or `(...) vector` *)
fun getContainerOfTupleTypeWidth (t: Type.t): int =
    case Type.dest t of
        Type.Array t' => getTupleTypeWidth t'
      | Type.Vector t' => getTupleTypeWidth t'
      | _ => 0

(* Callers rely on the fact that an empty container returns *false* *)
fun isAllSame (cmp: 'a * 'a -> bool) (xs: 'a vector): bool = let
   fun reduce (curr: 'a, prev: 'a option): 'a option =
       case prev of
           SOME prev' =>
           if cmp (curr, prev') then SOME prev'
           else NONE
         | NONE => NONE
   val init = if Vector.length xs = 0 then NONE
              else SOME (Vector.first xs)
in
   Option.isSome (Vector.fold (xs, init, reduce))
end

(* Returns:

   * t == (t1, t2, ...) tuple ? t1 == t2 == ...
   * otherwise, false
*)
fun isTupleOfSameTupleType (t: Type.t): bool =
    case Type.deTupleOpt t of
        SOME ts => isAllSame Type.equals ts
      | _ => false

(* Like above, but requires that `t` is `(...) array` or `(...) vector` *)
fun isContainerOfSameTupleType (t: Type.t): bool =
    case Type.dest t of
        Type.Array t' => isTupleOfSameTupleType t'
      | Type.Vector t' => isTupleOfSameTupleType t'
      | _ => false

datatype flattenPolicy = MaxWidth of int
                       | MaxWidthSameType of int

datatype flattenMechanism = FlattenSoA
                          | FlattenAoS

(* Should the value corresponding to `t` be flattened, according to `policy`? *)
fun shouldFlattenType (policy: flattenPolicy) (t: Type.t) : bool = let
   val (maxWidth, differentOk) =
       case policy of
           MaxWidth w => (w, true)
         | MaxWidthSameType w => (w, false)
   (* No reason to flatten tuples with <2 elements *)
   val kMinWidth = 2
   val currWidth = getContainerOfTupleTypeWidth t
   val matchesWidth =
       (currWidth >= kMinWidth) andalso
       (currWidth <= maxWidth)
   val matchesSame = differentOk orelse
                     (isContainerOfSameTupleType t)
in
   matchesWidth andalso matchesSame
end

fun getChildren (t: Type.t): Type.t vector =
    case Type.dest t of
        Type.Array t' => Vector.new1 t'
      | Type.Ref t' => Vector.new1 t'
      | Type.Tuple ts' => ts'
      | Type.Vector t' => Vector.new1 t'
      | Type.Weak t' => Vector.new1 t'
      | _ => Vector.new0 ()

datatype conDecision =
         PreserveNode of conDecision vector
         | FlattenNode of conDecision vector

fun layoutConDecision cd =
    case cd of
        PreserveNode cds =>
        Layout.seq [Layout.str "Preserve", Layout.paren (Vector.layout layoutConDecision cds)]
      | FlattenNode cds =>
        Layout.seq [Layout.str "Flatten", Layout.paren (Vector.layout layoutConDecision cds)]

fun getConDecisionForPolicy (policy: flattenPolicy)
                            (t: Type.t): conDecision = let
   val shouldMark = shouldFlattenType policy
   fun walk (t: Type.t) = let
      fun next t' = Vector.map (getChildren t', walk)
   in
      if shouldMark t then
         (* Peel off a layer in the recursion for flattening *)
         FlattenNode (next (deContainer t))
      else
         PreserveNode (next t)
   end
in
   walk t
end

fun getUniqueElement (xs: 'a vector): 'a =
    if Vector.length xs = 1 then
       Vector.first xs
    else Error.bug ("Bad length: " ^ Int.toString (Vector.length xs))

fun conDecisionEquals (l: conDecision, r: conDecision): bool = let
   fun checkSame (ls: conDecision vector, rs: conDecision vector) =
       Vector.length ls = Vector.length rs andalso
       Vector.forall2 (ls, rs, conDecisionEquals)
in
   case (l, r) of
       (PreserveNode ls, PreserveNode rs) => checkSame (ls, rs)
     | (FlattenNode ls, FlattenNode rs) => checkSame (ls, rs)
     | _ => false
end

exception InvalidConFlattening
fun applyConDecision (mech: flattenMechanism)
                     (cd: conDecision,
                      t: Type.t): Type.t = let
   fun assertEmpty xs =
       if Vector.length xs = 0 then ()
       else raise InvalidConFlattening
   fun getAllSamElType ts =
       if isAllSame Type.equals ts then
          Vector.first ts
       else raise InvalidConFlattening
   fun getAllSameConDecision cds =
       (* Note this can be slow in theory, but in practice the number of nested
          flattenings is unlikely to be large *)
       if isAllSame conDecisionEquals cds then
          Vector.first cds
       else raise InvalidConFlattening
   fun walk (t: Type.t, cd: conDecision): Type.t =
       case (Type.dest t, cd) of
           (* Single-child, flattenable nodes *)
           (Type.Array t', PreserveNode cd') =>
           Type.array (walk (t', getUniqueElement cd'))
         | (Type.Array t', FlattenNode cds') =>
           applyMechanism (Type.deTuple t', cds', Type.array)
         | (Type.Vector t', PreserveNode cd') =>
           Type.vector (walk (t', getUniqueElement cd'))
         | (Type.Vector t', FlattenNode cds') =>
           applyMechanism (Type.deTuple t', cds', Type.vector)
         (* Multi-child, un-flattenable internal nodes *)
         | (Type.Tuple ts', PreserveNode cds') =>
           Type.tuple (Vector.map2 (ts', cds', walk))
         (* Single-child, un-flattenable internal nodes *)
         | (Type.Ref t', PreserveNode cd') =>
           Type.reff (walk (t', getUniqueElement cd'))
         | (Type.Weak t', PreserveNode cd') =>
           Type.weak (walk (t', getUniqueElement cd'))
         (* Leaf nodes *)
         | (_, PreserveNode cd') =>
           (assertEmpty cd'; t)
         (* Invalid flattening decisions *)
         | _ => raise InvalidConFlattening
   and applyMechanism (elTypes: Type.t vector,
                       elDecisions: conDecision vector,
                       mkContainer: Type.t -> Type.t): Type.t =
       case mech of
           FlattenSoA =>
           (* ['a, 'b, 'c] + [cd1, cd2, cd3] + Type.array
              ->
              (walk ('a, cd1)) array *
              (walk ('b, cd2)) array *
              (walk ('c, cd3)) array
            *)
           Type.tuple (Vector.map2 (elTypes,
                                    elDecisions,
                                    mkContainer o walk))
         | FlattenAoS =>
           (* ['a, 'a, 'a] + [cd1, cd1, cd1] + Type.array
              ->
              (walk ('a, cd1)) array
           *)
           mkContainer (walk (getAllSamElType elTypes,
                              getAllSameConDecision elDecisions))

   val _ = ()
in
   walk (t, cd)
end

fun isArrayPrim prim =
    case prim of
        Prim.Array_alloc _=> true
      | Prim.Array_array  => true
      | Prim.Array_copyArray  => true
      | Prim.Array_copyVector  => true
      | Prim.Array_length  => true
      | Prim.Array_sub => true
      | Prim.Array_toArray  => true
      | Prim.Array_toVector  => true
      | Prim.Array_uninit  => true
      | Prim.Array_uninitIsNop  => true
      | Prim.Array_update => true
      | _ =>  false

fun isVectorPrim prim =
    case prim of
        Prim.Vector_length => true
      | Prim.Vector_sub =>  true
      | Prim.Vector_vector =>  true
      (* TODO: more cases *)
      | _ =>  false

fun isContainerPrim prim =
    isArrayPrim prim orelse
    isVectorPrim prim

datatype containerType = ArrayType
                      | VectorType


fun getContainerType prim =
    case (isArrayPrim prim, isVectorPrim prim) of
        (true, false) => ArrayType
      | (false, true) => VectorType
      | _ =>  Error.bug (concat ["Not container prim: ",
                                 Prim.toString prim])

(* Given the `targs` of a vector/array, returns the corresponding flattened type

    {('a * 'b), Prim.Array_...} -> 'a array * 'b array
    {('a * 'b), Prim.Vector_...} -> 'a array * 'b array

 *)
fun getFlattenedPrimTArg (prim, targs) =
    case (isArrayPrim prim, isVectorPrim prim, Vector.length targs) of
        (true, false, 1) =>
       (* Wrap in `array` to get the array type:
             Array_alloc['a * b'] ->
             ('a * 'b) array
        *)
       maybeFlattenType (Type.array (Vector.first targs))
     | (false, true, 1) => 
       maybeFlattenType (Type.vector (Vector.first targs))
     | _ => NONE

(* Given flattened array/vector targs, returns the nth element type

  ('a array * 'b array, 0) -> 'a
 *)
fun getNthElemType (flatArg, idx) = let
   (* {'a, 'b } *)
   val components = getFlattenedElementTypes flatArg
in
   (* -> 'a *)
   Vector.sub (components, idx)
end

(* {('a * 'b * ...), 1}
   ->
   'b
 *)
fun getNthTupleType (t: Type.t, idx: int) = let
   val elts = Type.deTuple t
in
   Vector.sub (elts, idx)
end


(* ('a array * 'b array) tuple -> 2  *)
fun getNumElementTypes flatArg = let
   (* {'a array, 'b array} *)
   val components = Type.deTuple flatArg
in
   Vector.length components
end

fun concatVecs (vecs: 'a vector list): 'a vector =
    Vector.concatV (Vector.fromList vecs)

fun maybeStmtsToLayout (maybeStmts: Statement.t vector option) =
    case maybeStmts of
        SOME ss => Layout.align (Vector.toList (Vector.map (ss, Statement.layout)))
      | NONE => Layout.str "(none)"

fun maybeFlattenStatement (s: Statement.t) = let
   val Statement.T {exp, ty, var} = s
   fun doPrimApp (args, prim, targs) = let
      fun logThunk () =
          Layout.align [
             Layout.seq [Layout.str "doPrimApp(maybeFlattenStatement): ",
                         Prim.layout prim,
                         Layout.str " with targs ",
                         Vector.layout Type.layout targs],
             Layout.seq [
                Layout.str "whole_statement: ",
                Statement.layout s]
          ]
      val _ = Control.diagnostic logThunk
      fun mkLogResultThunk (res) = let
         fun logThunk() = Layout.seq [
                Layout.str "Result(maybeFlattenStatement): ",
                maybeStmtsToLayout res
             ]
      in
         logThunk
      end
      (* flatBind = Array_alloc[targ](n)
         `primArg` is {raw=true/false}`
       *)
      fun mkAlloc primArg targ =  let
         val allocExp = Exp.PrimApp {args=args,
                                     prim=Prim.Array_alloc primArg,
                                     targs=Vector.new1 targ}
      in
         Statement.T {exp = allocExp,
                      ty = Type.array targ,
                      var = SOME (Var.newString "flatBind")}
      end
      (* arr_n = select(arr, n) *)
      fun mkSelect (flatArg, from) idx = let
         val arrTy = getNthTupleType (flatArg, idx)
         val selectExp = Exp.Select {offset = idx,
                                     tuple = from}
         val varBasename = "flatArr_" ^ (Int.toString idx)
         val newVar = Var.newString varBasename
      in
         Statement.T {exp = selectExp,
                      ty = arrTy,
                      var = SOME newVar}
      end
      (* arr: 'a array = ...
         ->
         x = Array_sub['a](arr, n)

         Assumes `prim` is `Vector_sub` or `Array_sub`
       *)
      fun mkLoad (stmt: Statement.t): Statement.t = let
         val Statement.T {ty=arrTy, ...} = stmt
         val elTy = deContainer arrTy
         val idxArg = Vector.sub (args, 1)
         val subExp = Exp.PrimApp {args = Vector.new2 (extractBind stmt, idxArg),
                                   prim = prim,
                                   targs = Vector.new1 elTy}
      in
         Statement.T {
            exp = subExp,
            ty = elTy,
            var = SOME (Var.newString "flatLoad")}

      end
      (* {x_arr: 'a array = ...; x: 'a = ...}
         ->
         _ = Array_update(x_arr, n, x)

         `primArg` is `{writeBarrier=true/false}`
      *)
      fun mkStore (arrStmt: Statement.t, varStmt: Statement.t): Statement.t = let
         val Statement.T {ty=elTy, ...} = varStmt
         val idxArg = Vector.sub (args, 1)
         val storeExp = Exp.PrimApp {args = Vector.new3 (extractBind arrStmt,
                                                         idxArg,
                                                         extractBind varStmt),
                                     prim = Prim.Array_update,
                                     targs = Vector.new1 elTy}
      in
         Statement.T {
            exp = storeExp,
            ty = Type.unit,
            var = NONE}

      end

      (* v1: 'a = ...
         v2: 'b = ...
         ...
         -->
         bindTo: ('a * b * ...) = tuple (v1, v2, ...)
      *)
      fun mkTuple (bindTo: Var.t option,
                   stmts: Statement.t vector): Statement.t = let
         val binds = Vector.map (stmts, extractBind)
         val tys = Vector.map (stmts, extractType)
         val tupleExp = Exp.Tuple binds
      in
         Statement.T {exp = tupleExp,
                      ty = Type.tuple tys,
                      var = bindTo}
      end
      (* arr: 'a array = ....
         ->
         vec: 'a vec = Array_toVector['a](arr)
      *)
      fun mkArrToVec (arrStmt: Statement.t): Statement.t = let
         val elTy = Type.deArray (extractType arrStmt)
         val toVecExp = Exp.PrimApp {args = Vector.new1 (extractBind arrStmt),
                                     prim = Prim.Array_toVector,
                                     targs = Vector.new1 elTy}
      in
         Statement.T {exp = toVecExp,
                      ty = Type.vector elTy,
                      var = SOME (Var.newString "flatVec")}
      end
      (* arr: 'a array = ....
         ->
         arr': 'a array = Array_toArray['a](arr)
      *)
      fun mkArrayToArray (arrStmt: Statement.t): Statement.t = let
         val elTy = Type.deArray (extractType arrStmt)
         val arrayToArrayExp = Exp.PrimApp {args = Vector.new1 (extractBind arrStmt),
                                            prim = Prim.Array_toArray,
                                            targs = Vector.new1 elTy}
      in
         Statement.T {exp = arrayToArrayExp,
                      ty = Type.array elTy,
                      var = SOME (Var.newString "flatArr")}
      end
      fun mkArrayUninit (arrStmt: Statement.t): Statement.t = let
         val elTy = Type.deArray (extractType arrStmt)
         (* Args are (arr, idx) *)
         val idx = Vector.sub (args, 1)
         val arrayUninitExp = Exp.PrimApp {args = Vector.new2 (extractBind arrStmt, idx),
                                           prim = Prim.Array_uninit,
                                           targs = Vector.new1 elTy}
      in
         Statement.T {exp = arrayUninitExp,
                      ty = Type.unit,
                      var = SOME (Var.newString "flatArr")}
      end
      fun buildArrayAlloc (primArg, flatArg) = let
         val components = getFlattenedElementTypes flatArg
         (*
            arr_a = Array_Alloc['a]
            arr_b = Array_Alloc['b]
            ...
         *)
         val newAllocs = Vector.map (components, mkAlloc primArg)
         (* arr = tuple (arr_a, arr_b, ...) *)
         val newTuple = Statement.T {
                exp=Exp.Tuple (Vector.map (newAllocs, extractBind)),
                ty=flatArg,
                var=var
             }
      in
         (*
           arr_a = ...
           arr_b = ...
           ...
           arr = tuple (...)

          TODO(pscollins): For some reason, `Vector.concat` fails
         *)
         Vector.concatV (Vector.fromList [newAllocs, Vector.new1 newTuple])
      end
      fun buildArrayLength flatArg = let
         val elemTy = getNthElemType (flatArg, 0)
         (* arr_a = select(arr, 0) *)
         val arrStmt = mkSelect (flatArg, Vector.first args) 0
         (* Array_length['a](arr_a) *)
         val lenExp = Exp.PrimApp {args = Vector.new1 (extractBind arrStmt),
                                   (* Pick either `Array_` or `Vector_` length *)
                                   prim = prim,
                                   targs = Vector.new1 elemTy}
         (* len: int = $lenExp *)
         val lenStmt = Statement.T {exp = lenExp,
                                    ty = ty,
                                    var = var}
      in
         (*
            arr_a: 'a array = select(arr, 0)
            len: int = Array_length['a](arr_a)
         *)
         Vector.new2 (arrStmt, lenStmt)
      end
      fun buildArraySub flatArg = let
         val numTypes = getNumElementTypes flatArg
         (* arr_a = select(arr, 0)
            arr_b = select(arr, 1)
            ...
          *)
         val arrValue = Vector.first args
         val selectStmts = Vector.tabulate (numTypes,
                                            mkSelect (flatArg, arrValue))
         (* x_a = Array_sub['a](arr_a, n)
            x_b = Array_sub['b](arr_b, n)
            ...
          *)
         val loadStmts = Vector.map (selectStmts, mkLoad)
         (* res = tuple (x_a, x_b, ...) *)
         val tupleStmt = mkTuple (var, loadStmts)
      in
         concatVecs [selectStmts,
                     loadStmts,
                     Vector.new1 tupleStmt]
      end
      fun buildArrayUpdate flatArg = let
         val numTypes = getNumElementTypes flatArg
         (* arr_a = select(arr, 0)
            arr_b = select(arr, 1)
            ...
          *)
         val arrValue = Vector.first args
         val selectArrs = Vector.tabulate (numTypes, mkSelect (flatArg,
                                                               arrValue))
         (* x_a = select(x, 0)
            x_b = select(x, 1)
            ...
          *)
         val xValue = Vector.last args
         (* ('a, 'b, ...) *)
         val xTypes = Type.tuple (getFlattenedElementTypes flatArg)
         val selectVars = Vector.tabulate (numTypes, mkSelect (xTypes,
                                                               xValue))
         (* _ = Array_update['a](arr_a, n, x_a)
            _ = Array_update['b](arr_b, n, x_b)
            ...
         *)
         val storeStmts = Vector.map2 (selectArrs, selectVars, mkStore)
      in
         concatVecs [selectArrs,
                     selectVars,
                     storeStmts]
      end
      fun buildArrayToVector flatArg = let
         val numTypes = getNumElementTypes flatArg
         (* arr_a = select(arr, 0)
            arr_b = select(arr, 1)
            ...
          *)
         val arrValue = Vector.first args
         val selectArrs = Vector.tabulate (numTypes, mkSelect (flatArg,
                                                               arrValue))
         (* vec_a = Array_toVector['a](arr_a)
            vec_b = Array_toVector['b](arr_b)
            ...
         *)
         val vecStmts = Vector.map (selectArrs, mkArrToVec)
         (* vec = (vec_a * vec_b * ...) *)
         val tupleStmt = mkTuple (var, vecStmts)
      in
         concatVecs [selectArrs,
                     vecStmts,
                     Vector.new1 tupleStmt]
      end
      fun buildArrayUninitIsNop () = let
         (* For now, just hardcode false: since the original elements were
          tuples, this should be no-worse performance than we had before
          (although it might waste some performance in case we could have
          avoided doing this initialization on the flattened elements) *)
         val falseExp = Exp.ConApp {con = Con.falsee,
                                    args = Vector.new0()}
         val assignStmt = Statement.T {exp = falseExp,
                                       ty = Type.bool,
                                       var = var}
      in
         Vector.new1 assignStmt
      end
      fun buildArrayToArray flatArg = let
         val numTypes = getNumElementTypes flatArg
         (* arr_a = select(arr, 0)
            arr_b = select(arr, 1)
            ...
          *)
         val arrValue = Vector.first args
         val selectArrs = Vector.tabulate (numTypes, mkSelect (flatArg,
                                                               arrValue))
         (* arr'_a = Array_toArray['a](arr_a)
            arr'_b = Array_toArray['b](arr_b)
            ...
         *)
         val newArrStmts = Vector.map (selectArrs, mkArrayToArray)
         (* arr' = (arr'_a * arr'_b * ...) *)
         val tupleStmt = mkTuple (var, newArrStmts)
      in
         concatVecs [selectArrs,
                     newArrStmts,
                     Vector.new1 tupleStmt]
      end
      fun buildArrayUninit flatArg = let
         val numTypes = getNumElementTypes flatArg
         (* arr_a = select(arr, 0)
            arr_b = select(arr, 1)
            ...
          *)
         (* Args are (arr, idx) *)
         val arrValue = Vector.first args
         val selectArrs = Vector.tabulate (numTypes, mkSelect (flatArg,
                                                               arrValue))
         (* _ = Array_uninit['a](arr_a, idx)
            _ = Array_uninit['b](arr_b, idx)
            ...
         *)
         val newInitStmts = Vector.map (selectArrs, mkArrayUninit)
      in
         concatVecs [selectArrs,
                     newInitStmts]
      end
      val result =
          case (prim, getFlattenedPrimTArg (prim, targs))  of
              (Prim.Array_alloc primArg, SOME flatArg) =>
              SOME (buildArrayAlloc (primArg, flatArg))
            | (Prim.Array_length, SOME flatArg) =>
              SOME (buildArrayLength flatArg)
            | (Prim.Vector_length, SOME flatArg) =>
              SOME (buildArrayLength flatArg)
            | (Prim.Array_sub, SOME flatArg) =>
              SOME (buildArraySub flatArg)
            | (Prim.Vector_sub, SOME flatArg) =>
              SOME (buildArraySub flatArg)
            | (Prim.Array_update, SOME flatArg) =>
              SOME (buildArrayUpdate flatArg)
            | (Prim.Array_toVector, SOME flatArg) =>
              SOME (buildArrayToVector flatArg)
            | (Prim.Array_uninitIsNop, _) =>
              (* This prim has an array-valued targ, so it returns NONE *)
              SOME (buildArrayUninitIsNop ())
            | (Prim.Array_toArray, SOME flatArg) =>
              SOME (buildArrayToArray flatArg)
            | (Prim.Array_uninit, SOME flatArg) =>
              SOME (buildArrayUninit flatArg)
            | _ => NONE
      val _ = Control.diagnostic (mkLogResultThunk result)
   in
      result
   end
in
   case exp of
       Exp.PrimApp {args, prim, targs} =>
       if isContainerPrim prim then
          doPrimApp (args, prim, targs)
       else SOME (Vector.new1 s)
     | _ => SOME (Vector.new1 s)
end

(* If

    t = ('a * 'b * 'c ...) tuple

   is a tuple type satisying `isTupleOfSameTupleType`, returns `SOME 'a`.
 *)
fun deTupleOfSameTupleType (t: Type.t): Type.t option =
    if isTupleOfSameTupleType t then
       SOME (Vector.first (Type.deTuple t))
    else NONE

fun getUniqueAosTArg (targs: Type.t vector) = let
in
   if Vector.size targs = 1 then
      deTupleOfSameTupleType (Vector.first targs)
   else NONE
end

fun getUniqueElementOrDefault (xs: 'a vector, default: 'a): 'a =
    if Vector.size xs = 1 then
       Vector.first xs
    else default

fun getLenPrim (ct: containerType) =
       case ct of
           ArrayType => Prim.Array_length
         | VectorType => Prim.Vector_length

fun mkContainerTypeOf (ct: containerType, elTy: Type.t): Type.t =
    case ct of
        ArrayType => Type.array elTy
      | VectorType => Type.vector elTy

fun maybeFlattenStatementAoS (s: Statement.t) = let
   val Statement.T {exp, ty, var} = s
   (* dest := Array_alloc[tArg](len) *)
   fun mkArrayAlloc (primArg, tArg: Type.t, len: Var.t,
                     dest: Var.t option) = let
      val allocExp = Exp.PrimApp {args = Vector.new1 len,
                                  prim = Prim.Array_alloc primArg,
                                  targs = Vector.new1 tArg}
   in
      Statement.T {exp = allocExp,
                   ty = Type.array tArg,
                   var = dest}
   end
   (* indexConst: indexTy := intVal *)
   fun mkIndexConst (intVal: int): Statement.t = let
      val intWordX = WordX.fromInt (intVal, WordSize.seqIndex ())
      val intExp = Exp.Const (Const.word intWordX)
   in
      Statement.T {exp = intExp,
                   ty = Type.word (WordSize.seqIndex ()),
                   var = SOME (Var.newString "indexConst")}
   end
   (* mulRes := lhs * rhs *)
   fun mkMul (lhs: Var.t, rhs: Var.t): Statement.t = let
      val mulExp = Exp.PrimApp {args = Vector.new2 (lhs, rhs),
                                (* TODO(pscollins): Is `signed = false` correct? *)
                                prim = Prim.Word_mul (WordSize.seqIndex (),
                                                      {signed = false}),
                                targs = Vector.new0 ()}
   in
      Statement.T {exp = mulExp,
                   ty = Type.word (WordSize.seqIndex ()),
                   var = SOME (Var.newString "mulRes")}
   end
   (* dest := lhs / rhs *)
   fun mkDiv (lhs: Var.t, rhs: Var.t, dest: Var.t option): Statement.t = let
      val divExp = Exp.PrimApp {args = Vector.new2 (lhs, rhs),
                                (* TODO(pscollins): Is `signed = false` correct? *)
                                prim = Prim.Word_quot (WordSize.seqIndex (),
                                                      {signed = false}),
                                targs = Vector.new0 ()}
   in
      Statement.T {exp = divExp,
                   ty = Type.word (WordSize.seqIndex ()),
                   var = dest}
   end
   (* addRes := lhs + rhs *)
   fun mkAdd (lhs: Var.t) (rhs: Var.t): Statement.t = let
      val addExp = Exp.PrimApp {args = Vector.new2 (lhs, rhs),
                                prim = Prim.Word_add (WordSize.seqIndex ()),
                                targs = Vector.new0 ()}
   in
      Statement.T {exp = addExp,
                   ty = Type.word (WordSize.seqIndex ()),
                   var = SOME (Var.newString "addRes")}
   end
   (* newLen := {Array,Vector}_length[tArg](arg) *)
   fun mkContainerLen (containerType, args, tArg) = let
      val lenExp = Exp.PrimApp {args = args,
                                prim = getLenPrim containerType,
                                targs = Vector.new1 tArg}
   in
      Statement.T {exp = lenExp,
                   ty = ty,
                   var = SOME (Var.newString "newLen")}
   end
   (* loadRes := {Array,Vector}_sub[tArg](arrVar, idxVar) *)
   fun mkContainerLoad (loadPrim: Type.t Prim.t,
                        arrVar: Var.t,
                        tArg: Type.t) (idxVar: Var.t) = let
      val subExp = Exp.PrimApp {args = Vector.new2 (arrVar, idxVar),
                                prim = loadPrim,
                                targs = Vector.new1 tArg}
   in
      Statement.T {exp = subExp,
                   (* Return type of a load is just the element type *)
                   ty = tArg,
                   var = SOME (Var.newString "loadRes")}
   end
   (* _ := Array_update[tArg](arrVar, idxVar, valVar) *)
   fun mkArrayStore (arrVar, tArg)
                    (idxVar: Var.t, valVar: Var.t) = let
      val storeExp = Exp.PrimApp {args = Vector.new3 (arrVar, idxVar, valVar),
                                  prim = Prim.Array_update,
                                  targs = Vector.new1 tArg}
   in
      Statement.T {exp = storeExp,
                   ty = Type.unit,
                   var = NONE}
   end

   (* dest: (ty1 * ty2* ...) := (x1, x2, ...)  *)
   fun mkTuple (stmts: Statement.t vector, dest: Var.t option) = let
      (* x1, x2, ... *)
      val vars = Vector.map (stmts, extractBind)
      (* t1, t2, ... *)
      val tys = Vector.map (stmts, extractType)
   in
      Statement.T {exp = Exp.Tuple vars,
                   ty = Type.tuple tys,
                   var = dest}
   end
   (* selectRes: ty := tuple[idx] *)
   fun mkSelect (tuple: Var.t, ty: Type.t) (idx: int) =
       Statement.T {exp = Exp.Select {offset = idx, tuple = tuple},
                    ty = ty,
                    var = SOME (Var.newString "selectRes")}
   (* dest := Array_toVector[tArg](arrVar) *)
   fun mkToVector (arrVar: Var.t, tArg: Type.t, dest: Var.t option) = let
      val toVecExp = Exp.PrimApp {args = Vector.new1 arrVar,
                                  prim = Prim.Array_toVector,
                                  targs = Vector.new1 tArg}
   in
      Statement.T {exp = toVecExp,
                   ty = Type.vector tArg,
                   var = dest}
   end
   (* dest := Array_toArray[tArg](arrVar) *)
   fun mkToArray (arrVar: Var.t, tArg: Type.t, dest: Var.t option) = let
      val toArrExp = Exp.PrimApp {args = Vector.new1 arrVar,
                                  prim = Prim.Array_toArray,
                                  targs = Vector.new1 tArg}
   in
      Statement.T {exp = toArrExp,
                   ty = Type.array tArg,
                   var = dest}
   end
   (* _ := Array_uninit[tArg](arrVar, idxVar) *)
   fun mkArrayUninit (arrVar: Var.t, tArg: Type.t)
                     (idxVar: Var.t) = let
      val uninitExp = Exp.PrimApp {args = Vector.new2 (arrVar, idxVar),
                                   prim = Prim.Array_uninit,
                                   targs = Vector.new1 tArg}
   in
      Statement.T {exp = uninitExp,
                   ty = Type.unit,
                   var = NONE}
   end

   fun doPrimApp (args, prim, targs) = let
      val tupleWidth = getTupleTypeWidth (getUniqueElementOrDefault (targs,
                                                                     Type.unit))
      fun logThunk () =
          Layout.align [
             Layout.seq [Layout.str "doPrimApp(maybeFlattenStatementAos): ",
                         Prim.layout prim,
                         Layout.str " with targs ",
                         Vector.layout Type.layout targs],
             Layout.seq [
                Layout.str "whole_statement: ",
                Statement.layout s]
          ]
      val _ = Control.diagnostic logThunk
      fun mkLogResultThunk (res) = let
         fun logThunk() = Layout.seq [
                Layout.str "Result(maybeFlattenStatementAoS): ",
                maybeStmtsToLayout res
             ]
      in
         logThunk
      end
      fun buildArrayAlloc (primArg, tArg) = let
         (* tupleSize: indexTy = tupleWidth *)
         val constStmt = mkIndexConst tupleWidth
         (* newLen = n * tupleWidth *)
         val mulStmt = mkMul (Vector.first args,
                              extractBind constStmt)
         (* arr = Array_alloc[elTy](newLen) *)
         val allocStmt = mkArrayAlloc (primArg, tArg,
                                       extractBind mulStmt, var)
      in
         Vector.new3 (constStmt, mulStmt, allocStmt)
      end
      fun buildContainerLength tArg = let
         (* tupleSize: indexTy = tupleWidth *)
         val constStmt = mkIndexConst tupleWidth
         (* newLen = Array_length[elTy](arr) *)
         val lenStmt = mkContainerLen (getContainerType prim,
                                       args, tArg)
         (* n = newLen / tupleSize *)
         val divStmt = mkDiv (extractBind lenStmt,
                              extractBind constStmt,
                              var)
      in
         Vector.new3 (lenStmt, constStmt, divStmt)
      end
      (* x := (Array_sub(i * tupleWidth), Array_sub(i * tupleWidth + 1), ...)  *)
      fun buildContainerLoad tArg = let
         (* tupleSize: indexTy = tupleWidth *)
         val constStmt = mkIndexConst tupleWidth
         (* baseIdx: indexTy = tupleWidth * i *)
         val mulStmt = mkMul (Vector.sub (args, 1),
                              extractBind constStmt)
         (* [val_{j} = j for j in range(tupleWidth)]  *)
         val offsetStmts = Vector.tabulate (tupleWidth, mkIndexConst)
         (* [idx_{j} = baseIdx + val_{j} for j in range(tupleWidth)]  *)
         val idxStmts = Vector.map (Vector.map (offsetStmts, extractBind),
                                    mkAdd (extractBind mulStmt))
         (* [x_{j} = Array_sub[elTy](x, j) for j in range(tupleWidth) *)
         val loadStmts = Vector.map (Vector.map (idxStmts, extractBind),
                                     (* `prim` is `{Array,Vector}_sub` (maybe
                                     with a `primArg`) *)
                                     mkContainerLoad (prim,
                                                      Vector.first args, tArg))
         (* x = (x_0, x_1, ...) *)
         val tupleStmt = mkTuple (loadStmts, var)
      in
         concatVecs [Vector.new2 (constStmt, mulStmt),
                     offsetStmts,
                     idxStmts,
                     loadStmts,
                     Vector.new1 tupleStmt]
      end
      (* arr[i*tupleWidth:i*(tupleWidth+1)-1] = x[0:tupleWidth-1] *)
      fun buildArrayStore tArg = let
         (* tupleSize: indexTy = tupleWidth *)
         val constStmt = mkIndexConst tupleWidth
         (* baseIdx: indexTy = tupleWidth * i *)
         val mulStmt = mkMul (Vector.sub (args, 1),
                              extractBind constStmt)
         (* [val_{j} = j for j in range(tupleWidth)]  *)
         val offsetStmts = Vector.tabulate (tupleWidth, mkIndexConst)
         (* [idx_{j} = baseIdx + val_{j} for j in range(tupleWidth)]  *)
         val idxStmts = Vector.map (Vector.map (offsetStmts, extractBind),
                                    mkAdd (extractBind mulStmt))
         (* [x_{j} = x[j] for j in range(tupleWidth)] *)
          val selectStmts = Vector.tabulate (tupleWidth,
                                             mkSelect (Vector.sub (args, 2), tArg))
         (* _ := Array_update[elTy](arr, idx_{j}, x_{j}) for j in range(tupleWidth) *)
         val storeStmts = Vector.map2 (Vector.map (idxStmts, extractBind),
                                       Vector.map (selectStmts, extractBind),
                                       mkArrayStore (Vector.first args, tArg))
      in
         concatVecs [Vector.new2 (constStmt, mulStmt),
                     offsetStmts,
                     idxStmts,
                     selectStmts,
                     storeStmts]
      end
      (* var := Array_toVector[tArg](arr) *)
      fun buildArrayToVector tArg = let
         val toVectorStmt = mkToVector (Vector.first args, tArg, var)
      in
         Vector.new1 toVectorStmt
      end
      (* isNop: bool := false *)
      fun buildArrayUninitIsNop () = let
         (* For now, just hardcode false: since the original elements were
          tuples, this should be no-worse performance than we had before
          (although it might waste some performance in case we could have
          avoided doing this initialization on the flattened elements) *)
         val falseExp = Exp.ConApp {con = Con.falsee,
                                    args = Vector.new0()}
         val assignStmt = Statement.T {exp = falseExp,
                                       ty = Type.bool,
                                       var = var}
      in
         Vector.new1 assignStmt
      end
      (* var := Array_toArray[tArg](arr) *)
      fun buildArrayToArray tArg = let
         val toArrayStmt = mkToArray (Vector.first args, tArg, var)
      in
         Vector.new1 toArrayStmt
      end
      (* Array_uninit[tArg](arr[i*tupleWidth:(i+1)*tupleWidth-1]) *)
      fun buildArrayUninit tArg = let
         (* tupleSize: indexTy = tupleWidth *)
         val constStmt = mkIndexConst tupleWidth
         (* baseIdx: indexTy = tupleWidth * i *)
         val mulStmt = mkMul (Vector.sub (args, 1),
                              extractBind constStmt)
         (* [val_{j} = j for j in range(tupleWidth)]  *)
         val offsetStmts = Vector.tabulate (tupleWidth, mkIndexConst)
         (* [idx_{j} = baseIdx + val_{j} for j in range(tupleWidth)]  *)
         val idxStmts = Vector.map (Vector.map (offsetStmts, extractBind),
                                    mkAdd (extractBind mulStmt))

         val arrayUninitStmts = Vector.map (Vector.map (idxStmts, extractBind),
                                            mkArrayUninit (Vector.first args,
                                                           tArg))
      in
         concatVecs [Vector.new2 (constStmt, mulStmt),
                     offsetStmts,
                     idxStmts,
                     arrayUninitStmts]
      end
      val result =
          case (prim, getUniqueAosTArg (targs)) of
              (Prim.Array_alloc primArg, SOME tArg)
              => SOME (buildArrayAlloc (primArg, tArg))
           | (Prim.Array_length, SOME tArg)
             => SOME (buildContainerLength tArg)
           | (Prim.Vector_length, SOME tArg)
             => SOME (buildContainerLength tArg)
           | (Prim.Array_sub, SOME tArg)
             => SOME (buildContainerLoad tArg)
           | (Prim.Vector_sub, SOME tArg)
             => SOME (buildContainerLoad tArg)
           | (Prim.Array_update, SOME tArg)
             => SOME (buildArrayStore tArg)
           | (Prim.Array_toVector, SOME tArg)
             => SOME (buildArrayToVector tArg)
           | (Prim.Array_uninitIsNop, SOME tArg)
             (* TODO(pscollins): The pattern match is a bit different here than
                in the SoA case -- the SoA version incorrectly thinks that the
                type argument is the array type when it should be the element
                type, so the SoA version effectively hardcodes this to `false`
                for all arrays in the program. Revisit. *)
             => SOME (buildArrayUninitIsNop ())
           | (Prim.Array_toArray, SOME tArg)
             => SOME (buildArrayToArray tArg)
           | (Prim.Array_uninit, SOME tArg)
             => SOME (buildArrayUninit tArg)
           | _ => NONE
      val _ = Control.diagnostic (mkLogResultThunk result)
   in
      result
   end
in
   case exp of
       Exp.PrimApp {args, prim, targs} =>
       if isContainerPrim prim then
          doPrimApp (args, prim, targs)
       else SOME (Vector.new1 s)
     | _ => SOME (Vector.new1 s)
end

type flattener = {
   updateType: Type.t -> Type.t,
   updateStatements: Statement.t vector -> Statement.t vector
}

fun flattenProgram (f: flattener) (p: Program.t) : Program.t = let
   val {updateType, updateStatements} = f
   val Program.T {datatypes, functions, globals, main} = p
   fun doCon {args, con} = {args = Vector.map (args, updateType),
                            con = con}
   fun doDatatype (Datatype.T {cons, tycon}) =
       Datatype.T {cons = Vector.map (cons, doCon),
                   tycon = tycon}
   fun doArgs args = let
      fun doArg (var, ty) = (var, updateType ty)
   in
      Vector.map (args, doArg)
   end
   fun doBlock (Block.T {args, label, statements, transfer}) =
       Block.T {args = doArgs args,
                label = label,
                statements = updateStatements statements,
                transfer = transfer}
   fun doFunction f = let
      val {args, blocks, mayInline, name, raises, returns, start} =
          Function.dest f
      fun doMaybeVec maybeVec =
          case maybeVec of
              SOME tys => SOME (Vector.map (tys, updateType))
            | NONE => NONE
   in
      Function.new {args = doArgs args,
                    blocks = Vector.map (blocks, doBlock),
                    mayInline = mayInline,
                    name = name,
                    raises = doMaybeVec raises,
                    returns = doMaybeVec returns,
                    start = start}

   end
in
   Program.T {datatypes = Vector.map (datatypes, doDatatype),
              functions = List.map (functions, doFunction),
              globals = updateStatements globals,
              main = main}
end

fun policyToString (policy: flattenPolicy) =
    case policy of
        MaxWidth w => concat ["MaxWidth:", Int.toString w]
     |  MaxWidthSameType w => concat ["MaxWidthSameType:", Int.toString w]

fun mechanismToString (mechanism: flattenMechanism) =
    case mechanism of
        FlattenAoS => "FlattenAoS"
      | FlattenSoA => "FlattenSoA"

fun deepFlattenTypeForConfig (policy: flattenPolicy, mechanism: flattenMechanism)
                             (t: Type.t): Type.t = let
    fun doFlatten t = let
       (* TODO: simplify? *)
       val t' =
           applyConDecision mechanism (getConDecisionForPolicy policy t, t)
    in
       (* Iteratively apply to convergence *)
       if Type.equals (t, t') then t'
       else doFlatten t'
    end
    val t' = doFlatten t
    fun logThunk () = Layout.align [
           Layout.str
               (String.concat ["deepFlattenTypeForConfig(",
                               policyToString policy, ", ",
                               mechanismToString mechanism,
                               "):"]),
           Layout.str "old: ",
           Type.layout t,
           Layout.str "new: ",
           Type.layout t'
        ]
    val _ = verboseDiagnostic logThunk
in
   t'
end


fun doesPolicyFlattenStatement (policy: flattenPolicy)
                               (s: Statement.t): bool = let
   val Statement.T {exp, ty, var} = s
   (* TODO: add a new version that doesn't require wrapping *)
   fun checkElType targs = shouldFlattenType policy (Type.array (getUniqueElement targs))
   fun checkPrim {args, prim, targs} =
       (* All currently-supported cases take the the `targ` as the element type

       TODO(pscollins): Audit
       *)
       case prim of
           Prim.Array_alloc _ => checkElType targs
         | Prim.Array_array => checkElType targs         | Prim.Array_copyArray => checkElType targs
         | Prim.Array_copyVector => checkElType targs
         | Prim.Array_length => checkElType targs
         | Prim.Array_sub => checkElType targs
         | Prim.Array_toArray => checkElType targs
         | Prim.Array_toVector => checkElType targs
         | Prim.Array_uninit => checkElType targs
         | Prim.Array_uninitIsNop => checkElType targs
         | Prim.Array_update => checkElType targs
         | Prim.Vector_length => checkElType targs
         | Prim.Vector_sub => checkElType targs
         | Prim.Vector_vector => checkElType targs
         (* Non-container prims always return false *)
         | _ => false
in
   case exp of
       Exp.PrimApp prim => checkPrim prim
     | _ => false
end

fun deepFlattenStatementsForConfig (policy: flattenPolicy, mechanism: flattenMechanism)
                                   (s: Statement.t): Statement.t vector = let
   val updateType = deepFlattenTypeForConfig (policy, mechanism)
   fun updateTypesInExp exp =
       case exp of
           Exp.PrimApp {args, prim, targs} =>
           Exp.PrimApp {args = args,
                        prim = prim,
                        targs = Vector.map (targs, updateType)}
           | _ => exp

   fun updateTypesInStatement (Statement.T {exp, ty, var}) =
       Statement.T {exp = updateTypesInExp exp,
                    ty = updateType ty,
                    var = var}

   fun doMaybeFlatten stmt =
       case mechanism of
           FlattenSoA => maybeFlattenStatement stmt
         | FlattenAoS => maybeFlattenStatementAoS stmt

   fun flattenIfNeeded (stmt: Statement.t): Statement.t vector =
       if (doesPolicyFlattenStatement policy stmt) then
          (* NONE means a prim is missing, crash *)
          Option.valOf (doMaybeFlatten stmt)
       else Vector.new1 stmt

   fun recursiveFlatten (stmts: Statement.t vector) = let
      (* Recursively apply the flattening transformation until it converges
         (which we can detect by checking to see if we've emitted more
         statements)

         This is necessary to handle nested flattenable types, i.e.:

           arr: (a' * (b' * c')) array = Array_alloc[('a * (b' * c'))](n)
           ->
           arr_0 = Array_alloc[a']
           arr_1_0 = Array_alloc[b']
           arr_1_1 = Array_alloc[c']
           arr_1 = tuple (arr_1_0, arr_1_1)
           arr = tuple (arr_0, arr_1)
      *)
      val result = Vector.concatV (Vector.map (stmts,
                                               flattenIfNeeded))
   in
      if Vector.length result > Vector.length stmts then
         recursiveFlatten result
      else
         result
   end

   (* First, flatten all the `PrimApp`s that we can... *)
   val statements = recursiveFlatten (Vector.new1 s)
   (* ...then update types... *)
   val resultInit = Vector.map (statements, updateTypesInStatement)

             (* ...updating types may have created more flattening opportunities, so run
      again.

      TODO: should we instead run this to convergence? *)
   val result = recursiveFlatten resultInit
    fun logThunk() = Layout.align [
           Layout.str "deepFlattenStatementsForConfig: ",
          Layout.str "initial=",
          Statement.layout s,
          Layout.str "afterFlatten=",
          Vector.layout Statement.layout statements,
          Layout.str "afterTransform=",
          Vector.layout Statement.layout result
       ]
   val _ = Control.diagnostic logThunk
in
   result
end

fun bindEquals (l: Var.t option, r: Var.t option) =
    case (l, r) of
      (NONE, NONE) => true
     | (SOME l', SOME r') => Var.equals (l', r')
     | _ => false

fun statementEquals (l, r) = let
   val Statement.T {exp=lExp, ty=lTy, var=lVar} = l
   val Statement.T {exp=rExp, ty=rTy, var=rVar} = r
in
   Exp.equals (lExp, rExp) andalso
   Type.equals (lTy, rTy) andalso
   bindEquals (lVar, rVar)
end


fun flattenOnce (policy: flattenPolicy, mechanism: flattenMechanism) (p: Program.t): Program.t option = let
   val progress = ref false

   fun checkProgress (statements: Statement.t vector,
                      flattened: Statement.t vector vector) = let
      fun check (s: Statement.t, fs: Statement.t vector): bool =
          (* New statements always means progress *)
          if Vector.length fs > 1 then true
          (* Otherwise, need to inspect the statement to see if any types
             changed *)
          else not (statementEquals (s, getUniqueElement fs))
      fun doUpdate (s, fs) =
          if !progress then
             ()
          else if check (s, fs) then
             progress := true
          else ()
   in
      Vector.foreach2 (statements, flattened, doUpdate)
   end

   (* Applies `deepFlattenStatementsForConfig` and updates `progress` to relect
      any changes *)
   fun flattenStatements statements = let
      val flattened = Vector.map (statements,
                                  deepFlattenStatementsForConfig (policy, mechanism))
      val _ = checkProgress (statements, flattened)
   in
      Vector.concatV flattened
   end
   val flattener = {
      updateType = deepFlattenTypeForConfig (policy, mechanism),
      updateStatements = flattenStatements
   }
   val p' = flattenProgram flattener p
in
   if !progress then
      SOME p'
   else NONE
end


fun transform (p: Program.t): Program.t =
    let
       val policy =
           case !Control.shallowFlattenPolicy of
               Control.ShallowFlattenPolicy.MaxWidth n => MaxWidth n
             | Control.ShallowFlattenPolicy.MaxWidthSameType n => MaxWidthSameType n
       val mechanism =
           case !Control.shallowFlattenMechanism of
               Control.ShallowFlattenMechanism.Aos => FlattenAoS
             | Control.ShallowFlattenMechanism.Soa => FlattenSoA
       fun loop (p, n) =
          if n >= !Control.shallowFlattenMaxIters
             then p
          else
             case flattenOnce (policy, mechanism) p of
                NONE => p
              | SOME p' => loop (p', n + 1)
    in
        loop (p, 0)
     end


end (* end struct *)
