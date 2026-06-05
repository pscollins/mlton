structure Atoms = Atoms ()
structure TypeEnv = TypeEnv (open Atoms)
structure CoreML = CoreML (open Atoms
                           structure Type =
                              struct
                                 open TypeEnv.Type

                                 val makeHom =
                                    fn {con, var} =>
                                    makeHom {con = con,
                                             expandOpaque = true,
                                             var = var}

                                 fun layout t =
                                    #1 (layoutPretty
                                        (t, {expandOpaque = true,
                                             layoutPrettyTycon = Tycon.layout,
                                             layoutPrettyTyvar = Tyvar.layout}))
                              end)
structure InlineTrace = InlineTrace (structure CoreML = CoreML)
structure AnnotateTraceValue = AnnotateTraceValue (structure CoreML = CoreML)
structure AnnotateTrace = AnnotateTrace (structure CoreML = CoreML)
structure CoreMLUtil = CoreMLUtil (structure CoreML = CoreML)
open CoreMLUtil

val _ = print "Instantiated all functors.\n"

val emptyProg = Vector.fromList []
val _ = InlineTrace.inlineTrace {prog = emptyProg}
val _ = AnnotateTrace.annotateTrace {prog = emptyProg}

val _ = print "Testing collectVarsBoundToPred...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val y = Var.newString "y"
      val z = Var.newString "z"
      val ty = CoreML.Type.unit

      fun mkValDec (var, exp) =
         CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = exp,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (var, ty),
               regionPat = Region.bogus
            }
         }

      val expX = CoreML.Exp.var (x, ty)
      val expY = CoreML.Exp.var (y, ty)

      val dec1 = mkValDec (x, expY) (* x = y *)
      val dec2 = mkValDec (y, expX) (* y = x *)
      val dec3 = mkValDec (z, expX) (* z = x *)

      val prog = Vector.fromList [[dec1], [dec2, dec3]]

      fun isVarX e =
         case CoreML.Exp.node e of
            CoreML.Exp.Var (v, _) => Var.equals (v (), x)
          | _ => false

      val res = collectVarsBoundToPred (prog, isVarX)
      val _ = if List.length (VarSet.toList res) = 2 then () else Error.bug "Test failed: wrong size"

      val _ = if setContains res y then () else Error.bug "Test failed: missing y"
      val _ = if setContains res z then () else Error.bug "Test failed: missing z"
      val _ = if setContains res x then Error.bug "Test failed: contains x" else ()

      (* Test multiple bindings - should be ignored *)
      val decMultiple = CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new2 ({
               ctxt = fn () => Layout.empty,
               exp = expX,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (y, ty),
               regionPat = Region.bogus
            }, {
               ctxt = fn () => Layout.empty,
               exp = expX,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (z, ty),
               regionPat = Region.bogus
            })
         }
      val res2 = collectVarsBoundToPred (Vector.fromList [[decMultiple]], isVarX)
      val _ = if VarSet.isEmpty res2 then () else Error.bug "Test failed: decMultiple should be ignored"

      (* Test non-variable pattern - should be ignored *)
      val decWild = CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = expX,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.wild ty,
               regionPat = Region.bogus
            }
         }
      val res3 = collectVarsBoundToPred (Vector.fromList [[decWild]], isVarX)
      val _ = if VarSet.isEmpty res3 then () else Error.bug "Test failed: decWild should be ignored"

      (* Test recursive bindings - should be ignored *)
      val decRvbs = CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new1 {
               lambda = CoreML.Lambda.bogus,
               var = y
            },
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new0 ()
         }
      val res4 = collectVarsBoundToPred (Vector.fromList [[decRvbs]], isVarX)
      val _ = if VarSet.isEmpty res4 then () else Error.bug "Test failed: decRvbs should be ignored"
   in
      ()
   end

val _ = print "Testing recursiveCollectVarsBoundToPred...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val y = Var.newString "y"
      val z = Var.newString "z"
      val w = Var.newString "w"
      val ty = CoreML.Type.unit

      fun mkValDec (var, exp) =
         CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = exp,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (var, ty),
               regionPat = Region.bogus
            }
         }

      val expX = CoreML.Exp.var (x, ty)
      val expY = CoreML.Exp.var (y, ty)

      (* val y = x; val w = y *)
      val decY = mkValDec (y, expX)
      val decW = mkValDec (w, expY)

      val prog = Vector.fromList [[decY, decW]]

      fun isVarX e =
         case CoreML.Exp.node e of
            CoreML.Exp.Var (v, _) => Var.equals (v (), x)
          | _ => false

      val res = recursiveCollectVarsBoundToPred (prog, isVarX)
      
      (* Should contain y because y is bound to x *)
      val _ = if setContains res y then () else Error.bug "Test failed: missing y"
      (* Should contain w because w is bound to y, which is bound to x *)
      val _ = if setContains res w then () else Error.bug "Test failed: missing w"

      (* Test deeply nested and multiple bindings *)
      (*
         val b = x
         val d = x
         val c = d
         val e = y
         val a = b
      *)
      val a = Var.newString "a"
      val b = Var.newString "b"
      val c = Var.newString "c"
      val d = Var.newString "d"
      val e = Var.newString "e"
      
      val decB = mkValDec (b, expX)
      val decD = mkValDec (d, expX)
      val decC = mkValDec (c, CoreML.Exp.var (d, ty))
      val decE = mkValDec (e, expY)
      val decA = mkValDec (a, CoreML.Exp.var (b, ty))
      
      val prog2 = Vector.fromList [[decY, decB, decD, decC, decE, decA]]
      val res2 = recursiveCollectVarsBoundToPred (prog2, isVarX)
      
      val _ = if setContains res2 b then () else Error.bug "Test failed: missing b"
      val _ = if setContains res2 d then () else Error.bug "Test failed: missing d"
      val _ = if setContains res2 a then () else Error.bug "Test failed: missing a"
      val _ = if setContains res2 c then () else Error.bug "Test failed: missing c"
      val _ = if setContains res2 e then () else Error.bug "Test failed: missing e"
   in
      ()
   end

val _ = print "Testing setContains...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val y = Var.newString "y"
      val z = Var.newString "z"
      val vs = VarSet.fromList [x, y]
   in
      if setContains vs x then () else Error.bug "setContains failed: should contain x"
      ; if setContains vs y then () else Error.bug "setContains failed: should contain y"
      ; if setContains vs z then Error.bug "setContains failed: should not contain z" else ()
      ; if setContains (VarSet.empty) x then Error.bug "setContains failed: empty set should not contain x" else ()
   end

val _ = print "Testing mapExps...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val y = Var.newString "y"
      val z = Var.newString "z"
      val ty = CoreML.Type.unit

      fun mkValDec (var, exp) =
         CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = exp,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (var, ty),
               regionPat = Region.bogus
            }
         }

      val expX = CoreML.Exp.var (x, ty)
      val expY = CoreML.Exp.var (y, ty)
      val nodeX = CoreML.Exp.node expX
      val nodeY = CoreML.Exp.node expY

      fun isVarX node =
         case node of
            CoreML.Exp.Var (v, _) => Var.equals (v (), x)
          | _ => false

      fun replaceXWithY node =
         if isVarX node then SOME nodeY else NONE

      (* Test 1: Simple replacement *)
      val prog1 = Vector.fromList [[mkValDec (z, expX)]]
      val res1 = mapExps (prog1, replaceXWithY)

      fun getFirstExp prog =
         case Vector.sub (prog, 0) of
            [CoreML.Dec.Val {vbs, ...}] => #exp (Vector.sub (vbs, 0))
          | _ => Error.bug "Test failed: unexpected program structure"

      fun isVarY e =
         case CoreML.Exp.node e of
            CoreML.Exp.Var (v, _) => Var.equals (v (), y)
          | _ => false

      val _ = if isVarY (getFirstExp res1) then () else Error.bug "Test 1 failed: x not replaced by y"

      (* Test 2: Nested replacement *)
      val letExp = CoreML.Exp.make (CoreML.Exp.Let (Vector.new1 (mkValDec (z, expX)), expX), ty)
      val prog2 = Vector.fromList [[mkValDec (y, letExp)]]
      val res2 = mapExps (prog2, replaceXWithY)

      fun checkNested res =
         case CoreML.Exp.node (getFirstExp res) of
            CoreML.Exp.Let (decs, body) =>
               let
                  val dec = Vector.sub (decs, 0)
                  val innerExp = case dec of
                                    CoreML.Dec.Val {vbs, ...} => #exp (Vector.sub (vbs, 0))
                                  | _ => Error.bug "Unexpected dec"
               in
                  isVarY innerExp andalso isVarY body
               end
          | _ => false

      val _ = if checkNested res2 then () else Error.bug "Test 2 failed: nested x not replaced by y"

      (* Test 3: No recursion after SOME *)
      (* Replace x with (let val z = x in z end). 
         If it recurses, it will replace the inner x too.
         If it doesn't, the inner x remains x. *)
      val replacementNode = CoreML.Exp.Let (Vector.new1 (mkValDec (z, expX)), CoreML.Exp.var (z, ty))
      fun replaceXWithLet node =
         if isVarX node then SOME replacementNode else NONE

      val prog3 = Vector.fromList [[mkValDec (y, expX)]]
      val res3 = mapExps (prog3, replaceXWithLet)

      fun checkNoRecursion res =
         case CoreML.Exp.node (getFirstExp res) of
            CoreML.Exp.Let (decs, _) =>
               let
                  val dec = Vector.sub (decs, 0)
                  val innerExp = case dec of
                                    CoreML.Dec.Val {vbs, ...} => #exp (Vector.sub (vbs, 0))
                                  | _ => Error.bug "Unexpected dec"
               in
                  case CoreML.Exp.node innerExp of
                     CoreML.Exp.Var (v, _) => Var.equals (v (), x)
                   | _ => false
               end
          | _ => false

      val _ = if checkNoRecursion res3 then () else Error.bug "Test 3 failed: recursed after SOME"
   in
      ()
   end

val _ = print "Testing inlineSourceMarkCall...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val y = Var.newString "y"
      val z = Var.newString "z"
      val ty = CoreML.Type.unit

      val expX = CoreML.Exp.var (x, ty)
      val expY = CoreML.Exp.var (y, ty)
      val expZ = CoreML.Exp.var (z, ty)

      val vset = VarSet.fromList [x, y]

      fun mkApp (f, a) = CoreML.Exp.App (f, a)

      (* Test 1: x(z) where x is in vset *)
      val node1 = mkApp (expX, expZ)
      val res1 = inlineSourceMarkCall vset node1
      val _ = case res1 of
                 SOME (CoreML.Exp.PrimApp {args, prim, ...}) =>
                    if Prim.equals (prim, Prim.Trace_sourceMark)
                       andalso Vector.length args = 1
                       andalso (case CoreML.Exp.node (Vector.sub (args, 0)) of
                                   CoreML.Exp.Var (v, _) => Var.equals (v (), z)
                                 | _ => false)
                    then ()
                    else Error.bug "Test 1 failed: wrong PrimApp"
               | _ => Error.bug "Test 1 failed: should have inlined"

      (* Test 2: y(z) where y is in vset *)
      val node2 = mkApp (expY, expZ)
      val res2 = inlineSourceMarkCall vset node2
      val _ = case res2 of
                 SOME (CoreML.Exp.PrimApp {args, ...}) => ()
               | _ => Error.bug "Test 2 failed: should have inlined"

      (* Test 3: z(x) where z is NOT in vset *)
      val node3 = mkApp (expZ, expX)
      val res3 = inlineSourceMarkCall vset node3
      val _ = case res3 of
                 NONE => ()
               | SOME _ => Error.bug "Test 3 failed: should NOT have inlined"

      (* Test 4: Not an App *)
      val node4 = CoreML.Exp.node expX
      val res4 = inlineSourceMarkCall vset node4
      val _ = case res4 of
                 NONE => ()
               | SOME _ => Error.bug "Test 4 failed: should NOT have inlined"

      (* Test 5: App where func is not a Var (e.g. nested App) *)
      val node5 = mkApp (CoreML.Exp.make (node1, ty), expY)
      val res5 = inlineSourceMarkCall vset node5
      val _ = case res5 of
                 NONE => ()
               | SOME _ => Error.bug "Test 5 failed: should NOT have inlined (nested app)"

      (* Test 6: App where func is a Lambda *)
      val lam = CoreML.Lambda.make {arg = z, argType = ty, body = expZ, mayInline = true}
      val node6 = mkApp (CoreML.Exp.lambda lam, expX)
      val res6 = inlineSourceMarkCall vset node6
      val _ = case res6 of
                 NONE => ()
               | SOME _ => Error.bug "Test 6 failed: should NOT have inlined (lambda)"
   in
      ()
   end

val _ = print "Testing convertSourceMarkValueToStatic...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val y = Var.newString "y"
      val z = Var.newString "z"
      val ty = CoreML.Type.unit
      val stringTy = TypeEnv.Type.unresolvedString ()
      val talpha = CoreML.Type.unit (* Placeholder for 'a *)

      val expX = CoreML.Exp.var (x, ty)
      val expY = CoreML.Exp.var (y, ty)
      val expZ = CoreML.Exp.var (z, ty)

      val vset = VarSet.fromList [x, y]

      fun mkApp (f, a) = CoreML.Exp.App (f, a)

      (* sourceMarkValueWrapper ['a] (value, "name") *)
      val valueExp = expZ
      val name = "test_mark"
      val nameExp = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string name), stringTy)
      val tupleArg = CoreML.Exp.tuple (Vector.new2 (valueExp, nameExp))
      
      val funcWithTargs = CoreML.Exp.make (
         CoreML.Exp.Var (fn () => x, fn () => Vector.new1 talpha),
         CoreML.Type.arrow (CoreML.Exp.ty tupleArg, ty)
      )

      (* Test 1: Matching call *)
      val node1 = mkApp (funcWithTargs, tupleArg)
      val res1 = convertSourceMarkValueToStatic vset node1
      val _ = case res1 of
                 SOME (CoreML.Exp.PrimApp {args, prim, targs, ...}) =>
                    (case prim of
                        Prim.Trace_staticSourceMarkValue s =>
                           if s = name
                              andalso Vector.length args = 1
                              andalso (case CoreML.Exp.node (Vector.sub (args, 0)) of
                                          CoreML.Exp.Var (v, _) => Var.equals (v (), z)
                                        | _ => false)
                              andalso Vector.length targs = 1
                           then ()
                           else Error.bug "Test 1 failed: wrong PrimApp content"
                      | _ => Error.bug "Test 1 failed: wrong prim")
               | _ => Error.bug "Test 1 failed: should have inlined"

      (* Test 2: Var not in vset *)
      val funcNotMatch = CoreML.Exp.make (
         CoreML.Exp.Var (fn () => z, fn () => Vector.new1 talpha),
         CoreML.Type.arrow (CoreML.Exp.ty tupleArg, ty)
      )
      val node2 = mkApp (funcNotMatch, tupleArg)
      val res2 = convertSourceMarkValueToStatic vset node2
      val _ = case res2 of
                 NONE => ()
               | SOME _ => Error.bug "Test 2 failed: should NOT have inlined (var not in vset)"

      (* Test 3: Arg is not a tuple *)
      val node3 = mkApp (funcWithTargs, expZ)
      val res3 = convertSourceMarkValueToStatic vset node3
      val _ = case res3 of
                 NONE => ()
               | SOME _ => Error.bug "Test 3 failed: should NOT have inlined (arg not a tuple)"

      (* Test 4: Tuple arg has wrong size *)
      val tripleArg = CoreML.Exp.tuple (Vector.new3 (valueExp, nameExp, valueExp))
      val node4 = mkApp (funcWithTargs, tripleArg)
      val res4 = convertSourceMarkValueToStatic vset node4
      val _ = case res4 of
                 NONE => ()
               | SOME _ => Error.bug "Test 4 failed: should NOT have inlined (tuple size != 2)"

      (* Test 5: Second element of tuple is not a constant string *)
      val dynamicNameExp = CoreML.Exp.var (Var.newString "v", stringTy)
      val dynamicTupleArg = CoreML.Exp.tuple (Vector.new2 (valueExp, dynamicNameExp))
      val node5 = mkApp (funcWithTargs, dynamicTupleArg)
      val res5 = convertSourceMarkValueToStatic vset node5
      val _ = case res5 of
                 NONE => ()
               | SOME _ => Error.bug "Test 5 failed: should NOT have inlined (non-constant name)"
   in
      ()
   end

val _ = print "Testing convertSourceMarkToStatic...\n"

val _ =
   let
      open Atoms
      val ty = CoreML.Type.unit
      val stringTy = TypeEnv.Type.unresolvedString ()

      fun mkPrimApp (prim, args) =
         CoreML.Exp.PrimApp {
            args = Vector.fromList args,
            prim = prim,
            targs = Vector.new0 ()
         }

      (* Test 1: Trace_sourceMark with string constant *)
      val s = "test_mark"
      val constExp = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string s), stringTy)
      val node1 = mkPrimApp (Prim.Trace_sourceMark, [constExp])
      val res1 = convertSourceMarkToStatic node1
      val _ = case res1 of
                 SOME (CoreML.Exp.PrimApp {prim, args, ...}) =>
                    (case prim of
                        Prim.Trace_staticSourceMark s' =>
                           if s = s' andalso Vector.length args = 0 then ()
                           else Error.bug
                                    (concat [
                                         "Test 1 failed: wrong static mark or has args; mark=",
                                         s', " vs want=", s])
                      | _ => Error.bug "Test 1 failed: not Trace_staticSourceMark")
               | _ => Error.bug "Test 1 failed: should have converted"

      (* Test 2: Trace_sourceMark with non-constant argument *)
      val varExp = CoreML.Exp.var (Var.newString "v", stringTy)
      val node2 = mkPrimApp (Prim.Trace_sourceMark, [varExp])
      val res2 = convertSourceMarkToStatic node2
      val _ = case res2 of
                 NONE => ()
               | SOME _ => Error.bug "Test 2 failed: should NOT have converted (non-constant)"

      (* Test 3: Not Trace_sourceMark *)
      val node3 = mkPrimApp (Prim.Array_length, [varExp])
      val res3 = convertSourceMarkToStatic node3
      val _ = case res3 of
                 NONE => ()
               | SOME _ => Error.bug "Test 3 failed: should NOT have converted (wrong prim)"

      (* Test 4: Trace_sourceMark with multiple arguments *)
      val node4 = mkPrimApp (Prim.Trace_sourceMark, [constExp, constExp])
      val res4 = convertSourceMarkToStatic node4
      val _ = case res4 of
                 NONE => ()
               | SOME _ => Error.bug "Test 4 failed: should NOT have converted (multiple args)"

      (* TODO(pscollins): revisit if this is necessary *)
      (* Test 5: Trace_sourceMark with non-string constant *)
      (* val intConstExp = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.intInf 42), CoreML.Type.unit) *)
      (* val node5 = mkPrimApp (Prim.Trace_sourceMark, [intConstExp]) *)
      (* val res5 = convertSourceMarkToStatic node5 *)
      (* val _ = case res5 of *)
      (*            NONE => () *)
      (*          | SOME _ => Error.bug "Test 5 failed: should NOT have converted (non-string constant)" *)
   in
      ()
   end

val _ = print "Testing InlineTrace.inlineTrace...\n"

val _ =
   let
      open Atoms
      val ty = CoreML.Type.unit
      val stringTy = TypeEnv.Type.unresolvedString ()

      fun mkValDec (var, exp) =
         CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = exp,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (var, CoreML.Exp.ty exp),
               regionPat = Region.bogus
            }
         }

      fun mkApp (f, a) = CoreML.Exp.make (CoreML.Exp.App (f, a), ty)

      fun mkPrimApp (prim, args) =
         CoreML.Exp.make (CoreML.Exp.PrimApp {
            args = Vector.fromList args,
            prim = prim,
            targs = Vector.new0 ()
         }, ty)

      fun isTraceSourceMarkExp e =
         case CoreML.Exp.node e of
            CoreML.Exp.PrimApp {prim, ...} => Prim.equals (prim, Prim.Trace_sourceMark)
          | _ => false

      val sourceMarkVar = Var.newString "sourceMark"
      val xVar = Var.newString "x"
      
      (* val sourceMark = fn x => Trace_sourceMark x *)
      val sourceMarkBody = mkPrimApp (Prim.Trace_sourceMark, [CoreML.Exp.var (xVar, stringTy)])
      val sourceMarkLambda = CoreML.Lambda.make {
         arg = xVar,
         argType = stringTy,
         body = sourceMarkBody,
         mayInline = true
      }
      val sourceMarkDec = mkValDec (sourceMarkVar, CoreML.Exp.lambda sourceMarkLambda)

      (* Case 1: Simple Inline *)
      val mark1Const = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string "mark1"), stringTy)
      val callExp1 = mkApp (CoreML.Exp.var (sourceMarkVar, CoreML.Type.arrow (stringTy, ty)), mark1Const)
      val unusedVar1 = Var.newString "u1"
      val callDec1 = mkValDec (unusedVar1, callExp1)

      val prog1 = Vector.fromList [[sourceMarkDec, callDec1]]
      val {prog = resProg1} = InlineTrace.inlineTrace {prog = prog1}

      val inlined1 = 
         case Vector.sub (resProg1, 0) of
            [_, CoreML.Dec.Val {vbs, ...}] => isTraceSourceMarkExp (#exp (Vector.sub (vbs, 0)))
          | _ => false
      val _ = if inlined1 then () else Error.bug "InlineTrace Case 1 failed: sourceMark call NOT inlined"

      (* Case 2: Multiple usages *)
      val mark2Const = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string "mark2"), stringTy)
      val callExp2 = mkApp (CoreML.Exp.var (sourceMarkVar, CoreML.Type.arrow (stringTy, ty)), mark2Const)
      val unusedVar2 = Var.newString "u2"
      val callDec2 = mkValDec (unusedVar2, callExp2)

      val prog2 = Vector.fromList [[sourceMarkDec, callDec1, callDec2]]
      val {prog = resProg2} = InlineTrace.inlineTrace {prog = prog2}

      val inlined2 = 
         case Vector.sub (resProg2, 0) of
            [_, CoreML.Dec.Val {vbs = vbs1, ...}, CoreML.Dec.Val {vbs = vbs2, ...}] => 
               isTraceSourceMarkExp (#exp (Vector.sub (vbs1, 0))) andalso
               isTraceSourceMarkExp (#exp (Vector.sub (vbs2, 0)))
          | _ => false
      val _ = if inlined2 then () else Error.bug "InlineTrace Case 2 failed: multiple sourceMark calls NOT inlined"

      (* Case 3: Nested inline (in Let) *)
      val letExp = CoreML.Exp.make (CoreML.Exp.Let (Vector.new1 callDec1, callExp2), ty)
      val unusedVar3 = Var.newString "u3"
      val letDec = mkValDec (unusedVar3, letExp)

      val prog3 = Vector.fromList [[sourceMarkDec, letDec]]
      val {prog = resProg3} = InlineTrace.inlineTrace {prog = prog3}

      val inlined3 =
         case Vector.sub (resProg3, 0) of
            [_, CoreML.Dec.Val {vbs, ...}] =>
               (case CoreML.Exp.node (#exp (Vector.sub (vbs, 0))) of
                   CoreML.Exp.Let (decs, body) =>
                      (case Vector.sub (decs, 0) of
                          CoreML.Dec.Val {vbs = innerVbs, ...} => isTraceSourceMarkExp (#exp (Vector.sub (innerVbs, 0)))
                        | _ => false)
                      andalso isTraceSourceMarkExp body
                 | _ => false)
          | _ => false
      val _ = if inlined3 then () else Error.bug "InlineTrace Case 3 failed: nested sourceMark calls NOT inlined"

      (* Case 4: Non-matching Lambda (too complex) *)
      (* val complexMark = fn x => (print x; Trace_sourceMark x) *)
      val complexMarkVar = Var.newString "complexMark"
      val complexMarkBody = CoreML.Exp.make (
         CoreML.Exp.Seq (Vector.fromList [
            mkApp (CoreML.Exp.var (Var.newString "print", CoreML.Type.arrow (stringTy, ty)), CoreML.Exp.var (xVar, stringTy)),
            sourceMarkBody
         ]), ty)
      val complexMarkLambda = CoreML.Lambda.make {
         arg = xVar,
         argType = stringTy,
         body = complexMarkBody,
         mayInline = true
      }
      val complexMarkDec = mkValDec (complexMarkVar, CoreML.Exp.lambda complexMarkLambda)
      val complexCallExp = mkApp (CoreML.Exp.var (complexMarkVar, CoreML.Type.arrow (stringTy, ty)), mark1Const)
      val complexCallDec = mkValDec (unusedVar1, complexCallExp)

      val prog4 = Vector.fromList [[complexMarkDec, complexCallDec]]
      val {prog = resProg4} = InlineTrace.inlineTrace {prog = prog4}

      val notInlined4 =
         case Vector.sub (resProg4, 0) of
            [_, CoreML.Dec.Val {vbs, ...}] =>
               (case CoreML.Exp.node (#exp (Vector.sub (vbs, 0))) of
                   CoreML.Exp.App _ => true
                 | _ => false)
          | _ => false
      val _ = if notInlined4 then () else Error.bug "InlineTrace Case 4 failed: complex mark should NOT be inlined"

      (* Case 5: Non-matching call (different variable) *)
      val otherVar = Var.newString "other"
      val otherDec = mkValDec (otherVar, CoreML.Exp.lambda sourceMarkLambda)
      val otherCallExp = mkApp (CoreML.Exp.var (otherVar, CoreML.Type.arrow (stringTy, ty)), mark1Const)
      val otherCallDec = mkValDec (unusedVar1, otherCallExp)

      (* Here we only provide sourceMarkDec as the source of inlining.
         Wait, InlineTrace.inlineTrace should find ALL suitable bindings.
         But if otherVar also matches the pattern, it should also be inlined.
         The comment says: "Finds ANY instances of a pattern like..."
         So otherVar should also be inlined if it matches the pattern.
      *)

      (* Let's test a call to something that DOES NOT match the pattern *)
      val identityVar = Var.newString "identity"
      val identityLambda = CoreML.Lambda.make {
         arg = xVar,
         argType = stringTy,
         body = CoreML.Exp.var (xVar, stringTy),
         mayInline = true
      }
      val identityDec = mkValDec (identityVar, CoreML.Exp.lambda identityLambda)
      val identityCallExp = mkApp (CoreML.Exp.var (identityVar, CoreML.Type.arrow (stringTy, stringTy)), mark1Const)
      val identityCallDec = mkValDec (unusedVar1, identityCallExp)

      val prog5 = Vector.fromList [[identityDec, identityCallDec]]
      val {prog = resProg5} = InlineTrace.inlineTrace {prog = prog5}

      val notInlined5 =
         case Vector.sub (resProg5, 0) of
            [_, CoreML.Dec.Val {vbs, ...}] =>
               (case CoreML.Exp.node (#exp (Vector.sub (vbs, 0))) of
                   CoreML.Exp.App _ => true
                 | _ => false)
          | _ => false
      val _ = if notInlined5 then () else Error.bug "InlineTrace Case 5 failed: identity call should NOT be inlined"

   in
      ()
   end

val _ = print "Testing AnnotateTrace.annotateTrace...\n"

val _ =
   let
      open Atoms
      val ty = CoreML.Type.unit
      val stringTy = TypeEnv.Type.unresolvedString ()

      fun mkValDec (var, exp) =
         CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = exp,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (var, CoreML.Exp.ty exp),
               regionPat = Region.bogus
            }
         }

      fun mkPrimApp (prim, args) =
         CoreML.Exp.make (CoreML.Exp.PrimApp {
            args = Vector.fromList args,
            prim = prim,
            targs = Vector.new0 ()
         }, ty)

      fun isTraceStaticSourceMarkExp (e, expectedName) =
         case CoreML.Exp.node e of
            CoreML.Exp.PrimApp {prim, args, ...} =>
               (case prim of
                   Prim.Trace_staticSourceMark s => 
                      s = expectedName andalso Vector.length args = 0
                 | _ => false)
          | _ => false

      (* Case 1: Simple replacement *)
      val mark1Name = "mark1"
      val mark1Const = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string mark1Name), stringTy)
      val sourceMarkExp1 = mkPrimApp (Prim.Trace_sourceMark, [mark1Const])
      val unusedVar1 = Var.newString "u1"
      val dec1 = mkValDec (unusedVar1, sourceMarkExp1)

      val prog1 = Vector.fromList [[dec1]]
      val {prog = resProg1} = AnnotateTrace.annotateTrace {prog = prog1}

      val annotated1 = 
         case Vector.sub (resProg1, 0) of
            [CoreML.Dec.Val {vbs, ...}] => isTraceStaticSourceMarkExp (#exp (Vector.sub (vbs, 0)), mark1Name)
          | _ => false
      val _ = if annotated1 then () else Error.bug "AnnotateTrace Case 1 failed: sourceMark NOT annotated"

      (* Case 2: Nested replacement (in Let) *)
      val mark2Name = "mark2"
      val mark2Const = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string mark2Name), stringTy)
      val sourceMarkExp2 = mkPrimApp (Prim.Trace_sourceMark, [mark2Const])
      val letExp = CoreML.Exp.make (CoreML.Exp.Let (Vector.new1 dec1, sourceMarkExp2), ty)
      val unusedVar2 = Var.newString "u2"
      val letDec = mkValDec (unusedVar2, letExp)

      val prog2 = Vector.fromList [[letDec]]
      val {prog = resProg2} = AnnotateTrace.annotateTrace {prog = prog2}

      val annotated2 =
         case Vector.sub (resProg2, 0) of
            [CoreML.Dec.Val {vbs, ...}] =>
               (case CoreML.Exp.node (#exp (Vector.sub (vbs, 0))) of
                   CoreML.Exp.Let (decs, body) =>
                      (case Vector.sub (decs, 0) of
                          CoreML.Dec.Val {vbs = innerVbs, ...} => isTraceStaticSourceMarkExp (#exp (Vector.sub (innerVbs, 0)), mark1Name)
                        | _ => false)
                      andalso isTraceStaticSourceMarkExp (body, mark2Name)
                 | _ => false)
          | _ => false
      val _ = if annotated2 then () else Error.bug "AnnotateTrace Case 2 failed: nested sourceMark NOT annotated"

      (* Case 3: Multiple replacements in one list *)
      val prog3 = Vector.fromList [[dec1, mkValDec (unusedVar2, sourceMarkExp2)]]
      val {prog = resProg3} = AnnotateTrace.annotateTrace {prog = prog3}

      val annotated3 =
         case Vector.sub (resProg3, 0) of
            [CoreML.Dec.Val {vbs = vbs1, ...}, CoreML.Dec.Val {vbs = vbs2, ...}] =>
               isTraceStaticSourceMarkExp (#exp (Vector.sub (vbs1, 0)), mark1Name) andalso
               isTraceStaticSourceMarkExp (#exp (Vector.sub (vbs2, 0)), mark2Name)
          | _ => false
      val _ = if annotated3 then () else Error.bug "AnnotateTrace Case 3 failed: multiple sourceMarks NOT annotated"

      (* Case 4: Non-constant Trace_sourceMark (should not be annotated) *)
      val varExp = CoreML.Exp.var (Var.newString "v", stringTy)
      val dynamicSourceMarkExp = mkPrimApp (Prim.Trace_sourceMark, [varExp])
      val dynamicDec = mkValDec (unusedVar1, dynamicSourceMarkExp)

      val prog4 = Vector.fromList [[dynamicDec]]
      val {prog = resProg4} = AnnotateTrace.annotateTrace {prog = prog4}

      val notAnnotated4 =
         case Vector.sub (resProg4, 0) of
            [CoreML.Dec.Val {vbs, ...}] =>
               (case CoreML.Exp.node (#exp (Vector.sub (vbs, 0))) of
                   CoreML.Exp.PrimApp {prim, ...} => Prim.equals (prim, Prim.Trace_sourceMark)
                 | _ => false)
          | _ => false
      val _ = if notAnnotated4 then () else Error.bug "AnnotateTrace Case 4 failed: dynamic sourceMark SHOULD NOT be annotated"

   in
      ()
   end

val _ = print "Testing AnnotateTraceValue.annotateTraceValue...\n"

val _ =
   let
      open Atoms
      val ty = CoreML.Type.unit
      val stringTy = TypeEnv.Type.unresolvedString ()
      val talpha = CoreML.Type.unit (* Placeholder for 'a *)

      fun mkValDec (var, exp) =
         CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = exp,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = CoreML.Pat.var (var, CoreML.Exp.ty exp),
               regionPat = Region.bogus
            }
         }

      fun mkPrimApp (prim, args, targs) =
         CoreML.Exp.make (CoreML.Exp.PrimApp {
            args = Vector.fromList args,
            prim = prim,
            targs = Vector.fromList targs
         }, ty)

      fun isTraceStaticSourceMarkValueExp (e, expectedName) =
         case CoreML.Exp.node e of
            CoreML.Exp.PrimApp {prim, args, ...} =>
               (case prim of
                   Prim.Trace_staticSourceMarkValue s => 
                      s = expectedName andalso Vector.length args = 1
                 | _ => false)
          | _ => false

      val sourceMarkValueVar = Var.newString "sourceMarkValue"
      val x = Var.newString "x"
      val xVal = Var.newString "x_val"
      val xMark = Var.newString "x_mark"
      val tupleTy = CoreML.Type.tuple (Vector.new2 (talpha, stringTy))
      
      (* Wrapper: fn x => case x of (xv, xm) => Trace_sourceMarkValue (xv, xm) *)
      val primApp = mkPrimApp (Prim.Trace_sourceMarkValue, 
                              [CoreML.Exp.var (xVal, talpha), CoreML.Exp.var (xMark, stringTy)],
                              [talpha])
      val rule = {
         exp = primApp,
         layPat = NONE,
         pat = CoreML.Pat.tuple (Vector.new2 (CoreML.Pat.var (xVal, talpha), CoreML.Pat.var (xMark, stringTy))),
         regionPat = Region.bogus
      }
      val caseExp = CoreML.Exp.make (
         CoreML.Exp.Case {
            ctxt = fn () => Layout.empty,
            kind = ("sourceMarkValue", ""),
            nest = [],
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            noMatch = CoreML.Exp.Impossible,
            region = Region.bogus,
            rules = Vector.new1 rule,
            test = CoreML.Exp.var (x, tupleTy)
         }, ty)
      val lam = CoreML.Lambda.make {
         arg = x,
         argType = tupleTy,
         body = caseExp,
         mayInline = true
      }
      val sourceMarkValueDec = mkValDec (sourceMarkValueVar, CoreML.Exp.lambda lam)

      (* Call: sourceMarkValue (val, "mark") *)
      val markName = "test_mark"
      val valVar = Var.newString "v"
      val valExp = CoreML.Exp.var (valVar, talpha)
      val markConst = CoreML.Exp.make (CoreML.Exp.Const (fn () => Const.string markName), stringTy)
      val tupleArg = CoreML.Exp.tuple (Vector.new2 (valExp, markConst))
      val callExp = CoreML.Exp.make (
         CoreML.Exp.App (
            CoreML.Exp.make (
               CoreML.Exp.Var (fn () => sourceMarkValueVar, fn () => Vector.new1 talpha),
               CoreML.Type.arrow (tupleTy, ty)),
            tupleArg
         ), ty)
      val callDec = mkValDec (Var.newString "u", callExp)

      val prog = Vector.fromList [[sourceMarkValueDec, callDec]]
      val {prog = resProg} = AnnotateTraceValue.annotateTraceValue {prog = prog}

      val annotated = 
         case Vector.sub (resProg, 0) of
            [_, CoreML.Dec.Val {vbs, ...}] => isTraceStaticSourceMarkValueExp (#exp (Vector.sub (vbs, 0)), markName)
          | _ => false
      val _ = if annotated then () else Error.bug "AnnotateTraceValue failed: sourceMarkValue call NOT annotated"
   in
      ()
   end

val _ = print "Testing isSourceMarkValueExp...\n"

val _ =
   let
      open Atoms
      val ty = CoreML.Type.unit
      val stringTy = TypeEnv.Type.unresolvedString ()
      val talpha = CoreML.Type.unit (* Placeholder for 'a *)

      val x = Var.newString "x"
      val xVal = Var.newString "x_val"
      val xMark = Var.newString "x_mark"

      (* 
         (fn x: 'a * string => 
            case x of 
               (x_val: 'a, x_mark: string) => 
               Trace_sourceMarkValue['a] (x_val, x_mark))
      *)

      val tupleTy = CoreML.Type.tuple (Vector.new2 (talpha, stringTy))
      
      val primApp = CoreML.Exp.make (
         CoreML.Exp.PrimApp {
            args = Vector.new2 (CoreML.Exp.var (xVal, talpha), CoreML.Exp.var (xMark, stringTy)),
            prim = Prim.Trace_sourceMarkValue,
            targs = Vector.new1 talpha
         }, ty)

      val rule = {
         exp = primApp,
         layPat = NONE,
         pat = CoreML.Pat.tuple (Vector.new2 (CoreML.Pat.var (xVal, talpha), CoreML.Pat.var (xMark, stringTy))),
         regionPat = Region.bogus
      }

      val caseExp = CoreML.Exp.make (
         CoreML.Exp.Case {
            ctxt = fn () => Layout.empty,
            kind = ("sourceMarkValue", ""),
            nest = [],
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            noMatch = CoreML.Exp.Impossible,
            region = Region.bogus,
            rules = Vector.new1 rule,
            test = CoreML.Exp.var (x, tupleTy)
         }, ty)

      val lam = CoreML.Lambda.make {
         arg = x,
         argType = tupleTy,
         body = caseExp,
         mayInline = true
      }

      val exp = CoreML.Exp.lambda lam

      val _ = if isSourceMarkValueExp exp then () else Error.bug "isSourceMarkValueExp failed: should be true"
   in
      ()
   end

val _ = print "Testing toVerboseString...\n"

val _ =
   let
      open Atoms
      val x = Var.newString "x"
      val ty = CoreML.Type.unit
      
      val expX = CoreML.Exp.var (x, ty)
      val s = toVerboseStringExp expX
      val _ = print (s ^ "\n")
      (* Expecting something like: Exp.make (Exp.Var (x_0, []), unit) *)
      val _ = if String.hasSubstring (s, {substring = "Exp.Var (x"}) then () else Error.bug "toVerboseStringExp failed"

      val patX = CoreML.Pat.var (x, ty)
      val s = toVerboseStringPat patX
      val _ = print (s ^ "\n")
      (* Expecting something like: Pat.make (Pat.Var x_0, unit) *)
      val _ = if String.hasSubstring (s, {substring = "Pat.Var x"}) then () else Error.bug "toVerboseStringPat failed"

      val dec = CoreML.Dec.Val {
            matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                          redundant = Control.Elaborate.DiagEIW.Ignore},
            rvbs = Vector.new0 (),
            tyvars = fn () => Vector.new0 (),
            vbs = Vector.new1 {
               ctxt = fn () => Layout.empty,
               exp = expX,
               layPat = fn () => Layout.empty,
               nest = [],
               pat = patX,
               regionPat = Region.bogus
            }
         }
      val s = toVerboseStringDec dec
      val _ = print (s ^ "\n")
      val _ = if String.hasSubstring (s, {substring = "Dec.Val"}) then () else Error.bug "toVerboseStringDec failed"
      val _ = if String.hasSubstring (s, {substring = "rvbs = []"}) then () else Error.bug "toVerboseStringDec failed (rvbs)"
      val _ = if String.hasSubstring (s, {substring = "tyvars = []"}) then () else Error.bug "toVerboseStringDec failed (tyvars)"
      val _ = if String.hasSubstring (s, {substring = "vbs = [{exp ="}) then () else Error.bug "toVerboseStringDec failed (vbs)"

      val prog = [dec]
      val s = toVerboseStringDecs prog
      val _ = print (s ^ "\n")
      val _ = if String.hasSubstring (s, {substring = "Dec.Val"}) then () else Error.bug "toVerboseStringDecs failed"
   in
      ()
   end

val _ = print "Tests Passed\n"
