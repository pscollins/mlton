(* Copyright (C) 2025 MLton.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CoreMLUtil (S: CORE_ML_UTIL_STRUCTS): CORE_ML_UTIL =
struct

open S
open CoreML

structure VarSet =
   UniqueSet (structure Element = Var
              val cacheSize = 1024
              val bits = 10)

fun setContains (vs: VarSet.t) (v: Var.t): bool = let
   val single = VarSet.singleton v
   val intersect = VarSet.intersect (vs, single)
in
   not (VarSet.isEmpty intersect)
end

(* Implementation detail of {,recursive}CollectVarsBoundToPred: `predWithVarSet`
wraps a predicate and allows the user to check the existing `VarSet.t` as part
of the decision. *)
fun collectVarsBoundToPredImpl (prog: Dec.t list vector,
                                predWithVarSet: VarSet.t * Exp.t -> bool) = let
   fun extractVb {exp, pat, ...}: (Var.t * Exp.t) option =
       case Pat.dest pat of
           (Pat.Var v, _) => SOME (v, exp)
         |  _ => NONE
   fun extractUniqueVb vbs: (Var.t * Exp.t) option =
       if Vector.length vbs = 1
       then extractVb (Vector.first vbs)
       else NONE
   fun extractUniqueDecBinding (dec: Dec.t): (Var.t * Exp.t) option =
       case dec of
           Dec.Val {vbs, ...} => extractUniqueVb vbs
         | _ => NONE
   fun collectVarsInDec (dec: Dec.t, vs: VarSet.t): VarSet.t =
       case (extractUniqueDecBinding dec) of
           NONE => vs
         | SOME (var, bind) =>
           if (predWithVarSet (vs, bind)) then VarSet.+ (vs, VarSet.singleton var)
           else vs
   fun collectVarsInDecs (decs: Dec.t list, vs: VarSet.t): VarSet.t =
       List.fold (decs, vs, collectVarsInDec)
in
   Vector.fold (prog, VarSet.empty, collectVarsInDecs)
end

fun collectVarsBoundToPred (prog: Dec.t list vector, pred) = let
   fun wrapPred (vs: VarSet.t, exp: Exp.t): bool =
       (* ignore `vs` *)
       pred exp
in
   collectVarsBoundToPredImpl (prog, wrapPred)
end

(* Implementation detail of `inlineSourceMarkCall`: checks if `exp` is a `Var`
node that blongs to the provided `VarSet`. *)
fun isTargetVarExp (vs: VarSet.t) (exp: Exp.t): bool =
    case Exp.node exp of
        Exp.Var (getVar, getTypes) => setContains vs (getVar())
     |  _ => false

fun recursiveCollectVarsBoundToPred (prog, pred): VarSet.t = let
   fun wrapPred (vs: VarSet.t, exp: Exp.t): bool =
       if isTargetVarExp vs exp then
          (* expand through aliases *)
          true
       else
          (* otherwise use the regular `pred` *)
          pred exp
in
   collectVarsBoundToPredImpl (prog, wrapPred)
end

fun mapExps (prog: Dec.t list vector, rewrite: Exp.node -> Exp.node option):
    CoreML.Dec.t list vector = let
      val currStmt = ref 0
      fun doExp (exp: Exp.t) = let
         val (node: Exp.node, ty: Type.t) = Exp.dest exp
         fun doCaseRules rules = let
            fun doCaseRule {exp, layPat, pat, regionPat} =
                {exp = doExp exp,
                 layPat = layPat,
                 pat = pat,
                 regionPat = regionPat}
         in
            Vector.map (rules, doCaseRule)
         end
         fun doExpNodeImpl (expNode: Exp.node): Exp.node =
             case expNode of
                  Exp.App (func, arg) =>
                  Exp.App (doExp func, doExp arg)
               | Exp.Case {
                    ctxt, kind, nest, matchDiags,
                    noMatch, region, rules, test} =>
                 Exp.Case {
                    ctxt = ctxt, kind = kind, nest = nest,
                    matchDiags = matchDiags, noMatch = noMatch,
                    region = region, rules = doCaseRules rules,
                    test = doExp test}
               | Exp.Con _ => expNode (* constructor can't contain Exp.node *)
               | Exp.Const _ => expNode     (* const can't contain Exp.node *)
               | Exp.EnterLeave (exp, loc) =>
                 Exp.EnterLeave (doExp exp, loc)
               | Exp.Handle {catch, handler, try} =>
                 Exp.Handle {catch = catch,
                             handler = doExp handler,
                             try = doExp try}
               | Exp.Lambda l => Exp.Lambda (doLambda l)
               | Exp.Let (decs, exp) =>
                 Exp.Let (Vector.map (decs, doDec), doExp exp)
               | Exp.List exps => Exp.List (Vector.map (exps, doExp))
               | Exp.PrimApp {args, prim, targs} =>
                 Exp.PrimApp {args = Vector.map (args, doExp),
                              prim = prim,
                              targs = targs}
               | Exp.Raise exp => Exp.Raise (doExp exp)
               | Exp.Record elts => Exp.Record (Record.map (elts, doExp))
               | Exp.Seq exps => Exp.Seq (Vector.map (exps, doExp))
               | Exp.Var _ => expNode (* no inner Exp.node *)
               | Exp.Vector exps => Exp.Vector (Vector.map (exps, doExp))
         fun doExpNode (expNode: Exp.node): Exp.node =
             case (rewrite expNode) of
               (* The rewrite matched, apply it *)
                SOME result => result
              (* Otheerwise, recurse *)
              | NONE => doExpNodeImpl expNode

      in
         Exp.make (doExpNode node, ty)
      end
      and doLambda (l: Lambda.t) =
         let
            val {arg, argType, body, mayInline} = Lambda.dest l
         in
            Lambda.make {arg = arg, argType = argType, body = doExp body, mayInline = mayInline}
         end
      and doVbs vbs = let
         (* value binding *)
         fun doVb {ctxt, exp, layPat, nest, pat, regionPat} =
             {ctxt = ctxt,
              exp = doExp exp,
              layPat = layPat,
              nest = nest,
              pat = pat,
              regionPat = regionPat}
      in
         Vector.map (vbs, doVb)
      end
      and doRvbs rvbs = let
         (* "recursive value binding" *)
         fun doRvb {lambda, var} =
             {lambda = doLambda lambda, var = var}
      in
         Vector.map (rvbs, doRvb)
      end
      and doFun {decs, tyvars} = let
         fun doFunDec {lambda, var} = {lambda = doLambda lambda, var = var}
      in
         {decs = Vector.map (decs, doFunDec), tyvars = tyvars}
      end
      and doDec (dec: Dec.t): Dec.t =
          case dec of
              Dec.Val {matchDiags, rvbs, tyvars, vbs} =>
              Dec.Val {matchDiags = matchDiags,
                       rvbs = doRvbs rvbs,
                       tyvars = tyvars,
                       vbs = doVbs vbs}
            | Dec.Fun funDec => Dec.Fun (doFun funDec)
            | Dec.Datatype _ => dec  (* no Exp.node *)
            | Dec.Exception _ => dec (* no Exp.node *)
      fun doDecs (decs: Dec.t list): Dec.t list = List.map (decs, doDec)
   in
      Vector.map (prog, doDecs)
   end

fun inlineSourceMarkCall (vs: VarSet.t) (node: Exp.node): Exp.node option = let
   val isTargetVarExp = isTargetVarExp vs
in
   case node of
       Exp.App (func, arg) =>
       if isTargetVarExp func then
          SOME (Exp.PrimApp {args = Vector.new1 (arg),
                             prim = Prim.Trace_sourceMark,
                             targs = Vector.new0 ()})
       else NONE
    |  _ => NONE
end

(* If `exp` is a `Const`, returns the value of the `const` as a string, with
leading/trailing `"` characters dropped. Otherwise, returns NONE *)
fun getConstStr (exp: Exp.t) = let
   fun getCleanName getConst = let
      val s = Const.toString (getConst())
   in
      (* The `Const` name shows up as `"name"` rather than `name`: drop it here. *)
      String.substring (s, 1, String.length s - 2)
   end
in
   case Exp.node exp of
       (* TODO(pscollins): Validate that the type is actually `string`. I
           think at this point we have already type-checked so it doesn't really
           matter. *)
       Exp.Const getConst => SOME (getCleanName getConst)
           | _ => NONE
     end
     
     fun convertSourceMarkValueToStatic
             (vs: VarSet.t) (exp: Exp.node): Exp.node option = let
        fun unpackFieldPair (arg, maybeName) =       case getConstStr maybeName of
           SOME name => SOME (arg, name)
         | _ => NONE
   fun unpackFields fields =
       if Vector.length fields = 2 then
       unpackFieldPair (Vector.first fields, Vector.last fields)
       else NONE
   fun unpackRecord record =
       case Record.detupleOpt record of
           SOME (fields) => unpackFields fields
        |  _ => NONE
   fun extractArgs (arg: Exp.t): (Exp.t * string) option =
       case Exp.node arg of
           Exp.Record record => unpackRecord record
        |  _ =>  NONE
   fun maybeBuildStaticValueMark arg =
       case extractArgs arg of
           SOME (dynArg, markName) =>
           SOME (Exp.PrimApp {args = Vector.new1 dynArg,
                              prim = Prim.Trace_staticSourceMarkValue markName,
                              (* Type argument follows the type of the dynamic arg *)
                              targs = Vector.new1 (Exp.ty dynArg)})
           | _ => NONE
in
   case exp of
       Exp.App (func, arg) =>
       if isTargetVarExp vs func then
          maybeBuildStaticValueMark arg
       else NONE
    |  _ => NONE

end

fun convertSourceMarkToStatic (node: Exp.node): Exp.node option = let
   fun maybeBuildStaticMark (arg: string option) =
       case arg of
           SOME str => SOME (Exp.PrimApp {args = Vector.new0(),
                                          prim = Prim.Trace_staticSourceMark str,
                                          targs = Vector.new0()})
         | _ => NONE
in
   case node of
       Exp.PrimApp {args = args, prim = Prim.Trace_sourceMark, targs = targs} =>
       if Vector.length args = 1 andalso
          Vector.length targs = 0 then
          maybeBuildStaticMark (getConstStr (Vector.first args))
       else NONE
     | _ => NONE
end

fun isSourceMarkValueExp (exp: Exp.t): bool = let
   fun matchRule {exp, ...} =
       case Exp.node exp of
           Exp.PrimApp {args = args,
                        prim = Prim.Trace_sourceMarkValue, ...} =>
           true
        |  _ =>  false
   fun matchRules rules =
       if Vector.length rules = 1 then
          matchRule (Vector.first rules)
       else false
   fun matchLambdaBody {body, ...} =
       case Exp.node body of
           Exp.Case {rules, ...} => matchRules rules
         | _ => false
in
   case Exp.node exp of
       Exp.Lambda lambda => matchLambdaBody (Lambda.dest lambda)
    | _ => false
end

fun v2l (v, f) = Layout.list (Vector.toList (Vector.map (v, f)))
fun o2l (opt, f) =
   case opt of
      NONE => Layout.str "NONE"
    | SOME x => Layout.seq [Layout.str "SOME ", Layout.paren (f x)]

fun ty2l ty = Type.layout ty
fun var2l v = Layout.str (Var.toString v)
fun con2l c = Layout.str (Con.toString c)
fun tycon2l t = Layout.str (Tycon.toString t)
fun tyvar2l t = Layout.str (Tyvar.toString t)
fun const2l c = Layout.str (Const.toString (c ()))
fun field2l f = Layout.str (Field.toString f)

fun record2l (r, f) =
   let
      val elts = Record.toVector r
      fun elt2l (field, x) = Layout.tuple [field2l field, f x]
   in
      v2l (elts, elt2l)
   end

fun pat2l p =
   let val (node, ty) = Pat.dest p
   in Layout.seq [Layout.str "Pat.make ", Layout.tuple [patNode2l node, ty2l ty]]
   end
and patNode2l node =
   case node of
      Pat.Con {arg, con, targs} =>
         Layout.namedRecord ("Pat.Con", [("arg", o2l (arg, pat2l)),
                                         ("con", con2l con),
                                         ("targs", v2l (targs, ty2l))])
    | Pat.Const c => Layout.seq [Layout.str "Pat.Const ", Layout.paren (const2l c)]
    | Pat.Layered (v, p) => Layout.seq [Layout.str "Pat.Layered ", Layout.tuple [var2l v, pat2l p]]
    | Pat.List ps => Layout.seq [Layout.str "Pat.List ", v2l (ps, pat2l)]
    | Pat.Or ps => Layout.seq [Layout.str "Pat.Or ", v2l (ps, pat2l)]
    | Pat.Record r => Layout.seq [Layout.str "Pat.Record ", record2l (r, pat2l)]
    | Pat.Var v => Layout.seq [Layout.str "Pat.Var ", var2l v]
    | Pat.Vector ps => Layout.seq [Layout.str "Pat.Vector ", v2l (ps, pat2l)]
    | Pat.Wild => Layout.str "Pat.Wild"

and exp2l e =
   let val (node, ty) = Exp.dest e
   in Layout.seq [Layout.str "Exp.make ", Layout.tuple [expNode2l node, ty2l ty]]
   end
and expNode2l node =
   case node of
      Exp.App (func, arg) =>
         Layout.namedRecord ("Exp.App", [("func", exp2l func), ("arg", exp2l arg)])
    | Exp.Case {rules, test, ...} =>
         Layout.namedRecord ("Exp.Case", [("rules", v2l (rules, rule2l)), ("test", exp2l test)])
    | Exp.Con (con, targs) =>
         Layout.seq [Layout.str "Exp.Con ", Layout.tuple [con2l con, v2l (targs, ty2l)]]
    | Exp.Const c => Layout.seq [Layout.str "Exp.Const ", Layout.paren (const2l c)]
    | Exp.EnterLeave (e, _) => Layout.seq [Layout.str "Exp.EnterLeave ", Layout.tuple [exp2l e, Layout.str "..."]]
    | Exp.Handle {catch=(v, ty), handler, try} =>
         Layout.namedRecord ("Exp.Handle", [("catch", Layout.tuple [var2l v, ty2l ty]),
                                            ("handler", exp2l handler),
                                            ("try", exp2l try)])
    | Exp.Lambda l => Layout.seq [Layout.str "Exp.Lambda ", Layout.paren (lambda2l l)]
    | Exp.Let (decs, e) =>
         Layout.namedRecord ("Exp.Let", [("decs", v2l (decs, dec2l)), ("exp", exp2l e)])
    | Exp.List es => Layout.seq [Layout.str "Exp.List ", v2l (es, exp2l)]
    | Exp.PrimApp {args, prim, targs} =>
         Layout.namedRecord ("Exp.PrimApp", [("args", v2l (args, exp2l)),
                                             ("prim", Layout.str (Prim.toString prim)),
                                             ("targs", v2l (targs, ty2l))])
    | Exp.Raise e => Layout.seq [Layout.str "Exp.Raise ", Layout.paren (exp2l e)]
    | Exp.Record r => Layout.seq [Layout.str "Exp.Record ", record2l (r, exp2l)]
    | Exp.Seq es => Layout.seq [Layout.str "Exp.Seq ", v2l (es, exp2l)]
    | Exp.Var (v, targs) => Layout.seq [Layout.str "Exp.Var ", Layout.tuple [var2l (v ()), v2l (targs (), ty2l)]]
    | Exp.Vector es => Layout.seq [Layout.str "Exp.Vector ", v2l (es, exp2l)]

and rule2l {exp, pat, ...} =
   Layout.record [("exp", exp2l exp), ("pat", pat2l pat)]

and lambda2l l =
   let val {arg, argType, body, ...} = Lambda.dest l
   in Layout.namedRecord ("Lambda.make", [("arg", var2l arg), ("argType", ty2l argType), ("body", exp2l body)])
   end

and dec2l d =
   let
      fun doTyvars tvs = v2l (tvs, tyvar2l)
      fun doCons cons =
         v2l (cons, fn {arg, con} =>
            Layout.record [("arg", o2l (arg, ty2l)),
                           ("con", con2l con)])
   in
      case d of
         Dec.Datatype v =>
            Layout.seq [Layout.str "Dec.Datatype ",
                        v2l (v, fn {cons, tycon, tyvars} =>
                           Layout.record [("cons", doCons cons),
                                          ("tycon", tycon2l tycon),
                                          ("tyvars", doTyvars tyvars)])]
    | Dec.Exception {arg, con, ...} =>
         Layout.namedRecord ("Dec.Exception", [("arg", o2l (arg, ty2l)), ("con", con2l con)])
    | Dec.Fun {decs, tyvars} =>
         Layout.namedRecord ("Dec.Fun", [("decs", v2l (decs, fn {lambda, var} =>
                                                           Layout.record [("lambda", lambda2l lambda),
                                                                          ("var", var2l var)])),
                                         ("tyvars", v2l (tyvars (), tyvar2l))])
    | Dec.Val {rvbs, vbs, tyvars, ...} =>
         Layout.namedRecord ("Dec.Val", [("rvbs", v2l (rvbs, fn {lambda, var} =>
                                                           Layout.record [("lambda", lambda2l lambda),
                                                                          ("var", var2l var)])),
                                         ("tyvars", v2l (tyvars (), tyvar2l)),
                                         ("vbs", v2l (vbs, fn {exp, pat, ...} =>
                                                           Layout.record [("exp", exp2l exp),
                                                                          ("pat", pat2l pat)]))])
   end

val toVerboseStringDec = Layout.toString o dec2l
fun toVerboseStringDecs decs = Layout.toString (Layout.align (List.map (decs, dec2l)))
val toVerboseStringExp = Layout.toString o exp2l
val toVerboseStringPat = Layout.toString o pat2l
val toVerboseStringType = Layout.toString o ty2l

fun verbosePrintDecs (decss: CoreML.Dec.t list vector) =  let
   val idx = ref 0
   fun decsToLayoutString (decs: CoreML.Dec.t list) =
       Layout.toString
           (Layout.align (List.map (decs, CoreML.Dec.layout)))
   fun decsToString (decs: CoreML.Dec.t list) = let
      val currIdx = !idx
      val _ = idx := currIdx + 1
      val idxStr = concat ["#", Int.toString currIdx]
   in
      (print o concat) ["Dec(", idxStr, ")=\n",
                        "\tLayout(", idxStr, ")=", decsToLayoutString decs, "\n",
                        "\tVerbose(", idxStr, ")=", toVerboseStringDecs decs, "\n",
                        "\n"]
   end
in
   Vector.foreach (decss, decsToString)
end

end
