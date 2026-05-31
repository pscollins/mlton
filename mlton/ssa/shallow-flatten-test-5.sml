local
   open Ssa

   fun assert (cond, msg) =
      if cond then () else raise TestFail msg

   fun statementEquals (Statement.T {exp=e1, ty=t1, var=v1},
                        Statement.T {exp=e2, ty=t2, var=v2}) =
      Exp.equals (e1, e2) andalso
      Type.equals (t1, t2) andalso
      (case (v1, v2) of
          (SOME v1', SOME v2') => Var.equals (v1', v2')
        | (NONE, NONE) => true
        | _ => false)

   fun assertType (Statement.T {ty, ...}, expected, msg) =
      if Type.equals (ty, expected) then ()
      else assert (false, msg ^ ": type mismatch")
in
(* Test 42: Deep flatten (2 levels of nesting) *)
   val _ = runTest ("Test 42: Deep flatten (2 levels of nesting)", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val policy = ShallowFlatten.MaxWidth 2
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      (* Type: (((int * int) array) * int) *)
      val argTy = Type.tuple (Vector.fromList [arrayTuple2Ty, intTy])
      
      val con1 = Con.fromString "Con1"
      val dt = Datatype.T {
         tycon = Tycon.fromString "t",
         cons = Vector.new1 {con = con1, args = Vector.new1 argTy}
      }
      
      val _ = ShallowFlatten.markDatatypeForPolicy (fv, policy) dt
      val dt' = ShallowFlatten.flattenDatatype fv dt
      
      val Datatype.T {cons = cons', ...} = dt'
      val {args = args', ...} = Vector.sub (cons', 0)
      val newArgTy = Vector.sub (args', 0)
      
      (* Expected: ((int array * int array) * int) *)
      val flattenedTuple2Ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val expectedTy = Type.tuple (Vector.fromList [flattenedTuple2Ty, intTy])
      
      val _ = assert (Type.equals (newArgTy, expectedTy), 
                      "Deep flatten 2 levels failed. Expected " ^ (Layout.toString (Type.layout expectedTy)) ^ 
                      " but got " ^ (Layout.toString (Type.layout newArgTy)))
      val _ = ShallowFlatten.destroyFlattenedVars fv
   in () end)(* Test 43: Deep flatten (3 levels of nesting) *)
   val _ = runTest ("Test 43: Deep flatten (3 levels of nesting)", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val policy = ShallowFlatten.MaxWidth 2
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      (* Type: ((((int * int) array) * int) * int) *)
      val nestedTy = Type.tuple (Vector.fromList [arrayTuple2Ty, intTy])
      val argTy = Type.tuple (Vector.fromList [nestedTy, intTy])
      
      val con1 = Con.fromString "Con1"
      val dt = Datatype.T {
         tycon = Tycon.fromString "t",
         cons = Vector.new1 {con = con1, args = Vector.new1 argTy}
      }
      
      val _ = ShallowFlatten.markDatatypeForPolicy (fv, policy) dt
      val dt' = ShallowFlatten.flattenDatatype fv dt
      
      val Datatype.T {cons = cons', ...} = dt'
      val {args = args', ...} = Vector.sub (cons', 0)
      val newArgTy = Vector.sub (args', 0)
      
      (* Expected: (((int array * int array) * int) * int) *)
      val flattenedTuple2Ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val expectedNestedTy = Type.tuple (Vector.fromList [flattenedTuple2Ty, intTy])
      val expectedTy = Type.tuple (Vector.fromList [expectedNestedTy, intTy])
      
      val _ = assert (Type.equals (newArgTy, expectedTy), 
                      "Deep flatten 3 levels failed. Expected " ^ (Layout.toString (Type.layout expectedTy)) ^ 
                      " but got " ^ (Layout.toString (Type.layout newArgTy)))
      val _ = ShallowFlatten.destroyFlattenedVars fv
   in () end)(* Test 44: Deep flatten (multiple flattened types) *)
   val _ = runTest ("Test 44: Deep flatten (multiple flattened types)", fn () => let
      val fv = ShallowFlatten.newFlattenedVars ()
      val policy = ShallowFlatten.MaxWidth 2
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      
      (* Type: (((int * int) array) * ((int * int) array)) *)
      val argTy = Type.tuple (Vector.fromList [arrayTuple2Ty, arrayTuple2Ty])
      
      val con1 = Con.fromString "Con1"
      val dt = Datatype.T {
         tycon = Tycon.fromString "t",
         cons = Vector.new1 {con = con1, args = Vector.new1 argTy}
      }
      
      val _ = ShallowFlatten.markDatatypeForPolicy (fv, policy) dt
      val dt' = ShallowFlatten.flattenDatatype fv dt
      
      val Datatype.T {cons = cons', ...} = dt'
      val {args = args', ...} = Vector.sub (cons', 0)
      val newArgTy = Vector.sub (args', 0)
      
      (* Expected: ((int array * int array) * (int array * int array)) *)
      val flattenedTuple2Ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val expectedTy = Type.tuple (Vector.fromList [flattenedTuple2Ty, flattenedTuple2Ty])
      
      val _ = assert (Type.equals (newArgTy, expectedTy), 
                      "Deep flatten multiple types failed. Expected " ^ (Layout.toString (Type.layout expectedTy)) ^ 
                      " but got " ^ (Layout.toString (Type.layout newArgTy)))
      val _ = ShallowFlatten.destroyFlattenedVars fv
   in () end)(* Test 45: Deep flatten (array + vector) *)
   val _ = runTest ("Test 45: Deep flatten (array + vector)", fn () => let
      (* TODO: vector is not currently supported *)
      val fv = ShallowFlatten.newFlattenedVars ()
      val policy = ShallowFlatten.MaxWidth 2
      val intTy = Type.intInf
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val arrayTuple2Ty = Type.array tuple2Ty
      val vectorTuple2Ty = Type.vector tuple2Ty
      
      (* Type: (((int * int) array) * ((int * int) vector)) *)
      val argTy = Type.tuple (Vector.fromList [arrayTuple2Ty, vectorTuple2Ty])
      
      val con1 = Con.fromString "Con1"
      val dt = Datatype.T {
         tycon = Tycon.fromString "t",
         cons = Vector.new1 {con = con1, args = Vector.new1 argTy}
      }
      
      val _ = ShallowFlatten.markDatatypeForPolicy (fv, policy) dt
      val dt' = ShallowFlatten.flattenDatatype fv dt
      
      val Datatype.T {cons = cons', ...} = dt'
      val {args = args', ...} = Vector.sub (cons', 0)
      val newArgTy = Vector.sub (args', 0)
      
      (* Expected: ((int array * int array) * (int vector * int vector)) *)
      val flattenedArray2Ty = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      val flattenedVector2Ty = Type.tuple (Vector.fromList [Type.vector intTy, Type.vector intTy])
      val expectedTy = Type.tuple (Vector.fromList [flattenedArray2Ty, flattenedVector2Ty])
      
      val _ = assert (Type.equals (newArgTy, expectedTy), 
                      "Deep flatten array + vector failed. Expected " ^ (Layout.toString (Type.layout expectedTy)) ^ 
                      " but got " ^ (Layout.toString (Type.layout newArgTy)))
      val _ = ShallowFlatten.destroyFlattenedVars fv
   in () end)(* Test 46: getConDecisionForPolicy *)
   val _ = runTest ("Test 46: getConDecisionForPolicy", fn () => let
      fun conDecisionToString cd =
         case cd of
            ShallowFlatten.PreserveNode v =>
               "PreserveNode[" ^ String.concatWith (Vector.toListMap (v, conDecisionToString), ", ") ^ "]"
          | ShallowFlatten.FlattenNode v =>
               "FlattenNode[" ^ String.concatWith (Vector.toListMap (v, conDecisionToString), ", ") ^ "]"

      fun cdEquals (c1, c2) = 
         case (c1, c2) of
            (ShallowFlatten.PreserveNode v1, ShallowFlatten.PreserveNode v2) =>
               Vector.length v1 = Vector.length v2 andalso
               let
                  fun loop i =
                     if i = Vector.length v1 then true
                     else cdEquals (Vector.sub (v1, i), Vector.sub (v2, i)) andalso loop (i + 1)
               in
                  loop 0
               end
          | (ShallowFlatten.FlattenNode v1, ShallowFlatten.FlattenNode v2) =>
               Vector.length v1 = Vector.length v2 andalso
               let
                  fun loop i =
                     if i = Vector.length v1 then true
                     else cdEquals (Vector.sub (v1, i), Vector.sub (v2, i)) andalso loop (i + 1)
               in
                  loop 0
               end
          | _ => false

      fun check (policy, ty, expected, msg) =
         let
            val res = ShallowFlatten.getConDecisionForPolicy policy ty
            val _ = print (concat ["Compare: ", Layout.toString (Type.layout ty),
                                   "\nexpected=",
                                   conDecisionToString expected,
                                   "\nactual  =",
                                   conDecisionToString res,
                                   "\n"])
         in
            if cdEquals (res, expected) then ()
            else assert (false, msg ^ ": conDecision mismatch")
         end

      val intTy = Type.intInf
      val policy2 = ShallowFlatten.MaxWidth 2
      
      fun preserve v = ShallowFlatten.PreserveNode (Vector.fromList v)
      fun flatten v = ShallowFlatten.FlattenNode (Vector.fromList v)
      val base = preserve []

                          
      (* Level 1: (int * int) array *)
      val t1 = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val e1 = flatten [base, base]

      (* Level 2: ((int * int) array * int) array *)
      val t2 = Type.array (Type.tuple (Vector.fromList [t1, intTy]))
      val e2 = flatten [e1, base]

      (* Level 3: (((int * int) array * int) array * int) array *)
      val t3 = Type.array (Type.tuple (Vector.fromList [t2, intTy]))
      val e3 = flatten [e2, base]

      (* Width test: (int * int * int) array with MaxWidth 2 *)
      val t_w3 = Type.array (Type.tuple (Vector.fromList [intTy, intTy, intTy]))
      val e_w3 = preserve [preserve [base, base, base]]

      (* Flattened inside non-flattened: ((int * int) array * int) *)
      val t_inf = Type.tuple (Vector.fromList [t1, intTy])
      val e_inf = preserve [e1, base]

      (* Non-flattened inside flattened: ((int * int * int) array * int) array *)
      val t_nif = Type.array (Type.tuple (Vector.fromList [t_w3, intTy]))
      val e_nif = flatten [e_w3, base]

      (* Vector cases *)
      (* Level 1: (int * int) vector *)
      val tv1 = Type.vector (Type.tuple (Vector.fromList [intTy, intTy]))
      val ev1 = flatten [base, base]

      (* Level 2: ((int * int) vector * int) vector *)
      val tv2 = Type.vector (Type.tuple (Vector.fromList [tv1, intTy]))
      val ev2 = flatten [ev1, base]

   in
      print ("t_inf: " ^ (Layout.toString (Type.layout t_inf)) ^ "\n");
      print ("t_inf conDecision: " ^ conDecisionToString e_inf ^ "\n");
      print ("t_nif: " ^ (Layout.toString (Type.layout t_nif)) ^ "\n");
      print ("t_nif conDecision: " ^ conDecisionToString e_nif ^ "\n");
      check (policy2, t1, e1, "Level 1");
      check (policy2, t2, e2, "Level 2");
      check (policy2, t3, e3, "Level 3");
      check (policy2, t_w3, e_w3, "Width > MaxWidth");
      check (policy2, t_inf, e_inf, "Flattened inside non-flattened");
      check (policy2, t_nif, e_nif, "Non-flattened inside flattened");
      check (policy2, tv1, ev1, "Vector Level 1");
      check (policy2, tv2, ev2, "Vector Level 2")
   end)(* Test 47: applyConDecision *)
   val _ = runTest ("Test 47: applyConDecision", fn () => let
      fun conDecisionToString cd =
         case cd of
            ShallowFlatten.PreserveNode v =>
               "PreserveNode[" ^ String.concatWith (Vector.toListMap (v, conDecisionToString), ", ") ^ "]"
          | ShallowFlatten.FlattenNode v =>
               "FlattenNode[" ^ String.concatWith (Vector.toListMap (v, conDecisionToString), ", ") ^ "]"

      fun check (cd, ty, expected, msg) =
         let
            val _ = print ("\n--- Test 47 Subcase: " ^ msg ^ " ---\n")
            val res = ShallowFlatten.applyConDecision (cd, ty)
            fun typeLayout t = Layout.toString (Type.layout t)
            val _ = print (concat ["Compare: ", msg,
                                   "\ntype       = ", typeLayout ty,
                                   "\nconDecision= ", conDecisionToString cd,
                                   "\nexpected   = ", typeLayout expected,
                                   "\nactual     = ", typeLayout res,
                                   "\n"])
         in
            if Type.equals (res, expected) then ()
            else assert (false, msg ^ ": type mismatch")
         end

      val intTy = Type.intInf
      fun preserve v = ShallowFlatten.PreserveNode (Vector.fromList v)
      fun flatten v = ShallowFlatten.FlattenNode (Vector.fromList v)
      val base = preserve []

      (* Level 1 *)
      val t1 = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val cd1 = flatten [base, base]
      val e1 = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])

      (* Level 2 *)
      val t2 = Type.array (Type.tuple (Vector.fromList [t1, intTy]))
      val cd2 = flatten [cd1, base]
      val e2 = Type.tuple (Vector.fromList [Type.array e1, Type.array intTy])

      (* Vector Level 1 *)
      val tv1 = Type.vector (Type.tuple (Vector.fromList [intTy, intTy]))
      val cdv1 = flatten [base, base]
      val ev1 = Type.tuple (Vector.fromList [Type.vector intTy, Type.vector intTy])

      (* Vector Level 2 *)
      val tv2 = Type.vector (Type.tuple (Vector.fromList [tv1, intTy]))
      val cdv2 = flatten [cdv1, base]
      val ev2 = Type.tuple (Vector.fromList [Type.vector ev1, Type.vector intTy])

      val cd_err = flatten [base]

   in
      check (cd1, t1, e1, "Level 1");
      check (cd2, t2, e2, "Level 2");
      check (cdv1, tv1, ev1, "Vector Level 1");
      check (cdv2, tv2, ev2, "Vector Level 2");

      (* Error case: FlattenNode on non-flattenable type *)
      print ("\n--- Test 47 Subcase: Error case ---\n");
      print ("type       = " ^ Layout.toString (Type.layout intTy) ^ "\n");
      print ("conDecision= " ^ conDecisionToString cd_err ^ "\n");
      (ShallowFlatten.applyConDecision (cd_err, intTy);
       assert (false, "Should have raised InvalidConFlattening"))
      handle ShallowFlatten.InvalidConFlattening => ()
           | e => assert (false, "Raised wrong exception: " ^ exnMessage e)
   end)(* Test 48: round-trip getConDecisionForPolicy -> applyConDecision *)
   val _ = runTest ("Test 48: round-trip getConDecisionForPolicy -> applyConDecision", fn () => let
      fun check (policy, ty, expected, msg) =
         let
            val cd = ShallowFlatten.getConDecisionForPolicy policy ty
            val res = ShallowFlatten.applyConDecision (cd, ty)
         in
            if Type.equals (res, expected) then ()
            else assert (false, msg ^ ": type mismatch.\nGot:      " ^ (Layout.toString (Type.layout res)) ^ 
                              "\nExpected: " ^ (Layout.toString (Type.layout expected)))
         end

      val intTy = Type.intInf
      val policy2 = ShallowFlatten.MaxWidth 2
      
      (* 1. Simple 2-tuple array *)
      val t1 = Type.array (Type.tuple (Vector.fromList [intTy, intTy]))
      val e1 = Type.tuple (Vector.fromList [Type.array intTy, Type.array intTy])
      
      (* 2. Width > MaxWidth (Preserved) *)
      val t2 = Type.array (Type.tuple (Vector.fromList [intTy, intTy, intTy]))
      (* Expected to be the same because it's preserved *)
      val e2 = t2

      (* 3. Nested flattening: ((int * int) array * int) array *)
      (* Inner array is flattened, outer array is flattened *)
      val t3_inner = t1 (* (int*int) array *)
      val t3 = Type.array (Type.tuple (Vector.fromList [t3_inner, intTy]))
      
      val e3_inner = e1 (* (int array * int array) *)
      val e3 = Type.tuple (Vector.fromList [Type.array e3_inner, Type.array intTy])

      (* 4. Mixed: ((int * int * int) array * int) array *)
      (* Inner array is preserved (width 3), outer array is flattened *)
      val t4_inner = t2 (* (int*int*int) array *)
      val t4 = Type.array (Type.tuple (Vector.fromList [t4_inner, intTy]))
      
      val e4 = Type.tuple (Vector.fromList [Type.array t4_inner, Type.array intTy])
   in
      check (policy2, t1, e1, "Level 1 flattening");
      check (policy2, t2, e2, "Width > MaxWidth preservation");
      check (policy2, t3, e3, "Level 2 nested flattening");
      check (policy2, t4, e4, "Mixed flattening/preservation")
   end)

      
      val _ = summarize ()
end
