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

   val _ = Control.libTargetDir := "../../build/lib/mlton/targets/self"

   val seqIndexSize = WordSize.seqIndex ()
   val seqIndexTy = Type.word seqIndexSize
   val intTy = Type.intInf
   val word32Ty = Type.word WordSize.word32

   (* Helper to build a PrimApp expression *)
   fun primApp (p, args, targs) = 
      Exp.PrimApp {args = Vector.fromList args,
                   prim = p,
                   targs = Vector.fromList targs}
in

   (* Test 1: maybeFlattenStatementAoS on non-container statements *)
   val _ = runTest ("Test 1: maybeFlattenStatementAoS - non-container statements", fn () => let
      val v1 = Var.fromString "v1"
      val s = Statement.T {
         exp = Exp.Const (Const.IntInf 123),
         ty = Type.intInf,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "non-container statement should return SOME (Vector.new1 s)"
      val _ = assert (Vector.length stmts = 1, "should be 1 statement")
      val _ = assert (statementEquals (Vector.sub (stmts, 0), s), "should equal original statement")
   in () end)

   (* Test 2: maybeFlattenStatementAoS - Array_alloc on non-tuple type *)
   val _ = runTest ("Test 2: maybeFlattenStatementAoS - Array_alloc on non-tuple type", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = primApp (allocPrim, [n], [intTy]),
         ty = Type.array intTy,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_alloc on non-tuple should return NONE")
   in () end)

   (* Test 3: maybeFlattenStatementAoS - Array_alloc on tuple type with mixed element types *)
   val _ = runTest ("Test 3: maybeFlattenStatementAoS - Array_alloc on mixed tuple type", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = primApp (allocPrim, [n], [mixedTupleTy]),
         ty = Type.array mixedTupleTy,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_alloc on mixed tuple should return NONE")
   in () end)

   (* Test 4: maybeFlattenStatementAoS - Array_alloc on tuple of identical types (width 2) *)
   val _ = runTest ("Test 4: maybeFlattenStatementAoS - Array_alloc on identical tuple (width 2)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val allocPrim = Prim.Array_alloc {raw = false}
      val s = Statement.T {
         exp = primApp (allocPrim, [n], [tuple2Ty]),
         ty = Type.array tuple2Ty,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_alloc on identical tuple should return SOME"
      val _ = assert (Vector.length stmts = 3, "should return 3 statements")

      (* First statement: const size = 2 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 2, "s0 constant should be 2")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* Second statement: flatSize = n * 2 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val flatSizeVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs should be empty");
                    assert (Vector.length args = 2, "s1 args length should be 2");
                    assert (Var.equals (Vector.sub (args, 0), n), "s1 first arg should be n");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* Third statement: x: 'a array = Array_alloc['a](flatSize) *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, Type.array intTy, "s2 type should be array of element type")
      val Statement.T {exp = e2, var = v2', ...} = s2
      val _ = assert (Option.isSome v2' andalso Var.equals (Option.valOf v2', v1),
                      "s2 should bind original variable")
      val _ = case e2 of
                 Exp.PrimApp {prim = Prim.Array_alloc {raw = false}, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), flatSizeVar),
                            "s2 arg should be flatSizeVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy),
                            "s2 targ should be element type")
                 )
               | _ => raise TestFail "s2 should be Array_alloc PrimApp"
   in () end)

   (* Test 5: maybeFlattenStatementAoS - Array_alloc on tuple of identical types (width 3) *)
   val _ = runTest ("Test 5: maybeFlattenStatementAoS - Array_alloc on identical tuple (width 3)", fn () => let
      val v1 = Var.fromString "v1"
      val n = Var.fromString "n"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val allocPrim = Prim.Array_alloc {raw = true}
      val s = Statement.T {
         exp = primApp (allocPrim, [n], [tuple3Ty]),
         ty = Type.array tuple3Ty,
         var = SOME v1
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_alloc on identical tuple (width 3) should return SOME"
      val _ = assert (Vector.length stmts = 3, "should return 3 statements")

      (* First statement: const size = 3 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 3, "s0 constant should be 3")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* Second statement: flatSize = n * 3 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val flatSizeVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs should be empty");
                    assert (Vector.length args = 2, "s1 args length should be 2");
                    assert (Var.equals (Vector.sub (args, 0), n), "s1 first arg should be n");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* Third statement: x: 'a array = Array_alloc['a](flatSize) *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, Type.array word32Ty, "s2 type should be array of element type")
      val Statement.T {exp = e2, var = v2', ...} = s2
      val _ = assert (Option.isSome v2' andalso Var.equals (Option.valOf v2', v1),
                      "s2 should bind original variable")
      val _ = case e2 of
                 Exp.PrimApp {prim = Prim.Array_alloc {raw = true}, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), flatSizeVar),
                            "s2 arg should be flatSizeVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty),
                            "s2 targ should be element type")
                 )
               | _ => raise TestFail "s2 should be Array_alloc PrimApp"
   in () end)

   (* Test 6: maybeFlattenStatementAoS - Array_length on identical tuple (width 2) *)
   val _ = runTest ("Test 6: maybeFlattenStatementAoS - Array_length on identical tuple (width 2)", fn () => let
      val lenVar = Var.fromString "len"
      val arrVar = Var.fromString "arr"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val s = Statement.T {
         exp = primApp (Prim.Array_length, [arrVar], [tuple2Ty]),
         ty = seqIndexTy,
         var = SOME lenVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_length on identical tuple should return SOME"
      val _ = assert (Vector.length stmts = 3, "should return 3 statements")

      (* First statement: physLen: seqIndexTy = Array_length[intTy](arr) *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val physLenVar = Option.valOf v0
      val _ = case e0 of
                 Exp.PrimApp {prim = Prim.Array_length, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), arrVar), "s0 arg should be arrVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s0 targ should be intTy")
                 )
               | _ => raise TestFail "s0 should be Array_length PrimApp"

      (* Second statement: const size = 2 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1, ...} = s1
      val _ = assert (Option.isSome v1, "s1 should bind a variable")
      val flatSizeConstVar = Option.valOf v1
      val _ = case e1 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s1 Const size");
                    assert (WordX.toIntInf wx = 2, "s1 constant should be 2")
                 )
               | _ => raise TestFail "s1 should be Word Const"

      (* Third statement: len = physLen div constSize *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2 andalso Var.equals (Option.valOf v2, lenVar), "s2 should bind lenVar")
      val _ = case e2 of
                 Exp.PrimApp {prim = Prim.Word_quot (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s2 Word_quot size");
                    assert (Vector.length targs = 0, "s2 targs should be empty");
                    assert (Vector.length args = 2, "s2 args length should be 2");
                    assert (Var.equals (Vector.sub (args, 0), physLenVar), "s2 first arg should be physLenVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s2 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s2 should be Word_quot PrimApp"
   in () end)

   (* Test 7: maybeFlattenStatementAoS - Vector_length on identical tuple (width 3) *)
   val _ = runTest ("Test 7: maybeFlattenStatementAoS - Vector_length on identical tuple (width 3)", fn () => let
      val lenVar = Var.fromString "len"
      val vecVar = Var.fromString "vec"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Vector_length, [vecVar], [tuple3Ty]),
         ty = seqIndexTy,
         var = SOME lenVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Vector_length on identical tuple should return SOME"
      val _ = assert (Vector.length stmts = 3, "should return 3 statements")

      (* First statement: physLen: seqIndexTy = Vector_length[word32Ty](vec) *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val physLenVar = Option.valOf v0
      val _ = case e0 of
                 Exp.PrimApp {prim = Prim.Vector_length, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), vecVar), "s0 arg should be vecVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s0 targ should be word32Ty")
                 )
               | _ => raise TestFail "s0 should be Vector_length PrimApp"

      (* Second statement: const size = 3 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1, ...} = s1
      val _ = assert (Option.isSome v1, "s1 should bind a variable")
      val flatSizeConstVar = Option.valOf v1
      val _ = case e1 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s1 Const size");
                    assert (WordX.toIntInf wx = 3, "s1 constant should be 3")
                 )
               | _ => raise TestFail "s1 should be Word Const"

      (* Third statement: len = physLen div constSize *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2 andalso Var.equals (Option.valOf v2, lenVar), "s2 should bind lenVar")
      val _ = case e2 of
                 Exp.PrimApp {prim = Prim.Word_quot (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s2 Word_quot size");
                    assert (Vector.length targs = 0, "s2 targs should be empty");
                    assert (Vector.length args = 2, "s2 args length should be 2");
                    assert (Var.equals (Vector.sub (args, 0), physLenVar), "s2 first arg should be physLenVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s2 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s2 should be Word_quot PrimApp"
   in () end)

   (* Test 8: maybeFlattenStatementAoS - Array_length on non-tuple type *)
   val _ = runTest ("Test 8: maybeFlattenStatementAoS - Array_length on non-tuple type", fn () => let
      val lenVar = Var.fromString "len"
      val arrVar = Var.fromString "arr"
      val s = Statement.T {
         exp = primApp (Prim.Array_length, [arrVar], [intTy]),
         ty = seqIndexTy,
         var = SOME lenVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_length on non-tuple should return NONE")
   in () end)

   (* Test 9: maybeFlattenStatementAoS - Array_length on mixed tuple type *)
   val _ = runTest ("Test 9: maybeFlattenStatementAoS - Array_length on mixed tuple type", fn () => let
      val lenVar = Var.fromString "len"
      val arrVar = Var.fromString "arr"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Array_length, [arrVar], [mixedTupleTy]),
         ty = seqIndexTy,
         var = SOME lenVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_length on mixed tuple should return NONE")
   in () end)

   (* Test 10: maybeFlattenStatementAoS - Array_sub on non-tuple type *)
   val _ = runTest ("Test 10: maybeFlattenStatementAoS - Array_sub on non-tuple type", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val subPrim = Prim.Array_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [intTy]),
         ty = intTy,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_sub on non-tuple should return NONE")
   in () end)

   (* Test 11: maybeFlattenStatementAoS - Array_sub on mixed tuple type *)
   val _ = runTest ("Test 11: maybeFlattenStatementAoS - Array_sub on mixed tuple type", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val subPrim = Prim.Array_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [mixedTupleTy]),
         ty = mixedTupleTy,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_sub on mixed tuple should return NONE")
   in () end)

   (* Test 12: maybeFlattenStatementAoS - Array_sub on identical tuple (width 2) *)
   val _ = runTest ("Test 12: maybeFlattenStatementAoS - Array_sub on identical tuple (width 2)", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val subPrim = Prim.Array_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [tuple2Ty]),
         ty = tuple2Ty,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_sub on identical tuple (width 2) should return SOME"
      val _ = assert (Vector.length stmts = 9, "should return 9 statements")

      (* s0: const size = 2 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 2, "s0 constant should be 2")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 2 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: add = offset + 0 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val offsetPlus0Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s4 Word_add size");
                    assert (Vector.length targs = 0, "s4 targs empty");
                    assert (Vector.length args = 2, "s4 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s4 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s4 second arg should be const0Var")
                 )
               | _ => raise TestFail "s4 should be Word_add PrimApp"

      (* s5: add = offset + 1 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus1Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s5 second arg should be const1Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: x_0 = Array_sub[int](arr, offset + 0) *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, intTy, "s6 type should be intTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val x0Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Array_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s6 targ should be intTy");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s6 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s6 second arg should be offsetPlus0Var")
                 )
               | _ => raise TestFail "s6 should be Array_sub PrimApp"

      (* s7: x_1 = Array_sub[int](arr, offset + 1) *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, intTy, "s7 type should be intTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val x1Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Array_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s7 targ should be intTy");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s7 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s7 second arg should be offsetPlus1Var")
                 )
               | _ => raise TestFail "s7 should be Array_sub PrimApp"

      (* s8: x = tuple(x0, x1) *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, tuple2Ty, "s8 type should be tuple2Ty")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isSome v8' andalso Var.equals (Option.valOf v8', xVar), "s8 should bind xVar")
      val _ = case e8 of
                 Exp.Tuple args => (
                    assert (Vector.length args = 2, "s8 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), x0Var), "s8 first arg should be x0Var");
                    assert (Var.equals (Vector.sub (args, 1), x1Var), "s8 second arg should be x1Var")
                 )
               | _ => raise TestFail "s8 should be Tuple"
   in () end)

   (* Test 13: maybeFlattenStatementAoS - Array_sub on identical tuple (width 3) *)
   val _ = runTest ("Test 13: maybeFlattenStatementAoS - Array_sub on identical tuple (width 3)", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val subPrim = Prim.Array_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [tuple3Ty]),
         ty = tuple3Ty,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_sub on identical tuple (width 3) should return SOME"
      val _ = assert (Vector.length stmts = 12, "should return 12 statements")

      (* s0: const size = 3 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 3, "s0 constant should be 3")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 3 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: const 2 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val const2Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s4 Const size");
                    assert (WordX.toIntInf wx = 2, "s4 constant should be 2")
                 )
               | _ => raise TestFail "s4 should be Word Const"

      (* s5: add = offset + 0 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus0Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s5 second arg should be const0Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: add = offset + 1 *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, seqIndexTy, "s6 type should be seqIndexTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val offsetPlus1Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s6 Word_add size");
                    assert (Vector.length targs = 0, "s6 targs empty");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s6 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s6 second arg should be const1Var")
                 )
               | _ => raise TestFail "s6 should be Word_add PrimApp"

      (* s7: add = offset + 2 *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, seqIndexTy, "s7 type should be seqIndexTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val offsetPlus2Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s7 Word_add size");
                    assert (Vector.length targs = 0, "s7 targs empty");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s7 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const2Var), "s7 second arg should be const2Var")
                 )
               | _ => raise TestFail "s7 should be Word_add PrimApp"

      (* s8: x_0 = Array_sub[word32](arr, offset + 0) *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, word32Ty, "s8 type should be word32Ty")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isSome v8', "s8 should bind a variable")
      val x0Var = Option.valOf v8'
      val _ = case e8 of
                 Exp.PrimApp {prim = Prim.Array_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s8 targ should be word32Ty");
                    assert (Vector.length args = 2, "s8 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s8 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s8 second arg should be offsetPlus0Var")
                 )
               | _ => raise TestFail "s8 should be Array_sub PrimApp"

      (* s9: x_1 = Array_sub[word32](arr, offset + 1) *)
      val s9 = Vector.sub (stmts, 9)
      val _ = assertType (s9, word32Ty, "s9 type should be word32Ty")
      val Statement.T {exp = e9, var = v9', ...} = s9
      val _ = assert (Option.isSome v9', "s9 should bind a variable")
      val x1Var = Option.valOf v9'
      val _ = case e9 of
                 Exp.PrimApp {prim = Prim.Array_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s9 targ should be word32Ty");
                    assert (Vector.length args = 2, "s9 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s9 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s9 second arg should be offsetPlus1Var")
                 )
               | _ => raise TestFail "s9 should be Array_sub PrimApp"

      (* s10: x_2 = Array_sub[word32](arr, offset + 2) *)
      val s10 = Vector.sub (stmts, 10)
      val _ = assertType (s10, word32Ty, "s10 type should be word32Ty")
      val Statement.T {exp = e10, var = v10', ...} = s10
      val _ = assert (Option.isSome v10', "s10 should bind a variable")
      val x2Var = Option.valOf v10'
      val _ = case e10 of
                 Exp.PrimApp {prim = Prim.Array_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s10 targ should be word32Ty");
                    assert (Vector.length args = 2, "s10 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s10 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus2Var), "s10 second arg should be offsetPlus2Var")
                 )
               | _ => raise TestFail "s10 should be Array_sub PrimApp"

      (* s11: x = tuple(x0, x1, x2) *)
      val s11 = Vector.sub (stmts, 11)
      val _ = assertType (s11, tuple3Ty, "s11 type should be tuple3Ty")
      val Statement.T {exp = e11, var = v11', ...} = s11
      val _ = assert (Option.isSome v11' andalso Var.equals (Option.valOf v11', xVar), "s11 should bind xVar")
      val _ = case e11 of
                 Exp.Tuple args => (
                    assert (Vector.length args = 3, "s11 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), x0Var), "s11 first arg should be x0Var");
                    assert (Var.equals (Vector.sub (args, 1), x1Var), "s11 second arg should be x1Var");
                    assert (Var.equals (Vector.sub (args, 2), x2Var), "s11 third arg should be x2Var")
                 )
               | _ => raise TestFail "s11 should be Tuple"
   in () end)

   (* Test 14: maybeFlattenStatementAoS - Vector_sub on non-tuple type *)
   val _ = runTest ("Test 14: maybeFlattenStatementAoS - Vector_sub on non-tuple type", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val subPrim = Prim.Vector_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [intTy]),
         ty = intTy,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Vector_sub on non-tuple should return NONE")
   in () end)

   (* Test 15: maybeFlattenStatementAoS - Vector_sub on mixed tuple type *)
   val _ = runTest ("Test 15: maybeFlattenStatementAoS - Vector_sub on mixed tuple type", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val subPrim = Prim.Vector_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [mixedTupleTy]),
         ty = mixedTupleTy,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Vector_sub on mixed tuple should return NONE")
   in () end)

   (* Test 16: maybeFlattenStatementAoS - Vector_sub on identical tuple (width 2) *)
   val _ = runTest ("Test 16: maybeFlattenStatementAoS - Vector_sub on identical tuple (width 2)", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val subPrim = Prim.Vector_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [tuple2Ty]),
         ty = tuple2Ty,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Vector_sub on identical tuple (width 2) should return SOME"
      val _ = assert (Vector.length stmts = 9, "should return 9 statements")

      (* s0: const size = 2 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 2, "s0 constant should be 2")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 2 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: add = offset + 0 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val offsetPlus0Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s4 Word_add size");
                    assert (Vector.length targs = 0, "s4 targs empty");
                    assert (Vector.length args = 2, "s4 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s4 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s4 second arg should be const0Var")
                 )
               | _ => raise TestFail "s4 should be Word_add PrimApp"

      (* s5: add = offset + 1 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus1Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s5 second arg should be const1Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: x_0 = Vector_sub[int](arr, offset + 0) *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, intTy, "s6 type should be intTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val x0Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Vector_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s6 targ should be intTy");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s6 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s6 second arg should be offsetPlus0Var")
                 )
               | _ => raise TestFail "s6 should be Vector_sub PrimApp"

      (* s7: x_1 = Vector_sub[int](arr, offset + 1) *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, intTy, "s7 type should be intTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val x1Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Vector_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s7 targ should be intTy");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s7 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s7 second arg should be offsetPlus1Var")
                 )
               | _ => raise TestFail "s7 should be Vector_sub PrimApp"

      (* s8: x = tuple(x0, x1) *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, tuple2Ty, "s8 type should be tuple2Ty")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isSome v8' andalso Var.equals (Option.valOf v8', xVar), "s8 should bind xVar")
      val _ = case e8 of
                 Exp.Tuple args => (
                    assert (Vector.length args = 2, "s8 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), x0Var), "s8 first arg should be x0Var");
                    assert (Var.equals (Vector.sub (args, 1), x1Var), "s8 second arg should be x1Var")
                 )
               | _ => raise TestFail "s8 should be Tuple"
   in () end)

   (* Test 17: maybeFlattenStatementAoS - Vector_sub on identical tuple (width 3) *)
   val _ = runTest ("Test 17: maybeFlattenStatementAoS - Vector_sub on identical tuple (width 3)", fn () => let
      val xVar = Var.fromString "x"
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val subPrim = Prim.Vector_sub
      val s = Statement.T {
         exp = primApp (subPrim, [arrVar, idxVar], [tuple3Ty]),
         ty = tuple3Ty,
         var = SOME xVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Vector_sub on identical tuple (width 3) should return SOME"
      val _ = assert (Vector.length stmts = 12, "should return 12 statements")

      (* s0: const size = 3 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 3, "s0 constant should be 3")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 3 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: const 2 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val const2Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s4 Const size");
                    assert (WordX.toIntInf wx = 2, "s4 constant should be 2")
                 )
               | _ => raise TestFail "s4 should be Word Const"

      (* s5: add = offset + 0 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus0Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s5 second arg should be const0Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: add = offset + 1 *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, seqIndexTy, "s6 type should be seqIndexTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val offsetPlus1Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s6 Word_add size");
                    assert (Vector.length targs = 0, "s6 targs empty");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s6 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s6 second arg should be const1Var")
                 )
               | _ => raise TestFail "s6 should be Word_add PrimApp"

      (* s7: add = offset + 2 *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, seqIndexTy, "s7 type should be seqIndexTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val offsetPlus2Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s7 Word_add size");
                    assert (Vector.length targs = 0, "s7 targs empty");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s7 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const2Var), "s7 second arg should be const2Var")
                 )
               | _ => raise TestFail "s7 should be Word_add PrimApp"

      (* s8: x_0 = Vector_sub[word32](arr, offset + 0) *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, word32Ty, "s8 type should be word32Ty")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isSome v8', "s8 should bind a variable")
      val x0Var = Option.valOf v8'
      val _ = case e8 of
                 Exp.PrimApp {prim = Prim.Vector_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s8 targ should be word32Ty");
                    assert (Vector.length args = 2, "s8 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s8 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s8 second arg should be offsetPlus0Var")
                 )
               | _ => raise TestFail "s8 should be Vector_sub PrimApp"

      (* s9: x_1 = Vector_sub[word32](arr, offset + 1) *)
      val s9 = Vector.sub (stmts, 9)
      val _ = assertType (s9, word32Ty, "s9 type should be word32Ty")
      val Statement.T {exp = e9, var = v9', ...} = s9
      val _ = assert (Option.isSome v9', "s9 should bind a variable")
      val x1Var = Option.valOf v9'
      val _ = case e9 of
                 Exp.PrimApp {prim = Prim.Vector_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s9 targ should be word32Ty");
                    assert (Vector.length args = 2, "s9 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s9 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s9 second arg should be offsetPlus1Var")
                 )
               | _ => raise TestFail "s9 should be Vector_sub PrimApp"

      (* s10: x_2 = Vector_sub[word32](arr, offset + 2) *)
      val s10 = Vector.sub (stmts, 10)
      val _ = assertType (s10, word32Ty, "s10 type should be word32Ty")
      val Statement.T {exp = e10, var = v10', ...} = s10
      val _ = assert (Option.isSome v10', "s10 should bind a variable")
      val x2Var = Option.valOf v10'
      val _ = case e10 of
                 Exp.PrimApp {prim = Prim.Vector_sub, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s10 targ should be word32Ty");
                    assert (Vector.length args = 2, "s10 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s10 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus2Var), "s10 second arg should be offsetPlus2Var")
                 )
               | _ => raise TestFail "s10 should be Vector_sub PrimApp"

      (* s11: x = tuple(x0, x1, x2) *)
      val s11 = Vector.sub (stmts, 11)
      val _ = assertType (s11, tuple3Ty, "s11 type should be tuple3Ty")
      val Statement.T {exp = e11, var = v11', ...} = s11
      val _ = assert (Option.isSome v11' andalso Var.equals (Option.valOf v11', xVar), "s11 should bind xVar")
      val _ = case e11 of
                 Exp.Tuple args => (
                    assert (Vector.length args = 3, "s11 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), x0Var), "s11 first arg should be x0Var");
                    assert (Var.equals (Vector.sub (args, 1), x1Var), "s11 second arg should be x1Var");
                    assert (Var.equals (Vector.sub (args, 2), x2Var), "s11 third arg should be x2Var")
                 )
               | _ => raise TestFail "s11 should be Tuple"
   in () end)

   (* Test 18: maybeFlattenStatementAoS - Array_update on non-tuple type *)
   val _ = runTest ("Test 18: maybeFlattenStatementAoS - Array_update on non-tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val xVar = Var.fromString "x"
      val updatePrim = Prim.Array_update
      val s = Statement.T {
         exp = primApp (updatePrim, [arrVar, idxVar, xVar], [intTy]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_update on non-tuple should return NONE")
   in () end)

   (* Test 19: maybeFlattenStatementAoS - Array_update on mixed tuple type *)
   val _ = runTest ("Test 19: maybeFlattenStatementAoS - Array_update on mixed tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val xVar = Var.fromString "x"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val updatePrim = Prim.Array_update
      val s = Statement.T {
         exp = primApp (updatePrim, [arrVar, idxVar, xVar], [mixedTupleTy]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_update on mixed tuple should return NONE")
   in () end)

   (* Test 20: maybeFlattenStatementAoS - Array_update on identical tuple (width 2) - writeBarrier = false *)
   val _ = runTest ("Test 20: maybeFlattenStatementAoS - Array_update on identical tuple (width 2) - writeBarrier = false", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val xVar = Var.fromString "x"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val updatePrim = Prim.Array_update
      val s = Statement.T {
         exp = primApp (updatePrim, [arrVar, idxVar, xVar], [tuple2Ty]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_update on identical tuple (width 2) should return SOME"
      val _ = assert (Vector.length stmts = 10, "should return 10 statements")

      (* s0: const size = 2 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 2, "s0 constant should be 2")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 2 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: add = offset + 0 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val offsetPlus0Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s4 Word_add size");
                    assert (Vector.length targs = 0, "s4 targs empty");
                    assert (Vector.length args = 2, "s4 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s4 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s4 second arg should be const0Var")
                 )
               | _ => raise TestFail "s4 should be Word_add PrimApp"

      (* s5: add = offset + 1 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus1Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s5 second arg should be const1Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: selectVal_0 = select(x, 0) *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, intTy, "s6 type should be intTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val selectVal0Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.Select {offset, tuple} => (
                    assert (offset = 0, "s6 select offset should be 0");
                    assert (Var.equals (tuple, xVar), "s6 tuple should be xVar")
                 )
               | _ => raise TestFail "s6 should be Select"

      (* s7: selectVal_1 = select(x, 1) *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, intTy, "s7 type should be intTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val selectVal1Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.Select {offset, tuple} => (
                    assert (offset = 1, "s7 select offset should be 1");
                    assert (Var.equals (tuple, xVar), "s7 tuple should be xVar")
                 )
               | _ => raise TestFail "s7 should be Select"

      (* s8: Array_update {writeBarrier = false} *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, Type.unit, "s8 type should be Type.unit")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isNone v8', "s8 should not bind a variable")
      val _ = case e8 of
                 Exp.PrimApp {prim = Prim.Array_update, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s8 targ should be intTy");
                    assert (Vector.length args = 3, "s8 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s8 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s8 second arg should be offsetPlus0Var");
                    assert (Var.equals (Vector.sub (args, 2), selectVal0Var), "s8 third arg should be selectVal0Var")
                 )
               | _ => raise TestFail "s8 should be Array_update PrimApp"

      (* s9: Array_update {writeBarrier = false} *)
      val s9 = Vector.sub (stmts, 9)
      val _ = assertType (s9, Type.unit, "s9 type should be Type.unit")
      val Statement.T {exp = e9, var = v9', ...} = s9
      val _ = assert (Option.isNone v9', "s9 should not bind a variable")
      val _ = case e9 of
                 Exp.PrimApp {prim = Prim.Array_update, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s9 targ should be intTy");
                    assert (Vector.length args = 3, "s9 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s9 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s9 second arg should be offsetPlus1Var");
                    assert (Var.equals (Vector.sub (args, 2), selectVal1Var), "s9 third arg should be selectVal1Var")
                 )
               | _ => raise TestFail "s9 should be Array_update PrimApp"
   in () end)

   (* Test 21: maybeFlattenStatementAoS - Array_update on identical tuple (width 3) - writeBarrier = true *)
   val _ = runTest ("Test 21: maybeFlattenStatementAoS - Array_update on identical tuple (width 3) - writeBarrier = true", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val xVar = Var.fromString "x"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val updatePrim = Prim.Array_update
      val s = Statement.T {
         exp = primApp (updatePrim, [arrVar, idxVar, xVar], [tuple3Ty]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_update on identical tuple (width 3) should return SOME"
      val _ = assert (Vector.length stmts = 14, "should return 14 statements")

      (* s0: const size = 3 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 3, "s0 constant should be 3")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 3 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: const 2 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val const2Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s4 Const size");
                    assert (WordX.toIntInf wx = 2, "s4 constant should be 2")
                 )
               | _ => raise TestFail "s4 should be Word Const"

      (* s5: add = offset + 0 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus0Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s5 second arg should be const0Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: add = offset + 1 *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, seqIndexTy, "s6 type should be seqIndexTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val offsetPlus1Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s6 Word_add size");
                    assert (Vector.length targs = 0, "s6 targs empty");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s6 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s6 second arg should be const1Var")
                 )
               | _ => raise TestFail "s6 should be Word_add PrimApp"

      (* s7: add = offset + 2 *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, seqIndexTy, "s7 type should be seqIndexTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val offsetPlus2Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s7 Word_add size");
                    assert (Vector.length targs = 0, "s7 targs empty");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s7 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const2Var), "s7 second arg should be const2Var")
                 )
               | _ => raise TestFail "s7 should be Word_add PrimApp"

      (* s8: selectVal_0 = select(x, 0) *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, word32Ty, "s8 type should be word32Ty")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isSome v8', "s8 should bind a variable")
      val selectVal0Var = Option.valOf v8'
      val _ = case e8 of
                 Exp.Select {offset, tuple} => (
                    assert (offset = 0, "s8 select offset should be 0");
                    assert (Var.equals (tuple, xVar), "s8 tuple should be xVar")
                 )
               | _ => raise TestFail "s8 should be Select"

      (* s9: selectVal_1 = select(x, 1) *)
      val s9 = Vector.sub (stmts, 9)
      val _ = assertType (s9, word32Ty, "s9 type should be word32Ty")
      val Statement.T {exp = e9, var = v9', ...} = s9
      val _ = assert (Option.isSome v9', "s9 should bind a variable")
      val selectVal1Var = Option.valOf v9'
      val _ = case e9 of
                 Exp.Select {offset, tuple} => (
                    assert (offset = 1, "s9 select offset should be 1");
                    assert (Var.equals (tuple, xVar), "s9 tuple should be xVar")
                 )
               | _ => raise TestFail "s9 should be Select"

      (* s10: selectVal_2 = select(x, 2) *)
      val s10 = Vector.sub (stmts, 10)
      val _ = assertType (s10, word32Ty, "s10 type should be word32Ty")
      val Statement.T {exp = e10, var = v10', ...} = s10
      val _ = assert (Option.isSome v10', "s10 should bind a variable")
      val selectVal2Var = Option.valOf v10'
      val _ = case e10 of
                 Exp.Select {offset, tuple} => (
                    assert (offset = 2, "s10 select offset should be 2");
                    assert (Var.equals (tuple, xVar), "s10 tuple should be xVar")
                 )
               | _ => raise TestFail "s10 should be Select"

      (* s11: Array_update {writeBarrier = true} *)
      val s11 = Vector.sub (stmts, 11)
      val _ = assertType (s11, Type.unit, "s11 type should be Type.unit")
      val Statement.T {exp = e11, var = v11', ...} = s11
      val _ = assert (Option.isNone v11', "s11 should not bind a variable")
      val _ = case e11 of
                 Exp.PrimApp {prim = Prim.Array_update, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s11 targ should be word32Ty");
                    assert (Vector.length args = 3, "s11 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s11 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s11 second arg should be offsetPlus0Var");
                    assert (Var.equals (Vector.sub (args, 2), selectVal0Var), "s11 third arg should be selectVal0Var")
                 )
               | _ => raise TestFail "s11 should be Array_update PrimApp"

      (* s12: Array_update {writeBarrier = true} *)
      val s12 = Vector.sub (stmts, 12)
      val _ = assertType (s12, Type.unit, "s12 type should be Type.unit")
      val Statement.T {exp = e12, var = v12', ...} = s12
      val _ = assert (Option.isNone v12', "s12 should not bind a variable")
      val _ = case e12 of
                 Exp.PrimApp {prim = Prim.Array_update, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s12 targ should be word32Ty");
                    assert (Vector.length args = 3, "s12 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s12 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s12 second arg should be offsetPlus1Var");
                    assert (Var.equals (Vector.sub (args, 2), selectVal1Var), "s12 third arg should be selectVal1Var")
                 )
               | _ => raise TestFail "s12 should be Array_update PrimApp"

      (* s13: Array_update {writeBarrier = true} *)
      val s13 = Vector.sub (stmts, 13)
      val _ = assertType (s13, Type.unit, "s13 type should be Type.unit")
      val Statement.T {exp = e13, var = v13', ...} = s13
      val _ = assert (Option.isNone v13', "s13 should not bind a variable")
      val _ = case e13 of
                 Exp.PrimApp {prim = Prim.Array_update, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s13 targ should be word32Ty");
                    assert (Vector.length args = 3, "s13 args length 3");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s13 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus2Var), "s13 second arg should be offsetPlus2Var");
                    assert (Var.equals (Vector.sub (args, 2), selectVal2Var), "s13 third arg should be selectVal2Var")
                 )
               | _ => raise TestFail "s13 should be Array_update PrimApp"
    in () end)

   (* Test 22: maybeFlattenStatementAoS - Array_toVector on non-tuple type *)
   val _ = runTest ("Test 22: maybeFlattenStatementAoS - Array_toVector on non-tuple type", fn () => let
      val vecVar = Var.fromString "vec"
      val arrVar = Var.fromString "arr"
      val s = Statement.T {
         exp = primApp (Prim.Array_toVector, [arrVar], [intTy]),
         ty = Type.vector intTy,
         var = SOME vecVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_toVector on non-tuple should return NONE")
   in () end)

   (* Test 23: maybeFlattenStatementAoS - Array_toVector on mixed tuple type *)
   val _ = runTest ("Test 23: maybeFlattenStatementAoS - Array_toVector on mixed tuple type", fn () => let
      val vecVar = Var.fromString "vec"
      val arrVar = Var.fromString "arr"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Array_toVector, [arrVar], [mixedTupleTy]),
         ty = Type.vector mixedTupleTy,
         var = SOME vecVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_toVector on mixed tuple should return NONE")
   in () end)

   (* Test 24: maybeFlattenStatementAoS - Array_toVector on identical tuple (width 2) *)
   val _ = runTest ("Test 24: maybeFlattenStatementAoS - Array_toVector on identical tuple (width 2)", fn () => let
      val vecVar = Var.fromString "vec"
      val arrVar = Var.fromString "arr"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val s = Statement.T {
         exp = primApp (Prim.Array_toVector, [arrVar], [tuple2Ty]),
         ty = Type.vector tuple2Ty,
         var = SOME vecVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_toVector on identical tuple (width 2) should return SOME"
      val _ = assert (Vector.length stmts = 1, "should return 1 statement")

      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, Type.vector intTy, "s0 type should be vector of element type")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0 andalso Var.equals (Option.valOf v0, vecVar),
                      "s0 should bind original variable")
      val _ = case e0 of
                 Exp.PrimApp {prim = Prim.Array_toVector, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), arrVar),
                            "s0 arg should be arrVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy),
                            "s0 targ should be element type")
                 )
               | _ => raise TestFail "s0 should be Array_toVector PrimApp"
   in () end)

   (* Test 25: maybeFlattenStatementAoS - Array_toVector on identical tuple (width 3) *)
   val _ = runTest ("Test 25: maybeFlattenStatementAoS - Array_toVector on identical tuple (width 3)", fn () => let
      val vecVar = Var.fromString "vec"
      val arrVar = Var.fromString "arr"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Array_toVector, [arrVar], [tuple3Ty]),
         ty = Type.vector tuple3Ty,
         var = SOME vecVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_toVector on identical tuple (width 3) should return SOME"
      val _ = assert (Vector.length stmts = 1, "should return 1 statement")

      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, Type.vector word32Ty, "s0 type should be vector of element type")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0 andalso Var.equals (Option.valOf v0, vecVar),
                      "s0 should bind original variable")
      val _ = case e0 of
                 Exp.PrimApp {prim = Prim.Array_toVector, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), arrVar),
                            "s0 arg should be arrVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty),
                            "s0 targ should be element type")
                 )
               | _ => raise TestFail "s0 should be Array_toVector PrimApp"
   in () end)

   (* Test 26: maybeFlattenStatementAoS - Array_uninitIsNop on non-tuple type *)
   val _ = runTest ("Test 26: maybeFlattenStatementAoS - Array_uninitIsNop on non-tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val isNopVar = Var.fromString "isNop"
      val boolTy = Type.bool
      val s = Statement.T {
         exp = primApp (Prim.Array_uninitIsNop, [arrVar], [intTy]),
         ty = boolTy,
         var = SOME isNopVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_uninitIsNop on non-tuple should return NONE")
   in () end)

   (* Test 27: maybeFlattenStatementAoS - Array_uninitIsNop on mixed tuple type *)
   val _ = runTest ("Test 27: maybeFlattenStatementAoS - Array_uninitIsNop on mixed tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val isNopVar = Var.fromString "isNop"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val boolTy = Type.bool
      val s = Statement.T {
         exp = primApp (Prim.Array_uninitIsNop, [arrVar], [mixedTupleTy]),
         ty = boolTy,
         var = SOME isNopVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_uninitIsNop on mixed tuple should return NONE")
   in () end)

   (* Test 28: maybeFlattenStatementAoS - Array_uninitIsNop on identical tuple (width 2) *)
   val _ = runTest ("Test 28: maybeFlattenStatementAoS - Array_uninitIsNop on identical tuple (width 2)", fn () => let
      val arrVar = Var.fromString "arr"
      val isNopVar = Var.fromString "isNop"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val boolTy = Type.bool
      val s = Statement.T {
         exp = primApp (Prim.Array_uninitIsNop, [arrVar], [tuple2Ty]),
         ty = boolTy,
         var = SOME isNopVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_uninitIsNop on identical tuple should return SOME"
      val _ = assert (Vector.length stmts = 1, "should return 1 statement")
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, boolTy, "s0 type should be bool")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0 andalso Var.equals (Option.valOf v0, isNopVar),
                      "s0 should bind original variable")
      val _ = case e0 of
                 Exp.ConApp {con, args} =>
                 assert (Con.equals (con, Con.falsee) andalso Vector.length args = 0, "expected Con.falsee")
               | _ => raise TestFail "s0 should be false ConApp"
   in () end)

   (* Test 29: maybeFlattenStatementAoS - Array_toArray on non-tuple type *)
   val _ = runTest ("Test 29: maybeFlattenStatementAoS - Array_toArray on non-tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val resVar = Var.fromString "res"
      val s = Statement.T {
         exp = primApp (Prim.Array_toArray, [arrVar], [intTy]),
         ty = Type.array intTy,
         var = SOME resVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_toArray on non-tuple should return NONE")
   in () end)

   (* Test 30: maybeFlattenStatementAoS - Array_toArray on mixed tuple type *)
   val _ = runTest ("Test 30: maybeFlattenStatementAoS - Array_toArray on mixed tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val resVar = Var.fromString "res"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Array_toArray, [arrVar], [mixedTupleTy]),
         ty = Type.array mixedTupleTy,
         var = SOME resVar
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_toArray on mixed tuple should return NONE")
   in () end)

   (* Test 31: maybeFlattenStatementAoS - Array_toArray on identical tuple (width 2) *)
   val _ = runTest ("Test 31: maybeFlattenStatementAoS - Array_toArray on identical tuple (width 2)", fn () => let
      val arr'Var = Var.fromString "arr'"
      val arrVar = Var.fromString "arr"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val s = Statement.T {
         exp = primApp (Prim.Array_toArray, [arrVar], [tuple2Ty]),
         ty = Type.array tuple2Ty,
         var = SOME arr'Var
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_toArray on identical tuple (width 2) should return SOME"
      val _ = assert (Vector.length stmts = 1, "should return 1 statement")

      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, Type.array intTy, "s0 type should be array of element type")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0 andalso Var.equals (Option.valOf v0, arr'Var),
                      "s0 should bind original variable")
      val _ = case e0 of
                 Exp.PrimApp {prim = Prim.Array_toArray, args, targs} => (
                    assert (Vector.length args = 1 andalso Var.equals (Vector.sub (args, 0), arrVar),
                            "s0 arg should be arrVar");
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy),
                            "s0 targ should be element type")
                 )
               | _ => raise TestFail "s0 should be Array_toArray PrimApp"
   in () end)

   (* Test 32: maybeFlattenStatementAoS - Array_uninit on non-tuple type *)
   val _ = runTest ("Test 32: maybeFlattenStatementAoS - Array_uninit on non-tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val s = Statement.T {
         exp = primApp (Prim.Array_uninit, [arrVar, idxVar], [intTy]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_uninit on non-tuple should return NONE")
   in () end)

   (* Test 33: maybeFlattenStatementAoS - Array_uninit on mixed tuple type *)
   val _ = runTest ("Test 33: maybeFlattenStatementAoS - Array_uninit on mixed tuple type", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val mixedTupleTy = Type.tuple (Vector.fromList [intTy, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Array_uninit, [arrVar, idxVar], [mixedTupleTy]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val _ = assert (Option.isNone res, "Array_uninit on mixed tuple should return NONE")
   in () end)

   (* Test 34: maybeFlattenStatementAoS - Array_uninit on identical tuple (width 2) *)
   val _ = runTest ("Test 34: maybeFlattenStatementAoS - Array_uninit on identical tuple (width 2)", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val tuple2Ty = Type.tuple (Vector.fromList [intTy, intTy])
      val s = Statement.T {
         exp = primApp (Prim.Array_uninit, [arrVar, idxVar], [tuple2Ty]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_uninit on identical tuple (width 2) should return SOME"
      val _ = assert (Vector.length stmts = 8, "should return 8 statements")

      (* s0: const size = 2 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 2, "s0 constant should be 2")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 2 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: add = offset + 0 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val offsetPlus0Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s4 Word_add size");
                    assert (Vector.length targs = 0, "s4 targs empty");
                    assert (Vector.length args = 2, "s4 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s4 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s4 second arg should be const0Var")
                 )
               | _ => raise TestFail "s4 should be Word_add PrimApp"

      (* s5: add = offset + 1 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus1Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s5 second arg should be const1Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: Array_uninit['a](arr, offset + 0) *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, Type.unit, "s6 type should be unit")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isNone v6', "s6 should not bind a variable")
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Array_uninit, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s6 targ should be intTy");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s6 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s6 second arg should be offsetPlus0Var")
                 )
               | _ => raise TestFail "s6 should be Array_uninit PrimApp"

      (* s7: Array_uninit['a](arr, offset + 1) *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, Type.unit, "s7 type should be unit")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isNone v7', "s7 should not bind a variable")
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Array_uninit, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), intTy), "s7 targ should be intTy");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s7 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s7 second arg should be offsetPlus1Var")
                 )
               | _ => raise TestFail "s7 should be Array_uninit PrimApp"
   in () end)

   (* Test 35: maybeFlattenStatementAoS - Array_uninit on identical tuple (width 3) *)
   val _ = runTest ("Test 35: maybeFlattenStatementAoS - Array_uninit on identical tuple (width 3)", fn () => let
      val arrVar = Var.fromString "arr"
      val idxVar = Var.fromString "idx"
      val tuple3Ty = Type.tuple (Vector.fromList [word32Ty, word32Ty, word32Ty])
      val s = Statement.T {
         exp = primApp (Prim.Array_uninit, [arrVar, idxVar], [tuple3Ty]),
         ty = Type.unit,
         var = NONE
      }
      val res = ShallowFlatten.maybeFlattenStatementAoS s
      val stmts = case res of
                     SOME s => s
                   | NONE => raise TestFail "Array_uninit on identical tuple (width 3) should return SOME"
      val _ = assert (Vector.length stmts = 11, "should return 11 statements")

      (* s0: const size = 3 *)
      val s0 = Vector.sub (stmts, 0)
      val _ = assertType (s0, seqIndexTy, "s0 type should be seqIndexTy")
      val Statement.T {exp = e0, var = v0, ...} = s0
      val _ = assert (Option.isSome v0, "s0 should bind a variable")
      val flatSizeConstVar = Option.valOf v0
      val _ = case e0 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s0 Const size");
                    assert (WordX.toIntInf wx = 3, "s0 constant should be 3")
                 )
               | _ => raise TestFail "s0 should be Word Const"

      (* s1: offset = idxVar * 3 *)
      val s1 = Vector.sub (stmts, 1)
      val _ = assertType (s1, seqIndexTy, "s1 type should be seqIndexTy")
      val Statement.T {exp = e1, var = v1', ...} = s1
      val _ = assert (Option.isSome v1', "s1 should bind a variable")
      val offsetVar = Option.valOf v1'
      val _ = case e1 of
                 Exp.PrimApp {prim = Prim.Word_mul (ws, {signed = false}), args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s1 Word_mul size");
                    assert (Vector.length targs = 0, "s1 targs empty");
                    assert (Vector.length args = 2, "s1 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), idxVar), "s1 first arg should be idxVar");
                    assert (Var.equals (Vector.sub (args, 1), flatSizeConstVar), "s1 second arg should be flatSizeConstVar")
                 )
               | _ => raise TestFail "s1 should be Word_mul PrimApp"

      (* s2: const 0 *)
      val s2 = Vector.sub (stmts, 2)
      val _ = assertType (s2, seqIndexTy, "s2 type should be seqIndexTy")
      val Statement.T {exp = e2, var = v2, ...} = s2
      val _ = assert (Option.isSome v2, "s2 should bind a variable")
      val const0Var = Option.valOf v2
      val _ = case e2 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s2 Const size");
                    assert (WordX.toIntInf wx = 0, "s2 constant should be 0")
                 )
               | _ => raise TestFail "s2 should be Word Const"

      (* s3: const 1 *)
      val s3 = Vector.sub (stmts, 3)
      val _ = assertType (s3, seqIndexTy, "s3 type should be seqIndexTy")
      val Statement.T {exp = e3, var = v3', ...} = s3
      val _ = assert (Option.isSome v3', "s3 should bind a variable")
      val const1Var = Option.valOf v3'
      val _ = case e3 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s3 Const size");
                    assert (WordX.toIntInf wx = 1, "s3 constant should be 1")
                 )
               | _ => raise TestFail "s3 should be Word Const"

      (* s4: const 2 *)
      val s4 = Vector.sub (stmts, 4)
      val _ = assertType (s4, seqIndexTy, "s4 type should be seqIndexTy")
      val Statement.T {exp = e4, var = v4', ...} = s4
      val _ = assert (Option.isSome v4', "s4 should bind a variable")
      val const2Var = Option.valOf v4'
      val _ = case e4 of
                 Exp.Const (Const.Word wx) => (
                    assert (WordSize.equals (WordX.size wx, seqIndexSize), "s4 Const size");
                    assert (WordX.toIntInf wx = 2, "s4 constant should be 2")
                 )
               | _ => raise TestFail "s4 should be Word Const"

      (* s5: add = offset + 0 *)
      val s5 = Vector.sub (stmts, 5)
      val _ = assertType (s5, seqIndexTy, "s5 type should be seqIndexTy")
      val Statement.T {exp = e5, var = v5', ...} = s5
      val _ = assert (Option.isSome v5', "s5 should bind a variable")
      val offsetPlus0Var = Option.valOf v5'
      val _ = case e5 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s5 Word_add size");
                    assert (Vector.length targs = 0, "s5 targs empty");
                    assert (Vector.length args = 2, "s5 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s5 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const0Var), "s5 second arg should be const0Var")
                 )
               | _ => raise TestFail "s5 should be Word_add PrimApp"

      (* s6: add = offset + 1 *)
      val s6 = Vector.sub (stmts, 6)
      val _ = assertType (s6, seqIndexTy, "s6 type should be seqIndexTy")
      val Statement.T {exp = e6, var = v6', ...} = s6
      val _ = assert (Option.isSome v6', "s6 should bind a variable")
      val offsetPlus1Var = Option.valOf v6'
      val _ = case e6 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s6 Word_add size");
                    assert (Vector.length targs = 0, "s6 targs empty");
                    assert (Vector.length args = 2, "s6 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s6 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const1Var), "s6 second arg should be const1Var")
                 )
               | _ => raise TestFail "s6 should be Word_add PrimApp"

      (* s7: add = offset + 2 *)
      val s7 = Vector.sub (stmts, 7)
      val _ = assertType (s7, seqIndexTy, "s7 type should be seqIndexTy")
      val Statement.T {exp = e7, var = v7', ...} = s7
      val _ = assert (Option.isSome v7', "s7 should bind a variable")
      val offsetPlus2Var = Option.valOf v7'
      val _ = case e7 of
                 Exp.PrimApp {prim = Prim.Word_add ws, args, targs} => (
                    assert (WordSize.equals (ws, seqIndexSize), "s7 Word_add size");
                    assert (Vector.length targs = 0, "s7 targs empty");
                    assert (Vector.length args = 2, "s7 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), offsetVar), "s7 first arg should be offsetVar");
                    assert (Var.equals (Vector.sub (args, 1), const2Var), "s7 second arg should be const2Var")
                 )
               | _ => raise TestFail "s7 should be Word_add PrimApp"

      (* s8: Array_uninit[word32](arr, offset + 0) *)
      val s8 = Vector.sub (stmts, 8)
      val _ = assertType (s8, Type.unit, "s8 type should be unit")
      val Statement.T {exp = e8, var = v8', ...} = s8
      val _ = assert (Option.isNone v8', "s8 should not bind a variable")
      val _ = case e8 of
                 Exp.PrimApp {prim = Prim.Array_uninit, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s8 targ should be word32Ty");
                    assert (Vector.length args = 2, "s8 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s8 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus0Var), "s8 second arg should be offsetPlus0Var")
                 )
               | _ => raise TestFail "s8 should be Array_uninit PrimApp"

      (* s9: Array_uninit[word32](arr, offset + 1) *)
      val s9 = Vector.sub (stmts, 9)
      val _ = assertType (s9, Type.unit, "s9 type should be unit")
      val Statement.T {exp = e9, var = v9', ...} = s9
      val _ = assert (Option.isNone v9', "s9 should not bind a variable")
      val _ = case e9 of
                 Exp.PrimApp {prim = Prim.Array_uninit, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s9 targ should be word32Ty");
                    assert (Vector.length args = 2, "s9 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s9 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus1Var), "s9 second arg should be offsetPlus1Var")
                 )
               | _ => raise TestFail "s9 should be Array_uninit PrimApp"

      (* s10: Array_uninit[word32](arr, offset + 2) *)
      val s10 = Vector.sub (stmts, 10)
      val _ = assertType (s10, Type.unit, "s10 type should be unit")
      val Statement.T {exp = e10, var = v10', ...} = s10
      val _ = assert (Option.isNone v10', "s10 should not bind a variable")
      val _ = case e10 of
                 Exp.PrimApp {prim = Prim.Array_uninit, args, targs} => (
                    assert (Vector.length targs = 1 andalso Type.equals (Vector.sub (targs, 0), word32Ty), "s10 targ should be word32Ty");
                    assert (Vector.length args = 2, "s10 args length 2");
                    assert (Var.equals (Vector.sub (args, 0), arrVar), "s10 first arg should be arrVar");
                    assert (Var.equals (Vector.sub (args, 1), offsetPlus2Var), "s10 second arg should be offsetPlus2Var")
                 )
               | _ => raise TestFail "s10 should be Array_uninit PrimApp"
   in () end)

   val _ = summarize ()
end
