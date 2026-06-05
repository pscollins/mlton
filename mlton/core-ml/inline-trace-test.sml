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

val _ = print "Instantiated InlineTrace.\n"

val emptyDecsListVector = Vector.fromList []
val _ = InlineTrace.inlineTrace {prog = emptyDecsListVector}

val _ = print "Test Passed\n"