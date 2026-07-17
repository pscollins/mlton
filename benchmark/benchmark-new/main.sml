(* Copyright (C) 2013,2014,2019,2022 Matthew Fluet.
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main =
struct

fun usage msg =
   CommandLine.usage {usage = "[-mlton </path/to/mlton>] bench1 bench2 ...",
                      msg = msg}

val doOnce = ref false
val doWiki = ref false
val runArgs : string list ref = ref []
   


val benchCount = BenchmarkLib.benchCount

val default_main = (fn bench => concat ["val _ = Main.doit ", benchCount bench, "\n"])

local
   val next = Counter.generator 0
in
   fun makeMLton commandPattern =
      case ChoicePattern.expand commandPattern of
         Result.No m => usage m
       | Result.Yes cmds =>
            List.map
            (cmds, fn cmd =>
             let
                val abbrv = "MLton" ^ (Int.toString (next ()))
             in
                {name = cmd,
                 abbrv = abbrv,
                 main = default_main,
                 test = (fn {bench} =>
                          BenchmarkLib.runTest
                          {bench = bench,
                           config = {cmd = cmd,
                                     abbrv = abbrv,
                                     main = default_main},
                           runArgs = !runArgs,
                           doOnce = !doOnce})}
             end)
end


fun main (_, args) =
   let
      val compilers: {name: string,
                       abbrv: string,
                       main: string -> string,
                       test: {bench: File.t} -> {compile: real option,
                                                 run: real option,
                                                 size: Position.int option}} list ref 
        = ref []

      fun pushCompilers compilers' = compilers := (List.rev compilers') @ (!compilers)


      (* Set the stack limit to its max, since mlkit segfaults on some benchmarks
       * otherwise.
       *)
       val _ =
          let
             open MLton.Platform.OS
          in
             if host = Linux
                then
                   let
                      open MLton.Rlimit
                      val {hard, ...} = get stackSize
                   in
                      set (stackSize, {hard = hard, soft = hard})
                   end
             else ()
          end
      local
         open Popt
      in
         val res =
            parse
            {switches = args,
             opts = [("args",
                      SpaceString
                      (fn args =>
                       runArgs := String.tokens (args, Char.isSpace))),
                      ("mlton",
                       SpaceString (fn arg => pushCompilers
                                    (makeMLton arg))),
                      ("once", trueRef doOnce),
                      trace,
                      ("wiki", trueRef doWiki)]}
      end
   in
      case res of
         Result.No msg => usage msg
       | Result.Yes benchmarks =>
            let
               val compilers = List.rev (!compilers)
               val base = #abbrv (hd compilers)
               val _ =
                  let
                     open MLton.Signal
                  in
                     setHandler (Pervasive.Posix.Signal.pipe, Handler.ignore)
                  end
               val failures = ref []
               fun show (results, {showAll}) =
                  let
                     val s =
                        BenchmarkLib.formatResults
                        {compilers = List.map (compilers, fn {name, abbrv, ...} => {name = name, abbrv = abbrv}),
                         benchmarks = benchmarks,
                         failures = !failures,
                         doWiki = !doWiki,
                         showAll = showAll,
                         results = results}
                  in
                     BenchmarkLib.writeResults s
                  end
               val _ = BenchmarkLib.formatResult
               val totalFailures = ref []
               val data = 
                  List.fold
                  (benchmarks, [],
                   fn (bench, ac) =>
                   let
                      val foundOne = ref false
                      val res =
                         List.fold
                         (compilers, ac, fn ({name, abbrv, test, ...}, ac) =>
                          if true
                             then
                                let
                                   val {compile, run, size} = test {bench = bench}
                                   val _ =
                                      if name = base
                                         andalso Option.isNone run
                                         then List.push (failures, bench)
                                      else ()
                                   val _ =
                                      if Option.isSome compile
                                         orelse Option.isSome run
                                         orelse Option.isSome size
                                         then foundOne := true
                                      else ()
                                   val ac =
                                      {bench = bench,
                                       compiler = abbrv,
                                       compile = compile,
                                       run = run,
                                       size = size} :: ac
                                   val _ = show (ac, {showAll = false})
                                   val _ = Out.flush Out.standard
                                in
                                   ac
                                end
                          else ac)
                      val _ =
                         if !foundOne
                            then ()
                         else List.push (totalFailures, bench)
                   in
                      res
                   end)
               val _ = show (data, {showAll = true})
               val _ = Out.flush Out.standard
               val totalFailures = !totalFailures
               val _ =
                  if List.isEmpty totalFailures
                     then ()
                  else (print ("The following benchmarks failed completely.\n")
                        ; List.foreach (totalFailures, fn s =>
                                        print (concat [s, "\n"])))
            in ()
            end
   end

val main = CommandLine.make main

end
