(* Copyright (C) 2013,2014,2019,2022 Matthew Fluet.
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BenchmarkLib : BENCHMARK_LIB =
struct

type int = Int.t

fun ignoreOutput f =
   let
      val nullFd =
         let
            open Pervasive.Posix.FileSys
         in
            openf ("/dev/null", O_WRONLY, O.flags [])
         end
      open FileDesc
   in
      Exn.finally
      (fn () => fluidLet (stderr, nullFd, fn () =>
                          fluidLet (stdout, nullFd, f)),
       fn () => close nullFd)
   end

datatype command =
   Explicit of {args: string list,
                com: string}
  | Shell of string list

fun timeIt ca =
   Process.time
   (fn () =>
    case ca of
       Explicit {args, com} =>
          Process.waitChildPid (Process.spawnp {file = com, args = com :: args})
     | Shell ss => List.foreach (ss, Process.system))

local
   val trialTime = Time.seconds (IntInf.fromInt 60)
in
   fun timeCall {exe, runArgs, doOnce}: real =
      let 
         fun doit ac =
            let
               val {user, system} = timeIt (Explicit {args = runArgs, com = exe})
               val op + = Time.+
            in ac + user + system
            end
         fun loop (n, ac: Time.t): real =
            if Time.> (ac, trialTime)
               then Time.toReal ac / Real.fromInt n
            else loop (n + 1, doit ac)
      in 
         if doOnce
            then Time.toReal (doit Time.zero)
         else loop (0, Time.zero)
      end
end

fun size (f: File.t): {text: int, data: int, bss: int}  =
   let
      val fail = fn () => Process.fail (concat ["size failed on ", f])
   in
      File.withTemp
      (fn sizeRes =>
       let
          val _ = Process.system (concat ["size ", f, ">", sizeRes])
       in
          File.withIn
          (sizeRes, fn ins =>
           case In.lines ins of
              [_, nums] =>
                 (case String.tokens (nums, Char.isSpace) of
                     text :: data :: bss :: _ =>
                        (case (Int.fromString text,
                               Int.fromString data,
                               Int.fromString bss) of
                            (SOME text, SOME data, SOME bss) =>
                               {text = text, data = data, bss = bss}
                          | _ => fail ())
                     | _ => fail ())
              | _ => fail ())
       end)
   end

fun compileSizeRun {command, exe, doTextPlusData: bool, runArgs, doOnce} =
   Escape.new
   (fn e =>
    let
       val exe = "./" ^ exe
       val {system, user} = timeIt command
          handle _ => Escape.escape (e, {compile = NONE,
                                         run = NONE,
                                         size = NONE})
       val compile = SOME (Time.toReal (Time.+ (system, user)))
       val size =
          if doTextPlusData
             then
                let 
                   val {text, data, ...} = size exe
                in SOME (Position.fromInt (text + data))
                end
          else SOME (File.size exe)
       val run =
          timeCall {exe = exe, runArgs = runArgs, doOnce = doOnce}
          handle _ => Escape.escape (e, {compile = compile,
                                         run = NONE,
                                         size = size})
    in {compile = compile,
        run = SOME run,
        size = size}
    end)

fun batch_ {abbrv, bench} =
   let
      val abbrv =
         String.translate
         (abbrv, fn c =>
          if Char.isAlphaNum c
             then String.fromChar c
          else "_")
   in
      concat [bench, ".", abbrv, ".batch"]
   end

fun batch ab =
  concat [batch_ ab, ".sml"]

fun runTest {bench: string,
             config: {cmd: string,
                      abbrv: string,
                      main: string -> string},
             runArgs: string list,
             doOnce: bool} : {compile: real option,
                              run: real option,
                              size: Position.int option} =
   let
      val src = batch {abbrv = #abbrv config, bench = bench}
      val exe = String.dropSuffix (src, 4)
      val cmds = (concat [#cmd config, " -output ", exe, " ", src])::nil
      
      val _ =
         File.withOut
         (src, fn out =>
          (File.outputContents (concat [bench, ".sml"], out);
           Out.output (out, (#main config bench))))
   in
      ignoreOutput (fn () =>
         compileSizeRun {command = Shell cmds,
                         exe = exe,
                         doTextPlusData = true,
                         runArgs = runArgs,
                         doOnce = doOnce})
   end

end
