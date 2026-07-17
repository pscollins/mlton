(* Copyright (C) 2013,2014,2019,2022 Matthew Fluet.
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BENCHMARK_LIB =
sig

   (* Redirects both standard output (stdout) and standard error (stderr)
    * to /dev/null while running the given function f, restoring them
    * afterwards.
    *)
   val ignoreOutput: (unit -> 'a) -> 'a

   (* Represents a compilation or execution command to run.
    * - Explicit: runs a command path directly with a list of arguments.
    * - Shell: runs a sequence of command lines in a shell.
    *)
   datatype command =
      Explicit of {args: string list,
                   com: string}
     | Shell of string list

   (* Executes the given command and measures the system and user CPU time
    * spent running it.
    *)
   val timeIt: command -> {system: Time.t, user: Time.t}

   (* Runs a compiled benchmark executable repeatedly (accumulating up to 60
    * seconds total) and returns the average run time in seconds. If doOnce
    * is true, it runs the executable only once.
    *)
   val timeCall: {exe: string, runArgs: string list, doOnce: bool} -> real

   (* Invokes the 'size' shell utility on the compiled binary file,
    * parsing its output to retrieve the text, data, and bss segment sizes.
    *)
   val size: File.t -> {text: Int.t, data: Int.t, bss: Int.t}

   (* Measures compilation CPU time, executable segment/file size, and average
    * execution run time for a given compilation command and executable name.
    *)
   val compileSizeRun: {command: command,
                        exe: string,
                        doTextPlusData: bool,
                        runArgs: string list,
                        doOnce: bool} -> {compile: real option,
                                          run: real option,
                                          size: Position.int option}

   (* Formats the batch SML source file name for a given compiler abbreviation
    * and benchmark name (e.g. "barnes-hut.MLton0.batch.sml").
    *)
   val batch: {abbrv: string, bench: string} -> string

   (* Runs a complete benchmark test: writes the driver SML batch file (which
    * merges the original benchmark source and the main driver loop code),
    * compiles it, determines sizes, and measures execution time.
    *
    * Returns the measured compile time, average run time, and executable size.
    *)
   val runTest: {bench: string,
                 config: {cmd: string,
                          abbrv: string,
                          main: string -> string},
                 runArgs: string list,
                 doOnce: bool} -> {compile: real option,
                                   run: real option,
                                   size: Position.int option}

   type 'a data = {bench: string,
                   compiler: string,
                   value: 'a} list

   (* Formats the benchmark execution results into a string.
    * Optionally generates wiki-formatted output if doWiki is true.
    *)
   val formatResults: {compilers: {name: string, abbrv: string} list,
                       benchmarks: string list,
                       failures: string list,
                       doWiki: bool,
                       showAll: bool,
                       results: {compiles: real data,
                                 runs: real data,
                                 sizes: Position.int data}} -> string

   (* Prints the formatted results string to the appropriate stream (stdout) and flushes.
    *)
   val writeResults: string -> unit

end
