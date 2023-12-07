This creates a directory named after the day.

  $ ./gen.exe 3
  $ ls
  day03
  gen.exe

If it already exists, we fail.

  $ ./gen.exe 3
  Directory already exists, exiting.
  [1]

The contents are the following:

  $ find day03 | sort
  day03
  day03/day03.ml
  day03/dune
  day03/part1.txt
  day03/part2.txt

  $ cat day03/dune
  (library
   (name day03)
   (public_name aoc2023.day03)
   (modules day03)
   (libraries base stdio angstrom diet)
   (preprocess
    (pps ppx_jane))
   (inline_tests))
  
  (executable
   (name run)
   (modules run)
   (libraries day03))
  
  (rule
   (write-file run.ml "let () = Day03.run ()"))
  
  (rule
   (with-stdout-to
    part1.txt.gen
    (run ./run.exe %{dep:input.txt})))
  
  (rule
   (alias runtest)
   (action
    (diff part1.txt part1.txt.gen)))
  
  (rule
   (with-stdout-to
    part2.txt.gen
    (run ./run.exe --2 %{dep:input.txt})))
  
  (rule
   (alias runtest)
   (action
    (diff part2.txt part2.txt.gen)))

  $ cat day03/day03.ml
  open Base
  open Stdio
  
  let sample = []
  
  let result _ = 0
  let result2 _ = 0
  
  let run () =
    match Sys.get_argv () with
    | [| _; path |] ->
        In_channel.read_all path |> parse |> result |> printf "%d\n"
    | [| _; "--2"; path |] ->
        In_channel.read_all path |> parse |> result2 |> printf "%d\n"
    | _ -> assert false
