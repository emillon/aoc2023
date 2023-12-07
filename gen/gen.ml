open Base
open Stdio

let create_dir path =
  try Unix.mkdir path 0o755
  with Unix.Unix_error (EEXIST, _, _) ->
    print_endline "Directory already exists, exiting.";
    Stdlib.exit 1

let outf oc fmt =
  Printf.ksprintf
    (fun s ->
      Out_channel.output_string oc s;
      Out_channel.output_char oc '\n')
    fmt

let generate_dune n oc =
  outf oc {|(library|};
  outf oc {| (name day%02d)|} n;
  outf oc {| (public_name aoc2023.day%02d)|} n;
  outf oc {| (modules day%02d)|} n;
  outf oc {| (libraries base lib stdio)|};
  outf oc {| (preprocess|};
  outf oc {|  (pps ppx_jane))|};
  outf oc {| (inline_tests))|};
  outf oc {||};
  outf oc {|(executable|};
  outf oc {| (name run)|};
  outf oc {| (modules run)|};
  outf oc {| (libraries day%02d))|} n;
  outf oc {||};
  outf oc {|(rule|};
  outf oc {| (write-file run.ml "let () = Day%02d.run ()"))|} n;
  outf oc {||};
  outf oc {|(rule|};
  outf oc {| (with-stdout-to|};
  outf oc {|  part1.txt.gen|};
  outf oc {|  (run ./run.exe %%{dep:input.txt})))|};
  outf oc {||};
  outf oc {|(rule|};
  outf oc {| (alias runtest)|};
  outf oc {| (action|};
  outf oc {|  (diff part1.txt part1.txt.gen)))|};
  outf oc {||};
  outf oc {|(rule|};
  outf oc {| (with-stdout-to|};
  outf oc {|  part2.txt.gen|};
  outf oc {|  (run ./run.exe --2 %%{dep:input.txt})))|};
  outf oc {||};
  outf oc {|(rule|};
  outf oc {| (alias runtest)|};
  outf oc {| (action|};
  outf oc {|  (diff part2.txt part2.txt.gen)))|}

let generate_lib oc =
  outf oc {|open Base|};
  outf oc {|open Lib|};
  outf oc {|open Stdio|};
  outf oc {||};
  outf oc {|let sample = []|};
  outf oc {||};
  outf oc {|let result _ = 0|};
  outf oc {|let result2 _ = 0|};
  outf oc {||};
  outf oc {|let run () =|};
  outf oc {|  match Sys.get_argv () with|};
  outf oc {|  | [| _; path |] ->|};
  outf oc
    {|      In_channel.read_all path |> parse |> result |> printf "%%d\n"|};
  outf oc {|  | [| _; "--2"; path |] ->|};
  outf oc
    {|      In_channel.read_all path |> parse |> result2 |> printf "%%d\n"|};
  outf oc {|  | _ -> assert false|}

let touch path = Out_channel.write_all path ~data:""

let generate n =
  let dirname = Printf.sprintf "day%02d" n in
  create_dir dirname;
  Out_channel.with_file
    (Stdlib.Filename.concat dirname "dune")
    ~f:(generate_dune n);
  Out_channel.with_file
    (Stdlib.Filename.concat dirname (dirname ^ ".ml"))
    ~f:generate_lib;
  touch (Stdlib.Filename.concat dirname "part1.txt");
  touch (Stdlib.Filename.concat dirname "part2.txt")

let () =
  match Sys.get_argv () with
  | [| _; n |] -> generate (Int.of_string n)
  | _ -> assert false
