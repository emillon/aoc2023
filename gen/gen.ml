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
  outf oc {|open! Lib|};
  outf oc {|open Stdio|};
  outf oc {||};
  outf oc {|let sample = []|};
  outf oc {|type t = unit|};
  outf oc {|[@@deriving sexp]|};
  outf oc {|let parse _ = ()|};
  outf oc {|let%%expect_test "parse" =|};
  outf oc {|  parse sample |> [%%sexp_of: t] |> print_s;|};
  outf oc {x|  [%%expect {| () |}]|x};
  outf oc {||};
  outf oc {|let result _ = 0|};
  outf oc {|let%%expect_test "result" =|};
  outf oc {|  parse sample |> result |> printf "%%d\n";|};
  outf oc {x|  [%%expect {| 0 |}]|x};
  outf oc {|let result2 _ = 0|};
  outf oc {|let%%expect_test "result2" =|};
  outf oc {|  parse sample |> result2 |> printf "%%d\n";|};
  outf oc {x|  [%%expect {| 0 |}]|x};
  outf oc {||};
  outf oc {|let run () = main All parse result result2|}

type data_file = { basename : string; write : Out_channel.t -> unit }

let data_files n =
  let dirname = Printf.sprintf "day%02d" n in
  [
    { basename = "dune"; write = generate_dune n };
    { basename = dirname ^ ".ml"; write = generate_lib };
    {
      basename = "part1.txt";
      write = (fun oc -> Out_channel.output_string oc "0\n");
    };
    {
      basename = "part2.txt";
      write = (fun oc -> Out_channel.output_string oc "0\n");
    };
    { basename = "input.txt"; write = ignore };
  ]

let generate_data_files dir l =
  List.iter l ~f:(fun { basename; write } ->
      let path = Stdlib.Filename.concat dir basename in
      Out_channel.with_file path ~f:write)

let generate n =
  let dirname = Printf.sprintf "day%02d" n in
  create_dir dirname;
  generate_data_files dirname (data_files n)

let () =
  match Sys.get_argv () with
  | [| _; n |] -> generate (Int.of_string n)
  | _ -> assert false
