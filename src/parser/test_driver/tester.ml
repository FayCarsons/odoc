open! Odoc_parser_test

(*
    NOTE (@FayCarsons): for anyone working on this parser - this is probably
    the easiest way to check if something is working. The tests are numerous
    and difficult to parse. A test suite will fail in its entirety if one case
    throws an exception.

    If you need to test a specific parser rule or test case, add it here and
    `dune exec tester -- {test cases you want to run}`
*)

let code_cases =
  [
    ("basic", "{[foo]}");
    ("empty", "{[]}");
    ("whitespace only", "{[ ]}");
    ("blank line only", "{[\n  \n]}");
    ("whitespace", "{[foo bar]}");
    ("newline", "{[foo\nbar]}");
    ("carriage return", "{[foo\r\nbar]}");
    ("contains blank line", "{[foo\n\nbar]}");
    ("leading whitespace", "{[ foo]}");
  ]

(* Cases (mostly) taken from the 'odoc for library authors' document *)
let documentation_cases =
  [
    ("Light list", "- a\n - b\n - c");
    ("Heavy list", "{ul {li foo} {li bar} {li baz}}");
    ("Simple ref", "{!Stdlib.Buffer}");
    ("Ref w/ replacement", "{{!https://ocaml.org/}the OCaml website}");
    ("Modules", "{!modules: Foo Bar Baz}");
    ("Block tag", "@see \"foo.ml\" bar baz quux\n");
    ("Inline tag", "@author Fay Carsons");
    ("Simple tag", "@open");
    ("Math", "{math \\sum_{i=0}^n x^i%}");
    ("Inline_math", "{m x + 4}");
    ( "Heavy table",
      "{table\n\
      \                    {tr\n\
      \                      {th Header 1}\n\
      \                      {th Header 2}\n\
      \                      {th Header 3}\n\
      \                    }\n\
      \                    {tr\n\
      \                      {td Cell 1}\n\
      \                      {td Cell with {e emphasized content}}\n\
      \                      {td {v a block v} }\n\
      \                    }\n\
      \                  }" );
    ( "Light table",
      "{t\n\
      \                    | Header 1 | Header 2 | Header 3 | Header 4|\n\
      \                    | :------: | --------:|:---------|---------|\n\
      \                    | centered | right    | left     | default |\n\
      \                      omitted  | bar at   | start and| finish\n\
      \                    | {e emph} | and | unaligned | bars |\n\
      \                  }" );
    ("Styled", "{i italicized}");
    ("Inline code", "[fmap Fun.id None]");
    ("Code block", "{[ let foo = 0 ]}");
  ]

let error_recovery =
  [ ("Stray at", "@"); ("Empty ref", "{!}"); ("Unmatched paren", "{!(.*()}") ]

let message_api =
  [
    ( "Multi-line",
      "These are some words\n\
       These are also some words\n\
       The next line should fail\n\
       {i}" );
    ("Blank line in list", "{ol \n\n}");
    ("Stray table", "{t");
    ("Empty style", "{i}");
  ]

let _print_token_list tokens =
  print_endline "Tokens: ";
  List.rev tokens
  |> List.fold_left
       (fun s token -> s ^ "\n" ^ Odoc_parser.Tester.string_of_token token)
       ""
  |> print_endline

let run_test (label, input_text) : (string, string) result =
  let module Parse = Odoc_parser.Tester in
  Printf.printf ">>> Running %s >>>\n\n" label;
  let input = Parse.dummy_loc in
  input.warnings <- [];
  let lexbuf = Lexing.from_string input_text in
  let push_warning warning = input.warnings <- warning :: input.warnings in
  let starting_location =
    Lexing.{ pos_fname = "f.ml"; pos_bol = 0; pos_cnum = 0; pos_lnum = 0 }
  in
  try
    let _ast =
      Parse.parse ~input_text ~starting_location ~input:Parse.dummy_loc ~lexbuf
        ~push_warning
    in
    let _warnings = input.warnings in
    print_newline ();
    print_newline ();
    Ok (Printf.sprintf "%s: successful" label)
  with exc ->
    let reason = Printexc.to_string exc in
    Error (Printf.sprintf "!Case \'%s\' failed:\n%s" label reason)

let () =
  let cases =
    if Array.length Sys.argv > 1 then (
      match Sys.argv.(1) with
      | "code" -> code_cases
      | "doc" -> documentation_cases
      | "errors" -> error_recovery
      | "messages" -> message_api
      | _ ->
          print_endline "unrecognized argument - running error cases";
          documentation_cases)
    else documentation_cases
  in
  let results = List.map run_test cases in
  List.iter
    (function Ok s -> print_endline s | Error e -> print_endline e)
    results
