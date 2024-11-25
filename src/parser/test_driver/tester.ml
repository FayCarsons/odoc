open! Odoc_parser_test

module Loc = Odoc_parser.Loc

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

let error_recovery =
  [
    ("Empty Italic", "{i}");
    ("Empty bold", "{b}");
    ("Light\nlist", "- foo\n - bar\n- baz");
    ("Empty ref w/ replacement", "{{!https://ocaml.org}}");
  ]

let see = [ ("See", "@see <foo> bar baz quux\n") ]

(* Cases (mostly) taken from the 'odoc for library authors' document *)
let documentation_cases =
  [
    ("Light list", "- foo\n- bar");
    ("Heavy list", "{ul {li foo} {li bar} {li baz}}");
    ("Simple ref", "{!Stdlib.Buffer}");
    ("Ref w/ replacement", "{{: https://ocaml.org/ }the OCaml website}");
    ("Modules", "{!modules: Foo Bar Baz}");
    ("Block tag", "@see <foo.ml> bar baz quux\n");
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
      {|{t\n\
        | Header 1 | Header 2 | Header 3 | Header 4|\n
        | :------: | --------:|:---------|---------|\n
        | centered | right    | left     | default |\n
          omitted  | bar at   | start and| finish\n
        | {e emph} | and | unaligned | bars |\n}|}
    );
    ("Styled", "{i italicized}");
    ("Inline code", "[fmap Fun.id None]");
    ("Code block", "{[\n let foo = 0 \n]}");
  ]

type failure = {
  label : string;
  case : string;
  tokens : Odoc_parser.Tester.token list;
  failed_at : int;
}

let tokens_string tokens =
  List.fold_left
    (fun acc token -> acc ^ "\n" ^ token)
    ""
    (List.map Odoc_parser.Tester.string_of_token tokens)

open Test.Serialize

let error err = Atom (Odoc_parser.Warning.to_string err)
let parser_output formatter (ast, warnings) =
  let value = Ast_to_sexp.(docs loc_at ast) in
  let warnings = List (List.map error warnings) in
  let output =
    List [ List [ Atom "output"; value ]; List [ Atom "warnings"; warnings ] ]
  in
  Sexplib0.Sexp.pp_hum formatter output;
  Format.pp_print_flush formatter ()

let run_test (failed : failure list) (label, case) =
  let module Parser = Odoc_parser.Tester in
  let unwrap_token lexbuf =
    let open Parser in
    let Loc.{ value; _ } = token lexbuf in
    value
  in
  let rec get_tokens lexbuf acc =
    let token = unwrap_token lexbuf in
    if Odoc_parser.Tester.is_EOI token then List.rev (token :: acc)
    else get_tokens lexbuf (token :: acc)
  in
  let lexbuf = Lexing.from_string case in
  let tokens_cached = get_tokens lexbuf [] in
  let idx = ref @@ -1 in
  try
    let tokens = ref tokens_cached in
    let intermediate =
      Odoc_parser.Tester.main
        (fun _ ->
          match !tokens with
          | t :: ts ->
              incr idx;
              tokens := ts;
              t
          | [] -> failwith "No more tokens")
        lexbuf
    in
    let ast, warnings = Parser.run ~filename:"" intermediate in
    Format.printf "%a\n" parser_output (ast, warnings);
    print_newline ();
    Printf.printf "Got %d warnings \n%!" (List.length warnings);
    Printf.printf "Warnings:\n%s\n%!"
      (List.fold_left
         (fun acc warning -> acc ^ "\n" ^ Parser.pp_warning warning)
         "" warnings);
    failed
  with _ ->
    { case; label; tokens = tokens_cached; failed_at = !idx } :: failed

let () =
  let cases =
    if Array.length Sys.argv > 1 then (
      match Sys.argv.(1) with
      | "code" -> code_cases
      | "recovery" | "r" -> error_recovery
      | "docs" | "d" -> documentation_cases
      | "see" | "s" -> see
      | _ ->
          print_endline "unrecognized argument - running documentation_cases";
          documentation_cases)
    else documentation_cases
  in
  List.fold_left run_test [] cases
  |> List.iter (fun { label; case; tokens; failed_at } ->
         Printf.printf
           "Failure: %s\nInput:\n%s\nOffending token:\n%d: %s\nTokens: %s\n\n"
           label case failed_at
           (List.nth tokens failed_at |> Odoc_parser.Tester.string_of_token)
           (tokens_string tokens))
