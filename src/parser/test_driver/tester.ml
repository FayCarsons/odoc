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
    "basic", "{[foo]}";
    "empty", "{[]}";
    "whitespace only", "{[ ]}";
    "blank line only", "{[\n  \n]}";
    "whitespace", "{[foo bar]}";
    "newline", "{[foo\nbar]}";
    "carriage return", "{[foo\r\nbar]}";
    "contains blank line", "{[foo\n\nbar]}";
    "leading whitespace", "{[ foo]}";
  ]

(* Cases (mostly) taken from the 'odoc for library authors' document *)
let documentation_cases =
  [ 
    "Light list", "- a\n - b\n - c"; 
    "Heavy list", "{ul {li foo} {li bar} {li baz}}";
    "Simple ref", "{!Stdlib.Buffer}";
    "Ref w/ replacement", "{{: https://ocaml.org/ }the OCaml website}";
    "Modules", "{!modules: Foo Bar Baz}";
    "Block tag", "@see \"foo.ml\" bar baz quux\n";
    "Inline tag", "@author Fay Carsons";
    "Simple tag", "@open";
    "Math", "{math \\sum_{i=0}^n x^i%}";
    "Inline_math", "{m x + 4}";
    "Heavy table", "{table
                    {tr
                      {th Header 1}
                      {th Header 2}
                      {th Header 3}
                    }
                    {tr
                      {td Cell 1}
                      {td Cell with {e emphasized content}}
                      {td {v a block v} }
                    }
                  }";
    "Light table", "{t
                    | Header 1 | Header 2 | Header 3 | Header 4|
                    | :------: | --------:|:---------|---------|
                    | centered | right    | left     | default |
                      omitted  | bar at   | start and| finish
                    | {e emph} | and | unaligned | bars |
                  }";
    "Styled", "{i italicized}";
    "Inline code", "[fmap Fun.id None]";
    "Code block", "{[\n let foo = 0 \n]}"
  ]

let run_test (label, case) =
  let tokens = ref [] in 
  let unwrap_token lexbuf =
    let open Odoc_parser.Tester in 
    let Odoc_parser.Loc.{ value; _ } = token lexbuf in 
    tokens := (string_of_token value ^ "\n") :: !tokens;
    value
  in
  try 
    let lexbuf = Lexing.from_string case in
    let _ = Odoc_parser.Tester.parse unwrap_token lexbuf in
    Printf.printf "%s: SUCCESSFUL\n" label
  with _ ->
    let tokens_string = List.fold_left (fun acc token -> acc ^ " " ^ token) "" @@ List.rev !tokens in
    Printf.printf 
      "%s: FAILED\nTOKENS:\n%s\n" 
      label 
      tokens_string
    

let () = 
  let cases = 
    if Array.length Sys.argv > 1 then
      match Sys.argv.(1) with 
      | "code" -> code_cases
      | _ -> 
        print_endline "unrecognized argument - running documentation_cases";
        documentation_cases
    else 
      documentation_cases
  in 
  List.iter run_test cases
