open! Odoc_parser_test

let cases =
  [ 
    "Light list", "- a\n - b\n - c"; 
    "Heavy list", "{ul {li foo} {li bar} {li baz}}";
    "Simple ref", "{!Stdlib.Buffer}";
    "Ref w/ replacement", "{{: https://ocaml.org/ }the OCaml website}";
    "Modules", "{!modules: A B C}";
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
    

let () = List.iter run_test cases
