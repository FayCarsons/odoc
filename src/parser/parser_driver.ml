open Parser

module Interpreter = MenhirInterpreter

let rec loop ~(input : Lexer.input) lexbuf = function
  | Interpreter.InputNeeded _ as checkpoint -> 
    let Loc.{ value = token; _ } = Lexer.token input lexbuf in
    let startp = lexbuf.Lexing.lex_start_p and endp = lexbuf.Lexing.lex_curr_p in 
    let checkpoint = Interpreter.offer checkpoint (token, startp, endp) in 
    loop ~input lexbuf checkpoint
  | (Interpreter.Shifting _ | Interpreter.AboutToReduce _) as checkpoint -> 
    loop ~input lexbuf @@ Interpreter.resume checkpoint
  | Interpreter.HandlingError _env -> 
    (* Here is where we'd want to check *)
    Printf.printf "Syntax error at offset %d\n" (Lexing.lexeme_start lexbuf);
    exit 1
  | Interpreter.Accepted ast -> ast
  | Interpreter.Rejected -> 
    (* This is unreachable because we panic when we encounter a `HandlingError` variant *)
    assert false 
    
    

let parse ~(location : Lexing.position) ~(text : string): Ast.t = 
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- location;
  assert false

