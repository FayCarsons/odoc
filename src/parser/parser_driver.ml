open Parser

module Interpreter = MenhirInterpreter

module Context = struct
  type t = {
    stack : context list;
        (* This stack keeps track of the tree of delimited
           elements that make up a comment. Similar to using a stack to find
           missing delimiters, the thing we are currently in is on the top of
           the stack, and when the token that closes it is discovered it gets
           popped. *)
    tokens : Parser.token list;
    (* All tokens encountered. This allows us to look back into the input to get
       more context when we encounter an error *)
    last_valid_checkpoint : Ast.t Interpreter.checkpoint option;
        (* Last `InputNeeded`
           before an error state *)
  }

  and context =
    [ (* Recursive inline elements *)
      `Style of
      Ast.t Interpreter.checkpoint * unit
    | `Reference
    | `Link
    | (* Block elements *)
      `Paragraph
    | `Code
    | `List
    | `Table ]

  let create () =
    let stack = [] and tokens = [] in
    { stack; tokens; last_valid_checkpoint = None }

  let update_context checkpoint production =
    let open Interpreter in
    match lhs production with
    | X (N N_list_heavy) -> Some (`Inline_element checkpoint)
    | X (N N_nestable_block_element) -> Some (`Block_element checkpoint)
    | _ -> None

  let push self context = { self with stack = context :: self.stack }
  let pop_if self ~predicate =
    match self.stack with
    | x :: xs when predicate x -> ({ self with stack = xs }, Some x)
    | _ -> (self, None)

  let matching_delimiter (context : context) ~(token : Parser.token) =
    match (context, token) with _ -> true

  let pop = function
    | { stack = x :: xs; _ } as self -> ({ self with stack = xs }, Some x)
    | self -> (self, None)

  let peek = function { stack = x :: _; _ } -> Some x | _ -> None
  let is_some_and predicate = function Some x -> predicate x | _ -> false
  let update self token =
    let push_context =
      match
        raise
        @@ Failure "unimplemented branch in parser_driver.ml - Context.update"
      with
      | Some ctx -> { self with stack = ctx :: self.stack }
      | None ->
          if peek self |> is_some_and (matching_delimiter ~token) then
            fst @@ pop self
          else self
    in
    { push_context with tokens = token :: push_context.tokens }
end

let add_warning input warning =
  let open Lexer in
  { input with warnings = warning :: input.warnings }

let rec pop_until ~predicate env =
  let items = Option.map predicate (Interpreter.top env) in
  match items with
  | Some [] ->
      (* NOTE: (@FayCarsons) I think this is safe and shouldn't throw, but I
         need to study this a bit more before I'm confident *)
      let new_env = Interpreter.pop env in
      Option.map (pop_until ~predicate) new_env |> Option.get
  | Some xs -> xs
  | _ -> []

let find_context = function
  | Interpreter.InputNeeded (env : Ast.t Interpreter.env) ->
      pop_until ~predicate:(Fun.const []) env
  | _ -> assert false

let contextual_error_message lexer checkpoint continuation =
  let context = find_context checkpoint in
  raise @@ Failure "Unimplemented"

(* Something like this, still needs a lot of fleshing out *)

let run_parser ~(input : Lexer.input) ~(location : Lexing.position)
    ~(text : string) : Ast.t =
  let open Interpreter in
  let rec handle_error ~context ~lexbuf (checkpoint : Ast.t checkpoint) =
    contextual_error_message lexbuf checkpoint (run ~context ~input ~lexbuf)
  and run ~(context : Context.t) ~(input : Lexer.input)
      ~(lexbuf : Lexing.lexbuf) : Ast.t checkpoint -> Ast.t = function
    | InputNeeded env as checkpoint ->
        let Loc.{ value = token; _ } = Lexer.token input lexbuf in
        let startp = lexbuf.Lexing.lex_start_p
        and endp = lexbuf.Lexing.lex_curr_p in
        let checkpoint = Interpreter.offer checkpoint (token, startp, endp)
        and context = Context.update context token in
        run ~context ~input ~lexbuf checkpoint
    | Shifting (before, after, still_parsing) as checkpoint -> assert false
    | AboutToReduce (_, production) as checkpoint ->
        run ~context ~input ~lexbuf (Interpreter.resume checkpoint)
    | (HandlingError _ | Rejected) as checkpoint ->
        handle_error ~context ~lexbuf checkpoint
    | Accepted ast -> ast
  in
  let lexbuf = { (Lexing.from_string text) with lex_curr_p = location } in
  let initial_checkpoint = Incremental.main lexbuf.lex_curr_p
  and context = Context.create () in
  run ~context ~input ~lexbuf initial_checkpoint
