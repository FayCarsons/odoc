open Parser

module Interpreter = MenhirInterpreter

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n" s

module Context = struct
  open Interpreter

  let ( >>= ) = Option.bind

  type 'a t = {
    file : string;
    last_valid : 'a checkpoint;
    current : focus option;
    tokens : (Parser.token * Lexing.position * Lexing.position) list;
  }
  and focus = Main | Inline | Block | Tag

  let create ~(file : string) ~beginning_checkpoint:last_valid =
    { file; last_valid; current = None; tokens = [] }

  let pp_checkpoint = function
    | InputNeeded _ -> "Input needed (Parser needs token)"
    | AboutToReduce _ ->
        "About to reduce (replacing token sequence with symbol)"
    | Shifting _ -> "Shifting (accepting token into stack)"
    | Accepted _ -> "Accepted (successfully parsed input)"
    | HandlingError _ -> "Handling error"
    | Rejected -> "Rejected"

  let string_of_focus : focus option -> string = function
    | Some Main -> "Comment"
    | Some Inline -> "Inline element"
    | Some Block -> "Block element"
    | Some Tag -> "Tag element"
    | None -> "No match"

  let push_token self token = { self with tokens = token :: self.tokens }
  let peek :
      type a. a t -> (Parser.token * Lexing.position * Lexing.position) option =
    function
    | { tokens = x :: _; _ } -> Some x
    | _ -> None

  let pp self =
    let current =
      Option.map
        (function
          | Main -> "Main"
          | Inline -> "Inline"
          | Block -> "Block"
          | Tag -> "Tag")
        self.current
      |> Option.value ~default:"Haven't matched symbol"
    and most_recent_token =
      let token =
        Option.map (fun (t, _, _) -> Describe.describe t) (peek self)
      in
      Option.value ~default:"No tokens" token
    in
    Printf.printf "{ checkpoint: %s; current: %s; most_recent_token: %s }\n%!"
      (pp_checkpoint self.last_valid)
      current most_recent_token

  let get_focus_element : type a. a nonterminal -> focus option = function
    | N_main -> Some Main
    | N_inline_element -> Some Inline
    | N_nestable_block_element -> Some Block
    | N_tag -> Some Tag
    | _ -> None

  let get_focus (production, _) =
    match lhs production with
    | X (N nonterminal_symbol) -> get_focus_element nonterminal_symbol
    | _ -> None

  let update_focus self = function
    | Shifting (before, _after, _what_is_this_bool) ->
        top before
        >>= (function
              | Element (state, _, _, _) ->
                  List.find_map get_focus (items state))
        >>= (fun elt -> Some { self with current = Some elt })
        |> Option.value ~default:self
    | _ -> self

  let context_string : 'a t -> string = fun self -> string_of_focus self.current
end

let loc_of_positions :
    file:string ->
    start_pos:Lexing.position ->
    end_pos:Lexing.position ->
    Loc.span =
 fun ~file ~start_pos ~end_pos ->
  let of_position pos =
    let open Lexing in
    Loc.{ line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol }
  in
  let start = of_position start_pos and end_ = of_position end_pos in
  Loc.{ file; start; end_ }

(*-----------------------------------------------------------------------------*)

let keep_predictions acc (production, focus) =
  if focus < List.length (Interpreter.rhs production) then
    Interpreter.lhs production :: acc
  else acc

let _find_context (env : 'a Interpreter.env) =
  let not_empty (Interpreter.Element (state, _, _, _)) =
    Interpreter.items state |> List.fold_left keep_predictions []
  in
  let rec find env =
    let items = Option.map not_empty (Interpreter.top env) in
    match items with
    | Some [] ->
        Option.map find (Interpreter.pop env) |> Option.value ~default:[]
    | Some xs -> xs
    | _ -> []
  in
  find env

let dummy_table : Ast.table = (([ [] ], None), `Light)
let dummy_inline_element : Ast.inline_element = `Word "oops"
let how_many_times = ref 0

let contextual_error_message (type a b) ~(context : a Context.t)
    ~(checkpoint : a Interpreter.checkpoint)
    ~(continuation : a Interpreter.checkpoint -> Ast.t) =
  incr how_many_times;
  Printf.printf "We have traversed the error branch of the program %d times\n"
    !how_many_times;
  match checkpoint with
  | Interpreter.InputNeeded env | Interpreter.HandlingError env ->
      let offending_token = Context.peek context in
      Printf.printf
        "Handling an error: this is the offending token(?) \'%s\'\n%!"
        (Option.map (fun (token, _, _) -> Describe.print token) offending_token
        |> Option.value ~default:"NO TOKENS");
      let _, start_pos, end_pos = List.hd context.tokens in
      print_endline "Got position from context";
      let env =
        Interpreter.feed
          Interpreter.(N N_inline_element)
          start_pos dummy_inline_element end_pos env
      in
      print_endline
        "Fed dummy value to interpreter - contructing new checkpoint";
      (* TODO: THIS SEGFAULTS with the current light-table example I am feeding
          it. I think that's because when it encounters the '}' token it believes
          it should be reducing to an Ast.inline_element but I replace it with an
          Ast.table

         Ok I think I've verified the above, so what we need to do here is
         essentially determine which container/higher-position symbol(I.E.
         Inline element, block element, tag) we're currently inside of and
         instead of returning to the current checkpoint, return to the checkpoint
         where we matched on that symbol with a dummy AST node?
      *)
      let checkpoint = Interpreter.input_needed env in
      continuation checkpoint
  | _ -> unreachable "contextual_error_message"

let get_parse_error (type a) env : Interpreter.item list =
  match Interpreter.stack env with
  | (lazy (Cons (Interpreter.Element (state, _, _, _), _))) ->
      Interpreter.items state
  | (lazy Nil) -> unreachable "get_parse_error"

(* This is the function we call to insert a dummy node into the AST & continue
   parsing when we encounter an error *)
let offer_solution :
    type a.
    lexbuf:Lexing.lexbuf ->
    valid_node:a ->
    symbol:a Interpreter.symbol ->
    Ast.t Interpreter.env ->
    Ast.t Interpreter.env =
 fun ~lexbuf ~valid_node ~symbol env ->
  let start_pos = lexbuf.Lexing.lex_start_p
  and end_pos = lexbuf.Lexing.lex_curr_p in
  Interpreter.feed symbol start_pos valid_node end_pos env

(*-----------------------------------------------------------------------------*)

(* NOTE: Use `Interpreter.force_reduction ` to force the parser to continue w/
    parsing in the event of an error.
    Essentially, the error handling should look something like:
    - Get context. What happened, where are we, what is the specific Odoc
      element which cannot be parsed and which error message do we need to
      push to `warnings`.
    - Find jump destination. What state should the parser resume in?
      The most trivial example would be in the case of something like
      `(** @ *)` where our only option would be to jump to EOF after pushing a
      "stray @" warning to the warning list. In that case,
      do we simply stop parsing and return an empty AST?
    Another potentially valuable function:
    `Interpreter.feed symbol start_pos semantic_value end_pos env` *)

(* Production, a specific nonterminal symbol with a left-hand and right-hand
   side.

   the Left Hand Side (LHS) is a symbol representing the production itself(???)
   and the Right Hand Side (RHS) is the, potentially terminal, symbols which
   break down into the sequence of tokens being matched on by the production.

   My understanding:
   for the rule
   `let whitespace :=
    | ~ = Space; <>
    | SPACE; { " " }`

    there are two productions
    N_whitespace -> T_Space
    N_whitespace -> T_SPACE
*)

(* Something like this, still needs a lot of fleshing out *)
let run_parser ~(starting_location : Lexing.position)
    ~(next_token : unit -> Parser.token * Lexing.position * Lexing.position)
    ~(push_warning : Warning.t -> unit) : Ast.t =
  let open Interpreter in
  let rec handle_error (type a b) ~context checkpoint =
    contextual_error_message ~context ~checkpoint ~continuation:(run ~context)
  and run ~context = function
    | InputNeeded _env as checkpoint ->
        let token = next_token () in
        let checkpoint = Interpreter.offer checkpoint token
        and context = Context.push_token context token in
        run ~context checkpoint
    | Shifting _ as checkpoint ->
        let context = Context.update_focus context checkpoint in
        run ~context (Interpreter.resume checkpoint)
    | AboutToReduce _ as checkpoint -> run ~context (resume checkpoint)
    | Rejected -> failwith "Input rejected"
    | HandlingError _ as checkpoint ->
        (* Use `Interpreter.force_reduction` to force the parser to continue w/
            parsing in the event of an error.

           Essentially, the error handling should look something like:
           - Get context. What happened, where are we, what is the specific Odoc
           element which cannot be parsed and which error message do we need to
           push to `warnings`.
           - Find jump destination. Where can we jump to? The most trivial example
           would be in the case of something like
           `(** @ *)` where our only option would be to jump to EOF. In that case,
           do we simply stop parsing and return an empty AST? *)
        print_endline "IN ERROR ARM";
        Context.pp context;
        let token = Context.peek context in
        Option.iter
          (fun (token, start_pos, end_pos) ->
            let span =
              loc_of_positions ~file:context.file ~start_pos ~end_pos
            in
            let warning =
              Parse_error.not_allowed
                ~what:(String.escaped @@ Describe.print token)
                ~in_what:(Context.context_string context)
                span
            in
            push_warning warning)
          token;
        handle_error ~context checkpoint
    | Accepted ast -> ast
  in
  let beginning_checkpoint = Incremental.main starting_location in
  let context =
    Context.create ~file:starting_location.Lexing.pos_fname
      ~beginning_checkpoint
  in
  run ~context beginning_checkpoint
