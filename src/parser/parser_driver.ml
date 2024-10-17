open Parser

module Interpreter = MenhirInterpreter

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n" s

module Context : sig
  type with_position = Parser.token * Lexing.position * Lexing.position

  type 'a t = {
    file : string;
    last_valid : 'a Interpreter.checkpoint;
    current_symbol : context option;
    push_warning : Warning.t -> unit;
    tokens : with_position list;
  }
  and context =
    | Main
    | Block of block_elt option
    | Tag of tag_elt option
    | Heading
  and block_elt = A
  and tag_elt = B

  val create :
    file:string ->
    push_warning:(Warning.t -> unit) ->
    first_checkpoint:'a Interpreter.checkpoint ->
    'a t

  val push_token : 'a t -> with_position -> 'a t
  val peek : 'a t -> with_position option
  val update_focus : 'a t -> 'b Interpreter.checkpoint -> 'a t

  val context_string : 'a t -> string
  val to_string : 'a t -> string
end = struct
  open Interpreter

  let ( >>= ) = Option.bind

  type with_position = Parser.token * Lexing.position * Lexing.position

  type 'a t = {
    file : string;
    last_valid : 'a checkpoint;
    current_symbol : context option;
    push_warning : Warning.t -> unit;
    tokens : with_position list;
  }
  and context =
    | Main
    | Block of block_elt option
    | Tag of tag_elt option
    | Heading
  and block_elt = A
  and tag_elt = B

  let create (type a) ~(file : string) ~(push_warning : Warning.t -> unit)
      ~(first_checkpoint : a checkpoint) =
    {
      file;
      push_warning;
      last_valid = first_checkpoint;
      current_symbol = None;
      tokens = [];
    }

  let pp_checkpoint = function
    | InputNeeded _ -> "Input needed (Parser needs token)"
    | AboutToReduce _ ->
        "About to reduce (replacing token sequence with symbol)"
    | Shifting _ -> "Shifting (accepting token into stack)"
    | Accepted _ -> "Accepted (successfully parsed input)"
    | HandlingError _ -> "Handling error"
    | Rejected -> "Rejected"

  let string_of_context : context option -> string = function
    | Some Main -> "Main"
    | Some (Block _) -> "Block element"
    | Some (Tag _) -> "Tag element"
    | Some Heading -> "Header"
    (* This is equivalent to matching on `None` as we are only using a subset of
       the `nonterminal` type *)
    | _ -> "No matches"

  let push_token self token = { self with tokens = token :: self.tokens }
  let peek : type a. a t -> with_position option = function
    | { tokens = x :: _; _ } -> Some x
    | _ -> None

  let to_string self =
    let current = string_of_context self.current_symbol
    and most_recent_token =
      let token =
        Option.map (fun (t, _, _) -> Describe.describe t) (peek self)
      in
      Option.value ~default:"No tokens" token
    in
    Printf.sprintf "{ checkpoint: %s; current: %s; most_recent_token: %s }\n%!"
      (pp_checkpoint self.last_valid)
      current most_recent_token

  let get_focus (production, _) =
    match lhs production with
    | X (N N_main) -> Some Main
    | X (N N_nestable_block_element) -> Some (Block None)
    | X (N N_tag) -> Some (Tag None)
    | X (N N_heading) -> Some Heading
    | _ -> None

  let update_focus self = function
    | Shifting (_before, after, _not_finished_parsing) -> (
        match stack after with
        | (lazy (Cons (Interpreter.Element (state, _, _, _), _))) ->
            List.find_map get_focus (items state)
            >>= (fun elt -> Some { self with current_symbol = Some elt })
            |> Option.value ~default:self
        | (lazy Nil) -> self)
    | _ -> self

  let context_string : 'a t -> string =
   fun self -> string_of_context self.current_symbol
end

let make_span :
    file:string ->
    start_pos:Lexing.position ->
    end_pos:Lexing.position ->
    Loc.span =
 fun ~file ~start_pos ~end_pos ->
  Loc.
    {
      file;
      start =
        {
          line = start_pos.pos_lnum;
          column = start_pos.pos_cnum - start_pos.pos_bol;
        };
      end_ =
        { line = end_pos.pos_lnum; column = end_pos.pos_cnum - end_pos.pos_bol };
    }

let dummy_table : Ast.table = (([ [] ], None), `Light)
let dummy_inline_element : Ast.inline_element = `Word ""

(* This needs to be a big match where we construct the AST node which will be
   inserted in place of the invalid element *)
let dummy_node (type a) (_context : a Context.t) = assert false

let how_many_times = ref 1

let contextual_error_message (type a b) ~(context : a Context.t)
    ~(checkpoint : a Interpreter.checkpoint)
    ~(continuation : a Interpreter.checkpoint -> Ast.t) =
  Printf.printf "We have traversed the error branch of the program %d times\n"
    !how_many_times;
  incr how_many_times;

  match checkpoint with
  | Interpreter.HandlingError env ->
      let offending_token = Context.peek context in
      (match offending_token with
      | Some ((Parser.Paragraph_style _ as token), start_pos, end_pos) ->
          context.push_warning
          @@ Parse_error.markup_should_not_be_used
               ~what:(Describe.describe token)
               (make_span ~file:context.file ~start_pos ~end_pos)
      | Some (Parser.END, start_pos, end_pos) ->
          context.push_warning
          @@ Parse_error.not_allowed
               ~what:(Describe.describe Parser.END)
               ~in_what:(Context.context_string context)
               (make_span ~file:context.file ~start_pos ~end_pos)
      | _ ->
          Printf.printf "UNHANDLED ERROR CASE!\nCONTEXT: %s\n%!"
            (Context.to_string context));
      let _, start_pos, end_pos = List.hd context.tokens in
      print_endline "Got position from context";
      let env =
        Interpreter.feed
          Interpreter.(N N_inline_element)
          start_pos dummy_inline_element end_pos env
      in

      (* NOTE: SEGFAULTS HERE *)
      let checkpoint = Interpreter.input_needed env in
      continuation checkpoint
  | _ -> unreachable "contextual_error_message"

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
    | Rejected ->
        (* What do we do here?? *)
        failwith "Input rejected"
    | HandlingError _ as checkpoint ->
        Printf.printf "IN ERROR ARM\nContext: %s\n%!"
          (Context.to_string context);
        handle_error ~context checkpoint
    | Accepted ast -> ast
  in
  let first_checkpoint = Incremental.main starting_location in
  let context =
    Context.create ~file:starting_location.Lexing.pos_fname ~push_warning
      ~first_checkpoint
  in
  run ~context first_checkpoint
