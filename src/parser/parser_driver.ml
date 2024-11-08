open Parser

module Engine = MenhirInterpreter
module Error = MenhirLib.ErrorReports

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n" s

let ( >>= ) = Option.bind
let ( >>| ) f o = Option.map o f
let ( let* ) = Option.bind

module With_position = struct
  type t = Parser.token * Lexing.position * Lexing.position

  let next_with_positon : Lexer.input -> Lexing.lexbuf -> t =
   fun input lexbuf ->
    let Loc.{ value; _ } = Lexer.token input lexbuf in
    (value, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

  let is : (Parser.token -> bool) -> t -> bool = fun f (x, _, _) -> f x
  let same : t -> Parser.token -> t =
   fun (_, start_pos, end_pos) other_token -> (other_token, start_pos, end_pos)
  let inner (x, _, _) = x
end

module Zipper = struct
  (* self.left is 'semantically backwards', naming should consistently reflect this *)
  type 'a t = {
    left : 'a list;
    focus : 'a;
    index : int;
    right : 'a list;
    length : int;
  }

  let move_left = function
    | { left = last :: rest; focus; index; right; _ } as self ->
        Some
          {
            self with
            right = focus :: right;
            index = pred index;
            focus = last;
            left = rest;
          }
    | _ -> None

  let move_right = function
    | { right = next :: rest; focus; index; left; _ } as self ->
        Some
          {
            self with
            left = focus :: left;
            focus = next;
            right = rest;
            index = succ index;
          }
    | _ -> None

  let peek_left = function { left = x :: _; _ } -> Some x | _ -> None
  let peek_right = function { right = x :: _; _ } -> Some x | _ -> None

  let nth self = function
    | n when n = self.index -> Some self.focus
    | n when n < 0 || n >= self.length -> None
    | index ->
        let rec go direction idx = function
          | x :: _ when idx = index -> Some x
          | _ :: xs when idx < self.length -> go direction (direction idx) xs
          | _ -> None
        in
        if index < self.index then go pred (pred self.index) self.left
        else go succ (succ self.index) self.right

  let rewind predicate self =
    let rec go self =
      if predicate self.focus then (
        Printf.printf "Zipper.rewind found target: %s\n"
          (Token.describe @@ With_position.inner self.focus);
        Some self)
      else move_left self >>= go
    in
    go self

  let insert_right : 'b -> 'a t -> 'a t =
   fun elt self ->
    { self with right = elt :: self.right; length = succ self.length }

  (* Does not modify index, only removes first element for which 'predicate'
     returns true *)
  let remove_in_place_right : ('a -> bool) -> 'a t -> 'a t =
   fun predicate self ->
    let rec remove acc = function
      | x :: xs when predicate x ->
          { self with right = List.rev acc @ xs; length = pred self.length }
      | x :: xs -> remove (x :: acc) xs
      | [] -> self
    in
    remove [] self.right

  let of_token_dispenser : (unit -> With_position.t) -> With_position.t t option
      =
   fun next_token ->
    let rec go focus acc =
      match next_token () with
      | (END, _, _) as token ->
          let right = List.rev (token :: acc) in
          let length = succ @@ List.length right in
          let print_token_list tokens =
            List.fold_left
              (fun s (token, _, _) -> s ^ "\n" ^ Token.describe token)
              "" tokens
          in
          Printf.printf "Tokens:\n%s\n%s\n%!"
            (Token.describe @@ With_position.inner focus)
            (print_token_list right);
          Printf.printf "Token list len is: %d" (succ @@ List.length right);
          Some { left = []; focus; index = 0; right; length }
      | token -> go focus (token :: acc)
    in
    match next_token () with END, _, _ -> None | first -> go first []

  let pp self =
    Printf.sprintf
      "Zipper { left: ( [ .. ] of len %d ), focus: %s, index: %d, right: ( [ \
       .. ] of len %d ) }"
      (List.length self.left)
      ((fun (t, _, _) -> Token.describe t) self.focus)
      (List.length self.right) self.index

  let to_list : 'a t -> 'a list =
   fun self -> List.rev self.left @ (self.focus :: self.right)
end

module Context = struct
  open Engine

  type snapshot = { state : Ast.t Engine.checkpoint; index : int }

  type t = {
    file : string;
    state_stack : snapshot list;
    push_warning : Warning.t -> unit;
    mutable tokens : With_position.t Zipper.t;
  }

  let create ~(file : string) ~(next_token : unit -> With_position.t)
      ~(push_warning : Warning.t -> unit) : t option =
    Zipper.of_token_dispenser next_token
    |> Option.map (fun tokens ->
           { file; push_warning; state_stack = []; tokens })

  let next_token : t -> With_position.t =
   fun self ->
    let temp = self.tokens.focus in
    Option.iter
      (fun next -> self.tokens <- next)
      (Zipper.move_right self.tokens);
    temp

  let pp_checkpoint = function
    | InputNeeded _ -> "Input needed (Parser needs token)"
    | AboutToReduce _ ->
        "About to reduce (replacing token sequence with symbol)"
    | Shifting _ -> "Shifting (accepting token into stack)"
    | Accepted _ -> "Accepted (successfully parsed input)"
    | HandlingError _ -> "Handling error"
    | Rejected -> "Rejected"

  let tokens : t -> With_position.t list =
   fun self -> Zipper.to_list self.tokens

  let print_states states =
    let as_str =
      List.map
        (fun ({ state; index }, token) ->
          Printf.sprintf "{\n\tstate : %s\n\tindex: %d\n\ttoken: %s\n}\n"
            (pp_checkpoint state) index token)
        states
      |> List.fold_left ( ^ ) ""
    in
    "[\n" ^ as_str ^ "]\n"

  let to_string : t -> string =
   fun self ->
    let most_recent_token =
      let t, _, _ = self.tokens.focus in
      Token.describe t
    in
    let states_with_tokens =
      List.map
        (fun ({ index; _ } as s) ->
          ( s,
            Zipper.nth self.tokens index
            |> Option.map (fun (t, _, _) -> Token.describe t)
            |> Option.value ~default:"NO TOKEN" ))
        self.state_stack
    in
    Printf.sprintf "{ checkpoints: %s; most_recent_token: %s }\n"
      (print_states states_with_tokens)
      most_recent_token

  let is_opening_token : Parser.token -> bool = function
    | Style _ | List _ | List_item _ | TABLE_LIGHT | TABLE_HEAVY | TABLE_ROW ->
        true
    | _ -> false

  let filter_opening_token :
      type a. a checkpoint -> Parser.token -> a checkpoint option =
   fun checkpoint token ->
    if is_opening_token token then Some checkpoint else None

  let is_some_and f = function Some x -> f x | _ -> false
  let update : t -> Ast.t checkpoint -> t =
   fun self checkpoint ->
    match
      Zipper.peek_left self.tokens
      >>| With_position.inner
      >>= filter_opening_token checkpoint
    with
    | Some checkpoint ->
        {
          self with
          state_stack =
            { state = checkpoint; index = pred self.tokens.index }
            :: self.state_stack;
        }
    | None -> self

  let get_checkpoint : t -> Ast.t Engine.checkpoint option =
   fun self ->
    List.find_map
      (function
        | { state; index } when index = self.tokens.index ->
            Printf.printf "Got checkpoint: %s\n%!" (pp_checkpoint state);
            Some state
        | _ -> None)
      self.state_stack
end

module Action = struct
  type t = Insert of substitution | Remove of substitution
  and substitution = { solution : Parser.token; parent : Parser.token }

  let pp = function
    | Insert { solution; parent } ->
        Printf.sprintf "Insert this: %s\nafter this: %s"
          (Token.describe solution) (Token.describe parent)
    | Remove { solution; parent } ->
        Printf.sprintf "Remove the first: %s\nafter this: %s"
          (Token.describe solution) (Token.describe parent)

  let ( let* ) = Option.bind

  let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false
  let to_digit c =
    if is_digit c then
      let digit = int_of_string (String.make 1 c) in
      Some digit
    else None

  let parse : string -> t option =
   fun metadata ->
    match String.split_on_char ' ' metadata with
    | "Insert" :: rest | "Remove" :: rest -> (
        let dummy_input =
          Lexer.
            {
              file = "";
              offset_to_location = Fun.const Loc.{ line = 0; column = 0 };
              warnings = [];
            }
        in
        let lexbuf = Lexing.from_string (List.fold_left String.cat "" rest) in
        try
          let Loc.{ value = solution; _ } = Lexer.token dummy_input lexbuf in
          let Loc.{ value = parent; _ } = Lexer.token dummy_input lexbuf in
          match dummy_input.warnings with
          | [] -> Option.some @@ Insert { solution; parent }
          | _ -> None
        with _ -> None)
    | _ -> None

  let element = function Insert t | Remove t -> t.parent

  let split : string -> string * string =
   fun input ->
    let chars = String.to_seq input |> List.of_seq in
    let rec go acc = function
      | [] -> assert false
      | ']' :: rest -> (List.rev acc, rest)
      | '[' :: rest -> go acc rest
      | c :: rest -> go (c :: acc) rest
    in
    let metadata, message = go [] chars in
    let to_string = List.fold_left (fun acc c -> acc ^ String.make 1 c) "" in
    (to_string metadata, to_string message)

  let eval : Context.t -> t -> (Ast.t Engine.checkpoint * Context.t) option =
   fun context self ->
    match self with
    | Insert { solution; parent } ->
        print_endline "Action.eval.Insert";
        let* zipper =
          Zipper.rewind (With_position.is (Token.same parent)) context.tokens
        in
        let zipper =
          Zipper.insert_right (With_position.same zipper.focus solution) zipper
        in
        Printf.printf "Inserted token into Zipper: %s\n%!" (Zipper.pp zipper);
        context.tokens <- zipper;
        let* checkpoint = Context.get_checkpoint context in
        Some (checkpoint, context)
    | Remove { solution; parent } ->
        print_endline "Action.eval.Remove";
        let* zipper =
          Zipper.rewind (With_position.is @@ Token.same parent) context.tokens
          >>| Zipper.remove_in_place_right
                (With_position.is @@ Token.same solution)
        in
        context.tokens <- zipper;
        let* checkpoint = Context.get_checkpoint context in
        Some (checkpoint, context)

  let get_warning : string -> Parse_error.parser_error option =
   fun tag ->
    let open Parse_error in
    let words = String.trim tag |> String.split_on_char ' ' in
    match words with
    | "Should_not_be_empty" :: _ -> Some Should_not_be_empty
    | "End_not_allowed" :: _ -> Some End_not_allowed
    | "Not_allowed" :: _token_string :: _ ->
        failwith
          "Parser_driver.Action.get_warning :: Not_allowed branch not \
           implemented"
    | "Should_be_followed_by_whitespace" :: _ ->
        Some Should_be_followed_by_whitespace
    | "Should_begin_line" :: _ -> Some Should_begin_line
    | "No_markup" :: _ -> Some No_markup
    | "Unpaired_right_brace" :: _ -> Some Unpaired_right_brace
    | _ -> None
end

let env : type a. a Engine.checkpoint -> a Engine.env = function
  | Engine.HandlingError env -> env
  | _ -> unreachable "Parser_driver.env"

let make_span : string -> Lexing.position -> Lexing.position -> Loc.span =
 fun file start_pos end_pos ->
  let open Loc in
  let open Lexing in
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

let fill_holes : type a. input_text:string -> a Engine.env -> int -> string =
 fun ~input_text env i ->
  match Engine.get i env with
  | Some (Engine.Element (_, _, start_pos, end_pos)) ->
      Error.extract input_text (start_pos, end_pos)
  | None -> "" (* Parser is in initial state, which shouldn't be possible *)

let log : tag:string -> string -> string =
 fun ~tag s ->
  Printf.printf "%s: %s%!" tag s;
  s

(* TODO: So this is failing because the functions in MehirLib's Error module
   seem to sanitize, specifically lowercase and remove underscores from, error
   messages. This breaks our metadata system. We will have to implement our own
   versions of these functions *)
let error_message :
    type a.
    input_text:string ->
    Context.t ->
    a Engine.env ->
    (Action.t * Warning.t) option =
 fun ~input_text context env ->
  print_endline "IN Main.error_message";
  match Engine.stack env with
  | (lazy (Cons (Engine.Element (state, _ast_node, start_pos, end_pos), _)))
    -> (
      let state_id = Engine.number state in
      Printf.printf "Error state number: %d\n\n%!" state_id;
      let menhir_message =
        try Parser_messages.message state_id
        with Not_found ->
          print_endline "<<MATCHING ERROR MESSAGE NOT FOUND>>";
          raise Not_found
      in
      try
        let metadata, message =
          menhir_message
          |> log ~tag:"Original message"
          |> Error.expand (fill_holes ~input_text env)
          |> Action.split
        in
        match Action.get_warning message with
        | Some warning_kind ->
            let action = Action.parse metadata |> Option.get in
            let element = Action.element action |> Token.describe in
            let span = make_span context.file start_pos end_pos in
            let warning = Parse_error.make_warning ~element span warning_kind in
            Some (action, warning)
        | None ->
            failwith "Parser_driver.error_message :: Action.get_warning is none"
      with exn ->
        print_endline "CANNOT RECOVER";
        raise exn)
  | (lazy Nil) -> None

let run_parser :
    input_text:string ->
    starting_location:Lexing.position ->
    lexbuf:Lexing.lexbuf ->
    input:Lexer.input ->
    push_warning:(Warning.t -> unit) ->
    Ast.t =
 fun ~input_text ~starting_location ~lexbuf ~input ~push_warning ->
  let open Engine in
  let rec handle_error : Context.t -> Ast.t Engine.env -> Ast.t =
    print_endline "run_parser.handle_error";
    fun context env ->
      let eval context action =
        Printf.printf "run_parser.handle_error.eval:\ncontext: %s\n%!"
          (Context.to_string context);
        match Action.eval context action with
        | Some (checkpoint, context) -> run context (Engine.resume checkpoint)
        | None -> raise (Failure "Main.run_parser.handle_error.eval")
      in
      match Printexc.print (error_message ~input_text context) env with
      | Some (action, warning) ->
          input.warnings <- warning :: input.warnings;
          eval context action
      | None ->
          (* here we would want to 'wordify' some portion, or potentially all
             of, the tokens. Transforming anything that isn't whitespace into
             a 'Word' *)
          print_endline "FATAL: NO MATCHING ERROR MESSAGE";
          raise (Failure "Main.run_parser.handle_error")
  and run : Context.t -> Ast.t Engine.checkpoint -> Ast.t =
    let n_runs = ref 0 in
    fun context checkpoint ->
      match checkpoint with
      | InputNeeded _ as checkpoint ->
          let ((t, _, _) as token) = Context.next_token context in
          Printf.printf "Got token: %s\n%!" (Token.print t);
          run context (Engine.offer checkpoint token)
      | Shifting _ as checkpoint ->
          Printf.printf "Tokens: %s\n" (Zipper.pp context.tokens);
          let context = Context.update context checkpoint in
          run context (Engine.resume checkpoint)
      | AboutToReduce _ as checkpoint -> run context (resume checkpoint)
      | HandlingError env ->
          incr n_runs;
          if !n_runs > 1 then failwith "TESTING" else handle_error context env
      | Accepted ast -> ast
      | Rejected ->
          (* Not sure what can be done here - generally
             only a consequence of repeated HandlingError states*)
          assert false
  in
  let first_checkpoint = Incremental.main starting_location in
  let file = starting_location.Lexing.pos_fname in
  let next_token () = With_position.next_with_positon input lexbuf in
  match Context.create ~file ~next_token ~push_warning with
  | Some context -> run context first_checkpoint
  | None ->
      (* This only happens when the token stream is empty (only contains 'END'
         token). Because errors are not possible without input, we can simply
         return the empty AST *)
      []
