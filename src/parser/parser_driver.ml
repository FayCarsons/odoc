open Parser

module Engine = MenhirInterpreter
module Error = MenhirLib.ErrorReports

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n" s

module With_position = struct
  type t = Parser.token * Lexing.position * Lexing.position

  let next_with_positon : Lexer.input -> Lexing.lexbuf -> t =
   fun input lexbuf ->
    let Loc.{ value; _ } = Lexer.token input lexbuf in
    (value, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

  let same (_, start_pos, end_pos) other_token =
    (other_token, start_pos, end_pos)
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

  let move_left self =
    match self.left with
    | last :: rest ->
        Some
          {
            right = self.focus :: self.right;
            index = pred self.index;
            focus = last;
            left = rest;
            length = self.length;
          }
    | [] -> None

  let move_right self =
    match self.right with
    | x :: xs ->
        Some
          {
            left = self.focus :: self.left;
            focus = x;
            index = succ self.index;
            right = xs;
            length = self.length;
          }
    | [] -> None

  let nth self = function
    | n when n = self.index -> Some self.focus
    | n when n < 0 || n >= self.length -> None
    | index ->
        let rec go direction idx = function
          | x :: _ when idx = index -> Some x
          | _ :: xs -> go direction (direction idx) xs
          | [] -> None
        in
        if index < self.index then go pred (pred self.index) self.left
        else go succ (succ self.index) self.right

  let rewind self predicate =
    let rec go self =
      if predicate self.focus then Some self
      else Option.bind (move_left self) go
    in
    go self

  let insert_right : 'a t -> 'a -> 'a t =
   fun self elt ->
    { self with right = elt :: self.right; length = succ self.length }

  (* Does not modify index, only removes first element for which 'predicate'
     returns true *)
  let remove_map_right : 'a t -> ('a -> bool) -> 'a t =
   fun self predicate ->
    let rec remove acc = function
      | x :: xs when predicate x -> { self with right = List.rev acc @ xs }
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
          let length = succ (List.length right) in
          Some { left = []; focus; index = 0; right; length }
      | token -> go focus (token :: acc)
    in
    match next_token () with END, _, _ -> None | first -> go first []

  let to_list : 'a t -> 'a list =
   fun self -> List.rev (self.focus :: self.left) @ self.right
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
      ~(push_warning : Warning.t -> unit) ~(first_checkpoint : Ast.t checkpoint)
      : t option =
    Zipper.of_token_dispenser next_token
    |> Option.map (fun tokens ->
           {
             file;
             push_warning;
             state_stack = [ { state = first_checkpoint; index = 0 } ];
             tokens;
           })

  let next_token : t -> With_position.t option =
   fun self ->
    Zipper.move_right self.tokens
    |> Option.map (fun next ->
           self.tokens <- next;
           next.focus)

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

  let filter_opening_token :
      type a. a checkpoint -> Parser.token -> a checkpoint option =
   fun checkpoint token ->
    match token with
    | Style _ | List _ | List_item _ | TABLE_LIGHT | TABLE_HEAVY | TABLE_ROW ->
        Some checkpoint
    | _ -> None

  let update : t -> Ast.t checkpoint -> With_position.t -> t =
   fun self checkpoint (token, _, _) ->
    match filter_opening_token checkpoint token with
    | Some checkpoint ->
        {
          self with
          state_stack =
            { state = checkpoint; index = self.tokens.index }
            :: self.state_stack;
        }
    | None -> self

  let get_checkpoint : t -> int -> Ast.t Engine.checkpoint option =
   fun self index ->
    List.find_map
      (fun { state; index = index' } ->
        if index = index' then Some state else None)
      self.state_stack
end

module Action = struct
  type t = Insert of substitution | Remove of substitution
  and substitution = { this : Parser.token; after_this : Parser.token }

  let pp = function
    | Insert { this; after_this } ->
        Printf.sprintf "Insert this: %s\nafter this: %s" (Token.describe this)
          (Token.describe after_this)
    | Remove { this; after_this } ->
        Printf.sprintf "Remove the first: %s\nafter this: %s"
          (Token.describe this)
          (Token.describe after_this)

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
          let Loc.{ value = this; _ } = Lexer.token dummy_input lexbuf in
          let Loc.{ value = after_this; _ } = Lexer.token dummy_input lexbuf in
          match dummy_input.warnings with
          | [] -> Option.some @@ Insert { this; after_this }
          | _ -> None
        with _ -> None)
    | _ -> None

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
    | Insert { this; after_this } -> (
        match
          Zipper.rewind context.tokens (fun (t, _, _) ->
              Token.same t after_this)
        with
        | Some zipper ->
            context.tokens <-
              Zipper.insert_right zipper (With_position.same zipper.focus this);
            Context.get_checkpoint context zipper.index
            |> Option.map (fun checkpoint -> (checkpoint, context))
        | None -> None)
    | Remove { this; after_this } -> (
        match
          Zipper.rewind context.tokens (fun (t, _, _) ->
              Token.same t after_this)
        with
        | Some zipper ->
            context.tokens <-
              Zipper.remove_map_right zipper (fun (t, _, _) ->
                  Token.same t this);
            Context.get_checkpoint context zipper.index
            |> Option.map (fun checkpoint -> (checkpoint, context))
        | None -> None)
end

let env : type a. a Engine.checkpoint -> a Engine.env = function
  | Engine.HandlingError env -> env
  | _ -> unreachable "Parser_driver.env"

let fill_holes : type a. input_text:string -> a Engine.env -> int -> string =
 fun ~input_text env i ->
  match Engine.get i env with
  | Some (Engine.Element (_, _, start_pos, end_pos)) ->
      Error.extract input_text (start_pos, end_pos)
      |> Error.sanitize |> Error.compress
  | None -> "" (* Parser is in initial state, which shouldn't be possible *)

let error_message :
    type a. input_text:string -> a Engine.env -> (Action.t * string) option =
 fun ~input_text env ->
  print_endline "IN Main.error_message";
  match Engine.stack env with
  | (lazy (Cons (Engine.Element (state, _, _, _), _))) -> (
      let state_id = Engine.number state in
      Printf.printf "Error state number: %d\n\n%!" state_id;
      try
        let metadata, message =
          Parser_messages.message state_id
          |> Error.expand (fill_holes ~input_text env)
          |> Action.split
        in
        let action = Action.parse metadata |> Option.get in
        Some (action, message)
      with Not_found -> None)
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
  let rec handle_error (type a) ~context = function
    | HandlingError env as _checkpoint -> (
        let eval context action =
          match Action.eval context action with
          | Some (checkpoint, context) ->
              run ~context (Engine.resume checkpoint)
          | None -> raise (Invalid_argument "Main.run_parser.handle_error")
        in
        match error_message ~input_text env with
        | Some (action, _error_message) -> eval context action
        | None ->
            (* here we would want to 'wordify' some portion, or potentially all
               of, the tokens. Transforming anything that isn't whitespace into
               a 'Word' *)
            print_endline "NO MATCHING ERROR MESSAGE";
            raise (Invalid_argument "Main.run_parser.handle_error"))
    | _ -> unreachable "Parser_driver.run_parser.handle_error"
  and run ~context = function
    | InputNeeded _ as checkpoint -> (
        match Context.next_token context with
        | Some ((t, _, _) as token) ->
            Printf.printf "Got token: %s\n%!" (Token.print t);
            run ~context (Engine.offer checkpoint token)
        | None -> raise (Invalid_argument "Main.run_parser.run/input_needed"))
    | Shifting _ as checkpoint ->
        let context = Context.update context checkpoint context.tokens.focus in
        run ~context (Engine.resume checkpoint)
    | AboutToReduce _ as checkpoint -> run ~context (resume checkpoint)
    | Rejected ->
        (* What do we do here?? *)
        assert false
    | HandlingError _ as checkpoint -> handle_error ~context checkpoint
    | Accepted ast -> ast
  in
  let first_checkpoint = Incremental.main starting_location in
  let file = starting_location.Lexing.pos_fname in
  let next_token () = With_position.next_with_positon input lexbuf in
  match Context.create ~file ~next_token ~push_warning ~first_checkpoint with
  | Some context -> run ~context first_checkpoint
  | None ->
      (* This only happens when the token stream is empty (only contains 'END'
         token). Because errors are not possible without input, we can simply
         return the empty AST *)
      []
