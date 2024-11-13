open Parser

module Engine = MenhirInterpreter
module Error = MenhirLib.ErrorReports

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n" s

let ( >>= ) = Option.bind
let ( >>| ) o f = Option.map f o
let ( let* ) = Option.bind

module With_position = struct
  type t = Parser.token * Lexing.position * Lexing.position

  let make_span :
      filename:string -> Lexing.position -> Lexing.position -> Loc.span =
   fun ~filename:file start_pos end_pos ->
    let to_point pos =
      let open Lexing in
      Loc.{ line = pos.pos_lnum; column = pos.pos_cnum - pos.pos_bol }
    in
    Loc.{ file; start = to_point start_pos; end_ = to_point end_pos }

  let next_with_positon : Lexer.input -> Lexing.lexbuf -> t =
   fun input lexbuf ->
    let Loc.{ value; _ } = Lexer.token input lexbuf in
    (value, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

  let inner (t, _, _) = t
  let same (_, start_pos, end_pos) other_token =
    (other_token, start_pos, end_pos)

  let is f (t, _, _) = f t
end

module Token_stream = struct
  type 'a t = { inner : 'a array; position : int }

  let length { inner; _ } = Array.length inner
  let position { position; _ } = position

  let is_valid_position pos self = pos >= 0 && length self > pos
  let at_end self = self.position = pred (length self)

  let current : 'a t -> 'a option =
   fun self ->
    if is_valid_position self.position self then Some self.inner.(self.position)
    else None

  let advance self =
    if is_valid_position self.position self then
      Some { self with position = succ self.position }
    else None

  let peek_next self =
    let next_pos = succ self.position in
    if is_valid_position next_pos self then Some self.inner.(next_pos) else None

  let next self =
    let* token = current self in
    let* next_self = advance self in
    Some (token, next_self)

  (* Does not modify internal state *)
  let peek_at : int -> 'a t -> 'a option =
   fun pos self ->
    if is_valid_position pos self then Some self.inner.(pos) else None

  let peek_back : int -> 'a t -> 'a option =
   fun pos self -> peek_at (self.position - pos) self

  let rewind : 'a t -> ('a -> bool) -> 'a t option =
   fun self predicate ->
    let rec go self =
      if
        is_valid_position self.position self
        && predicate self.inner.(self.position)
      then Some self
      else go { self with position = pred self.position }
    in
    go self

  let insert_right : 'a t -> 'a -> 'a t =
   fun self elt ->
    let insertion_point = succ self.position in
    let left = Array.sub self.inner 0 insertion_point in
    let right =
      Array.sub self.inner insertion_point (length self - insertion_point)
    in
    let inner = Array.concat [ left; Array.of_list [ elt ]; right ] in
    { self with inner }

  (* Does not modify position, only removes first element for which 'predicate'
     returns true *)
  let filter_right : ('a -> bool) -> 'a t -> 'a t option =
   fun predicate self ->
    let rec go = function
      | n when is_valid_position n self && predicate self.inner.(n) ->
          let left = Array.sub self.inner 0 n
          and right = Array.sub self.inner (succ n) (length self - succ n) in
          Some { self with inner = Array.concat [ left; right ] }
      | n when n >= length self -> None
      | n -> go (succ n)
    in
    go self.position

  let map_inplace f self = Array.map_inplace f self.inner

  let print_tokens arr =
    Array.fold_left (fun acc (t, _, _) -> acc ^ "\n" ^ Token.describe t) "" arr

  let of_generator : (unit -> With_position.t) -> With_position.t t =
   fun next_token ->
    let rec fill acc =
      match next_token () with
      | (END, _, _) as token -> List.rev (token :: acc)
      | token -> fill (token :: acc)
    in
    let inner = Array.of_list (fill []) in
    Printf.printf "INITIAL TOKENS: %s\nLEN: %d\n%!" (print_tokens inner)
      (Array.length inner);
    { inner; position = 0 }
end

module Context = struct
  open Engine

  type snapshot = { state : Ast.t Engine.checkpoint; index : int }

  type t = {
    file : string;
    state_stack : snapshot list;
    push_warning : Warning.t -> unit;
    mutable tokens : With_position.t Token_stream.t;
  }

  let create ~(file : string) ~(next_token : unit -> With_position.t)
      ~(push_warning : Warning.t -> unit) : t =
    let tokens = Token_stream.of_generator next_token in
    { file; state_stack = []; push_warning; tokens }

  let next_token : t -> With_position.t option =
   fun self ->
    match Token_stream.next self.tokens with
    | Some (token, tokens) ->
        self.tokens <- tokens;
        Some token
    | None -> None

  let pp_checkpoint = function
    | InputNeeded _ -> "Input needed (Parser needs token)"
    | AboutToReduce _ ->
        "About to reduce (replacing token sequence with symbol)"
    | Shifting _ -> "Shifting (accepting token into stack)"
    | Accepted _ -> "Accepted (successfully parsed input)"
    | HandlingError _ -> "Handling error"
    | Rejected -> "Rejected"

  let tokens : t -> With_position.t list =
   fun self -> Array.to_list self.tokens.inner

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
      Token_stream.current self.tokens
      >>| With_position.inner >>| Token.describe
      |> Option.value ~default:"End of stream"
    in
    let states_with_tokens =
      List.map
        (fun ({ index; _ } as s) ->
          ( s,
            Token_stream.peek_at index self.tokens
            |> Option.map (fun (t, _, _) -> Token.describe t)
            |> Option.value ~default:"NO TOKEN" ))
        self.state_stack
    in
    Printf.sprintf "{ checkpoints: %s; most_recent_token: %s }\n"
      (print_states states_with_tokens)
      most_recent_token

  let stringify_all self =
    Token_stream.map_inplace
      (fun (t, s, e) ->
        match t with
        | (END | Word _) as t -> (t, s, e)
        | t -> (Word (Token.print t), s, e))
      self.tokens;
    { self with tokens = { self.tokens with position = 0 }; state_stack = [] }

  let is_opening_token : Parser.token -> bool = function
    | Style _ | List _ | List_item _ | TABLE_LIGHT | TABLE_HEAVY | TABLE_ROW ->
        true
    | _ -> false

  let update : t -> Ast.t checkpoint -> t =
   fun self checkpoint ->
    match Token_stream.current self.tokens with
    | Some (t, _, _) when is_opening_token t ->
        let self =
          {
            self with
            state_stack =
              { state = checkpoint; index = self.tokens.position }
              :: self.state_stack;
          }
        in
        print_endline
        @@ print_states
             (List.map
                (fun s ->
                  let token_descr =
                    Token.describe
                    @@ With_position.inner self.tokens.inner.(s.index)
                  in
                  (s, token_descr))
                self.state_stack);
        self
    | _ -> self

  let nth_checkpoint : t -> int -> Ast.t Engine.checkpoint option =
   fun self index ->
    List.find_map
      (fun { state; index = index' } ->
        if index = index' then Some state else None)
      self.state_stack
end

module Action = struct
  type t = Insert of substitution | Remove of substitution
  and substitution = { this : Parser.token; after_this : Parser.token }

  let element = function
    | Insert { after_this; _ } | Remove { after_this; _ } -> after_this

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
    | Insert { this; after_this } ->
        let tokens =
          match
            Token_stream.rewind context.tokens
              (With_position.is (Token.same after_this))
          with
          | Some x -> x
          | None -> failwith "Cannot find element starting token"
        in
        Printf.printf "Rewound to token: %s, index: %d\n%!"
          (Token_stream.current tokens
          >>| With_position.inner >>| Token.describe
          |> Option.value ~default:"End of token stream")
          tokens.position;

        let wrapped_token =
          (* Option.get is safe here because we've already rewound to a valid
             index *)
          With_position.same (Token_stream.current tokens |> Option.get) this
        in
        let tokens = Token_stream.insert_right tokens wrapped_token in
        context.tokens <- tokens;
        let last_valid_checkpoint =
          match Context.nth_checkpoint context tokens.position with
          | Some x -> x
          | None -> failwith "Cannot find matching context "
        in
        Some (last_valid_checkpoint, context)
    | Remove { this; after_this } -> (
        let tokens =
          match
            Token_stream.rewind context.tokens
              (With_position.is (Token.same after_this))
          with
          | Some x -> (
              match
                Token_stream.filter_right (With_position.is (Token.same this)) x
              with
              | Some x -> x
              | None -> failwith "Cannot find token which needs to be removed")
          | None -> failwith "Cannot find token which starts element"
        in

        context.tokens <- tokens;
        match Context.nth_checkpoint context tokens.position with
        | Some x -> Some (x, context)
        | None -> failwith "Cannot find matching checkpoint")
end

let env : type a. a Engine.checkpoint -> a Engine.env = function
  | Engine.HandlingError env -> env
  | _ -> unreachable "Parser_driver.env"

let fill_holes :
    input_text:string -> Context.t -> Ast.t Engine.env -> int -> string =
 fun ~input_text context env i ->
  match Engine.get i env with
  | Some (Engine.Element (_, _, start_pos, end_pos)) ->
      let message =
        Error.extract input_text (start_pos, end_pos)
        |> Error.sanitize |> Error.compress
      in
      message
  | None -> (
      match i with
      | 0 ->
          Token_stream.current context.tokens
          >>| With_position.inner >>| Token.describe
          |> Option.value ~default:"Unknown"
      | n ->
          Token_stream.peek_back n context.tokens
          >>| With_position.inner >>| Token.describe
          |> Option.value ~default:"Unkown")

let logs : tag:string -> string -> string =
 fun ~tag s ->
  Printf.printf "%s: %s\n" tag s;
  s

let get_recovery_strategy :
    Context.t -> string -> Ast.t Engine.env -> (Action.t * Warning.t) option =
 fun context input_text env ->
  match Engine.stack env with
  | (lazy (Cons (Engine.Element (state, _, start_pos, end_pos), _))) -> (
      let state_id = Engine.number state in
      Printf.printf "Error state number: %d\n\n%!" state_id;
      try
        let recovery_instruction, error_case =
          Parser_messages.message state_id
          |> logs ~tag:"Raw error message"
          |> Error.expand (fill_holes ~input_text context env)
          |> logs ~tag:"With replacements"
          |> Action.split
        in
        let* recovery_instruction = Action.parse recovery_instruction in
        let warning =
          String.trim error_case |> Parse_error.of_string
          |> Parse_error.make_warning
               ~element:(Token.describe @@ Action.element recovery_instruction)
               (With_position.make_span ~filename:context.file start_pos end_pos)
        in
        Some (recovery_instruction, warning)
      with Not_found -> None)
  | (lazy Nil) ->
      (* We can't recover *)
      None

let eoi = ref false

let run_parser :
    input_text:string ->
    starting_location:Lexing.position ->
    lexbuf:Lexing.lexbuf ->
    input:Lexer.input ->
    push_warning:(Warning.t -> unit) ->
    Ast.t =
 fun ~input_text ~starting_location ~lexbuf ~input ~push_warning ->
  let open Engine in
  let resume = Engine.resume ~strategy:`Simplified in
  let rec handle_error context = function
    | HandlingError env -> (
        match get_recovery_strategy context input_text env with
        | Some (action, warning) -> (
            input.warnings <- warning :: input.warnings;
            match context.state_stack with
            | _ :: _ -> (
                match Action.eval context action with
                | Some (checkpoint, context) -> run context (resume checkpoint)
                | None -> failwith "Handle_error.eval")
            | [] -> fail_and_restart context)
        | None -> failwith "NO RECOVERY STRATEGY")
    | _ -> unreachable "Parser_driver.run_parser.handle_error"
  and fail_and_restart context =
    let context = Context.stringify_all context in
    Printf.printf "FAILED AND RESTARTED\nCOntext:\n%s\n%!"
      (Context.to_string context);
    let new_checkpoint = Incremental.main starting_location in
    run context new_checkpoint
  and run context = function
    | InputNeeded _ as checkpoint -> (
        print_endline "InputNeeded";
        match Context.next_token context with
        | Some token ->
            Printf.printf "Got token: %s, token_stream index: %d\n%!"
              (Token.describe @@ With_position.inner token)
              context.tokens.position;
            run context (Engine.offer checkpoint token)
        | None -> failwith "No tokens left")
    | Shifting _ as checkpoint ->
        print_endline "Shifting";
        let context = Context.update context checkpoint in
        run context (resume checkpoint)
    | AboutToReduce _ as checkpoint ->
        print_endline "AboutToReduce";
        run context (resume checkpoint)
    | Rejected ->
        print_endline "Rejected";
        (* What do we do here?? *)
        assert false
    | HandlingError _ as checkpoint ->
        print_endline "in HandlingError branch";
        handle_error context checkpoint
    | Accepted ast -> ast
  in
  print_endline "Starting run_parser";
  let first_checkpoint = Incremental.main starting_location in
  let file = starting_location.Lexing.pos_fname in
  let next_token () = With_position.next_with_positon input lexbuf in
  let context = Context.create ~file ~next_token ~push_warning in
  Printexc.print (run context) first_checkpoint
