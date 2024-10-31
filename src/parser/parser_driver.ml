open Parser

module Engine = MenhirInterpreter
module Error = MenhirLib.ErrorReports

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n%!" s
let unhandled s = failwith @@ Printf.sprintf "UNHANDLED BRANCH IN %s\n%!" s

module Context : sig
  type with_position = Parser.token * Lexing.position * Lexing.position
  type snapshot = { state : Ast.t Engine.checkpoint; index : int }

  type t = {
    file : string;
    state_stack : snapshot list;
    push_warning : Warning.t -> unit;
    tokens : with_position list;
    token_count : int;
  }

  val create :
    file:string ->
    push_warning:(Warning.t -> unit) ->
    first_checkpoint:Ast.t Engine.checkpoint ->
    t

  val push_token : t -> with_position -> t
  val peek : t -> with_position option
  val pop : t -> with_position option * t
  val tokens : t -> with_position list
  val update : t -> Ast.t Engine.checkpoint -> Parser.token -> t
  val to_string : t -> string
end = struct
  open Engine

  type with_position = Parser.token * Lexing.position * Lexing.position
  type snapshot = { state : Ast.t Engine.checkpoint; index : int }

  type t = {
    file : string;
    state_stack : snapshot list;
    push_warning : Warning.t -> unit;
    tokens : with_position list;
    token_count : int;
  }

  let create ~(file : string) ~(push_warning : Warning.t -> unit)
      ~(first_checkpoint : Ast.t checkpoint) =
    {
      file;
      push_warning;
      state_stack = [ { state = first_checkpoint; index = 0 } ];
      tokens = [];
      token_count = 0;
    }

  let pp_checkpoint = function
    | InputNeeded _ -> "Input needed (Parser needs token)"
    | AboutToReduce _ ->
        "About to reduce (replacing token sequence with symbol)"
    | Shifting _ -> "Shifting (accepting token into stack)"
    | Accepted _ -> "Accepted (successfully parsed input)"
    | HandlingError _ -> "Handling error"
    | Rejected -> "Rejected"

  let push_token : t -> with_position -> t =
   fun self token -> { self with tokens = token :: self.tokens }

  let peek : t -> with_position option = function
    | { tokens = x :: _; _ } -> Some x
    | _ -> None

  let pop : t -> with_position option * t = function
    | { tokens = x :: xs; _ } as self -> (Some x, { self with tokens = xs })
    | self -> (None, self)

  let tokens : t -> with_position list = fun self -> self.tokens

  let print_states state_stack =
    let as_str =
      List.map
        (fun { state; index } ->
          Printf.sprintf "{\nstate : %s\nindex: %d\n}\n" (pp_checkpoint state)
            index)
        state_stack
      |> List.fold_left ( ^ ) ""
    in
    "[\n" ^ as_str ^ "]\n"

  let to_string : t -> string =
   fun self ->
    let most_recent_token =
      Option.map (fun (t, _, _) -> Token.describe t) (peek self)
      |> Option.value ~default:"No tokens"
    in
    Printf.sprintf "{ checkpoint: %s; most_recent_token: %s }\n%!"
      (print_states self.state_stack)
      most_recent_token

  let filter_opening_token :
      type a. a checkpoint -> Parser.token -> a checkpoint option =
   fun checkpoint token ->
    match token with
    | Style _ | List _ | List_item _ | TABLE_LIGHT | TABLE_HEAVY | TABLE_ROW ->
        Some checkpoint
    | _ -> None

  let update : t -> Ast.t checkpoint -> Parser.token -> t =
   fun self checkpoint token ->
    match filter_opening_token checkpoint token with
    | Some checkpoint ->
        {
          self with
          state_stack =
            { state = checkpoint; index = self.token_count } :: self.state_stack;
          token_count = succ self.token_count;
        }
    | None -> self
end

module Action = struct
  type kind = Insert of int * Parser.token | Remove of int * Parser.token

  let of_string : string -> t 
    
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

let env : type a. a Engine.checkpoint -> a Engine.env = function
  | Engine.HandlingError env -> env
  | _ -> assert false (* unreachable *)

let fill_holes : type a. input_text:string -> a Engine.env -> int -> string =
 fun ~input_text env i ->
  match Engine.get i env with
  | Some (Engine.Element (_, _, start_pos, end_pos)) ->
      Error.extract input_text (start_pos, end_pos)
      |> Error.sanitize |> Error.compress
  | None -> "" (* Parser is in initial state, which shouldn't be possible *)

let error_message : type a. input_text:string -> a Engine.env -> string option =
 fun ~input_text env ->
  match Engine.stack env with
  | (lazy (Cons (Engine.Element (state, _, _, _), _))) -> (
      let state_id = Engine.number state in
      Printf.printf "State: %d\n%!" state_id;
      try
        let message = Parser_messages.message state_id in
        Option.some @@ Error.expand (fill_holes ~input_text env) message
      with Not_found -> None)
  | (lazy Nil) -> None

let run_parser :
    input_text:string ->
    starting_location:Lexing.position ->
    next_token:(unit -> Context.with_position) ->
    push_warning:(Warning.t -> unit) ->
    Ast.t =
 fun ~input_text ~starting_location ~next_token ~push_warning ->
  let open Engine in
  let rec handle_error ~context checkpoint : Ast.t =
    let continue () = run ~context (Engine.resume checkpoint) in
    let[@warning "-8"] (HandlingError env) = checkpoint in
    match error_message ~input_text env with
    | Some msg ->
        Printf.printf "Failed!\nMessage:\n%s\nContext:\n%s\n%!" msg
          (Context.to_string context);
        continue ()
    | None ->
        print_endline "Cannot find error message";
        continue ()
  and run ~context = function
    | InputNeeded _env as checkpoint ->
        let token = next_token () in
        let checkpoint = Engine.offer checkpoint token
        and context = Context.push_token context token in
        run ~context checkpoint
    | Shifting _ as checkpoint ->
        let context = Context.update context checkpoint in
        run ~context (Engine.resume checkpoint)
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
