open Parser

module Engine = MenhirInterpreter

let unreachable s = failwith @@ Printf.sprintf "UNREACHABLE BRANCH IN %s\n" s

module Context : sig
  type with_position = Parser.token * Lexing.position * Lexing.position

  type 'a t = {
    file : string;
    last_valid : 'a Engine.checkpoint;
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
    first_checkpoint:'a Engine.checkpoint ->
    'a t

  val push_token : 'a t -> with_position -> 'a t
  val peek : 'a t -> with_position option
  val pop : 'a t -> with_position option * 'a t
  val update_focus : 'a t -> 'b Engine.checkpoint -> 'a t

  val context_string : 'a t -> string
  val to_string : 'a t -> string
end = struct
  open Engine

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

  let push_token : 'a t -> with_position -> 'a t =
   fun self token -> { self with tokens = token :: self.tokens }

  let peek : 'a t -> with_position option = function
    | { tokens = x :: _; _ } -> Some x
    | _ -> None

  let pop : 'a t -> with_position option * 'a t = function
    | { tokens = x :: xs; _ } as self -> (Some x, { self with tokens = xs })
    | self -> (None, self)

  let to_string : 'a t -> string =
   fun self ->
    let current = string_of_context self.current_symbol
    and most_recent_token =
      Option.map (fun (t, _, _) -> Describe.describe t) (peek self)
      |> Option.value ~default:"No tokens"
    in
    Printf.sprintf "{ checkpoint: %s; current: %s; most_recent_token: %s }\n%!"
      (pp_checkpoint self.last_valid)
      current most_recent_token

  let get_focus : production * int -> context option =
   fun (production, _) ->
    match lhs production with
    | X (N N_main) -> Some Main
    | X (N N_nestable_block_element) -> Some (Block None)
    | X (N N_tag) -> Some (Tag None)
    | X (N N_heading) -> Some Heading
    | _ -> None

  let update_focus (self : 'a t) = function
    | Shifting (_before, after, _not_finished_parsing) -> (
        match stack after with
        | (lazy (Cons (Engine.Element (state, _, _, _), _))) ->
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

(* Constructing error messages *)

let warn_if_after_text :
    push_warning:(Warning.t -> unit) -> Parser.token Loc.with_location -> unit =
 fun ~push_warning { Loc.location; value = token } ->
  push_warning
  @@ Parse_error.should_begin_on_its_own_line ~what:(Describe.describe token)
       location

let warn_if_after_tags :
    push_warning:(Warning.t -> unit) -> Parser.token Loc.with_location -> unit =
 fun ~push_warning { Loc.location; value = token } ->
  let suggestion =
    Printf.sprintf "move %s before any tags." (Describe.describe token)
  in
  push_warning
  @@ Parse_error.not_allowed ~what:(Describe.describe token)
       ~in_what:"the tags section" ~suggestion location

let warn_because_not_at_top_level :
    push_warning:(Warning.t -> unit) ->
    parent_markup:Parser.token ->
    Parser.token Loc.with_location ->
    unit =
 fun ~push_warning ~parent_markup { Loc.location; value = token } ->
  let suggestion =
    Printf.sprintf "move %s outside of any other markup." (Describe.print token)
  in
  push_warning
  @@ Parse_error.not_allowed ~what:(Describe.describe token)
       ~in_what:(Describe.describe parent_markup)
       ~suggestion location

let element_cant_be_empty : Parser.token -> bool = function
  | Simple_link _ | Link_with_replacement _ | Ref_with_replacement _ -> true
  | _ -> false

let how_many_times = ref 1

let contextual_error_message :
    type a b.
    context:a Context.t ->
    checkpoint:a Engine.checkpoint ->
    continue:(a Engine.checkpoint -> Ast.t) ->
    Ast.t =
 fun ~context ~checkpoint ~continue ->
  Printf.printf "We have traversed the error branch of the program %d times\n"
    !how_many_times;
  incr how_many_times;

  match checkpoint with
  | Engine.HandlingError env ->
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
        Engine.feed
          Engine.(N N_inline_element)
          start_pos dummy_inline_element end_pos env
      in
      (* NOTE: SEGFAULTS HERE *)
      let checkpoint = Engine.input_needed env in
      continue checkpoint
  | _ -> unreachable "contextual_error_message"

let run_parser :
    starting_location:Lexing.position ->
    next_token:(unit -> Context.with_position) ->
    push_warning:(Warning.t -> unit) ->
    Ast.t =
 fun ~starting_location ~next_token ~push_warning ->
  let open Engine in
  let rec handle_error ~context checkpoint =
    contextual_error_message ~context ~checkpoint ~continue:(run ~context)
  and run ~context = function
    | InputNeeded _env as checkpoint ->
        let token = next_token () in
        let checkpoint = Engine.offer checkpoint token
        and context = Context.push_token context token in
        run ~context checkpoint
    | Shifting _ as checkpoint ->
        let context = Context.update_focus context checkpoint in
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
