(** Re-export for compatibility with 4.02. *)
type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b

(** The [result] type and a bind operator. This module is meant to be opened. *)
module ResultMonad = struct
  (** Re-export for compat *)
  type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b

  let map_error f = function Ok _ as ok -> ok | Error e -> Error (f e)

  let of_option ~error = function Some x -> Ok x | None -> Error error

  let bind m f = match m with Ok x -> f x | Error _ as e -> e

  let ( >>= ) = bind
end

(** A bind operator for the [option] type. This module is meant to be opened. *)
module OptionMonad = struct
  (* The error case become [None], the error value is ignored. *)
  let of_result = function Result.Ok x -> Some x | Error _ -> None

  let return x = Some x

  let bind m f = match m with Some x -> f x | None -> None

  let ( >>= ) = bind
end

module EitherMonad = struct
  type ('a, 'b) t = Left of 'a | Right of 'b

  let return x = Right x

  let return_left x = Left x

  let bind m f = match m with Right x -> f x | Left y -> Left y

  let bind_left m f = match m with Left x -> f x | Right y -> Right y

  let ( >>= ) = bind

  let of_option ~left = function Some x -> Right x | None -> Left left

  let of_result = function Result.Ok x -> Right x | Error y -> Left y
end

module List = Odoc_list

module Option = struct
  let map f = function None -> None | Some x -> Some (f x)

  let is_some = function None -> false | Some _ -> true
end

module Result = struct
  include Result

  let join = function Ok r -> r | Error _ as e -> e
end

module Fun = struct
  exception Finally_raised of exn

  let protect ~(finally : unit -> unit) work =
    let finally_no_exn () =
      try finally () with e -> raise (Finally_raised e)
    in
    match work () with
    | result ->
        finally_no_exn ();
        result
    | exception work_exn ->
        finally_no_exn ();
        raise work_exn
end

module Tree = Tree
module Forest = Tree.Forest
module Json = Json

include Astring
