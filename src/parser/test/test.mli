val test : ?location:Odoc_parser.Loc.point -> string -> unit
val output :
  Format.formatter -> Odoc_parser.Ast.t * Odoc_parser.Warning.t list -> unit
