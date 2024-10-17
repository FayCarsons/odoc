%{
  open Parser_aux

  let point_of_position Lexing.{ pos_lnum; pos_cnum;  _ } = 
      Loc.{ line = pos_lnum; column = pos_cnum  }

  type lexspan = (Lexing.position * Lexing.position)
  let to_location :  lexspan -> Loc.span =
    fun (start, end_) -> 
      let open Loc in
      let start_point = point_of_position start 
      and end_point = point_of_position end_ in 
      { file = start.pos_fname; start = start_point; end_ = end_point } 

  let wrap_location : lexspan -> 'a -> 'a Loc.with_location = fun loc value -> 
    let location = to_location loc in 
    { location; value }

  let pp_tag : Parser_aux.tag -> string = function 
  | Author _ -> "@author"
  | Deprecated -> "@deprecated"
  | Param _ -> "@param"
  | Raise _ -> "@raise/@raises"
  | Return -> "@return"
  | See _ -> "@see"
  | Since _ -> "@since"
  | Before _ -> "@before"
  | Version _ -> "@version"
  | Canonical _ -> "@canonical"
  | Inline | Open | Closed | Hidden -> "<internal>"

  exception No_children of string Loc.with_location

  let tag : Ast.tag -> Ast.block_element = fun tag -> `Tag tag 

  let tag_with_element (children : Ast.nestable_block_element Loc.with_location list) : Parser_aux.tag -> Ast.block_element = function 
  | Before version -> tag @@ `Before (version, children) 
  | Deprecated -> tag @@ `Deprecated children
  | Return -> tag @@ `Return children
  | Param param_name -> tag @@ `Param (param_name, children)
  | Raise exn -> tag @@ `Raise (exn, children)
  | See (kind, href) -> tag @@ `See (kind, href, children)
  | _ -> assert false (* Unreachable *)

  let tag_bare loc = function 
  | Version version -> tag @@ `Version version 
  | Since version -> tag @@ `Since version 
  | Canonical implementation -> tag @@ `Canonical (wrap_location loc implementation)
  | Author author -> tag @@ `Author author
  | Inline -> `Tag `Inline 
  | Open -> `Tag `Open 
  | Closed -> `Tag `Closed
  | Hidden -> `Tag `Hidden
  (* NOTE: (@FayCarsons) This should be unreachable, remove after dev *)
  | tag -> raise @@ No_children (wrap_location loc @@ Printf.sprintf "Tag %s expects children" (pp_tag tag)) 

  type align_error = 
  | Invalid_align (* An invalid align cell *)
  | Not_align (* Not an align cell *)

  (* This could be made a bit more specific by allowing Space elements only at
     the beginning and end *)
  let valid_elements (cell : Ast.inline_element list): string option = 
    let rec go acc = function
      | `Word _ :: _ when Option.is_some acc -> None 
      | `Word word :: rest ->
        go (Some word) rest
      | `Space _ :: rest -> 
        go acc rest
      | _ :: _ -> None 
      | [] -> acc
    in 
    go None cell

  let valid_word word =
    match String.length word with
    | 0 -> Ok None
    | 1 -> 
      begin
        match word.[0] with
        | ':' -> Ok (Some `Center)
        | '-' -> Ok None
        | _ -> Error Not_align
      end
    | len -> 
      if String.for_all (Char.equal '-') (String.sub word 1 (len - 2)) then  
      match word.[0], word.[pred len] with
      | ':', '-' -> Ok (Some `Left)
      | '-', ':' -> Ok (Some `Right)
      | ':', ':' -> Ok (Some `Center)
      | '-', '-' -> Ok None
      | _ -> Error Invalid_align
      else Error Not_align

  let valid_align_cell cell = 
    match List.map Loc.value cell |> valid_elements with
    | Some word -> valid_word word 
    | None -> Error Not_align

  let sequence : ('elt, 'err) result list -> ('elt list, 'err) result =
    fun list ->
    let rec go acc : ('elt, 'err) result list -> ('elt list, 'err) result =
      function 
      | Ok x :: xs -> go (x :: acc) xs
      | Error err :: _ -> Error err
      | [] ->  Ok (List.rev acc) 
    in 
    go [] list

  (* NOTE: (@FayCarsons) 
     When we get something that doesn't look like an align at all, we check to see if we've gotten 
     any valid aligns, if so we assume that the cell being considered is supposed to be an align and treat it as an error,
     otherwise we assume the row is not supposed to be an align row *)
  let valid_align_row (row : Ast.inline_element Loc.with_location list list): (Ast.alignment option list, align_error) result =
    let (align, not_align) = List.map valid_align_cell row |> List.partition (function Ok _ | Error Invalid_align -> true | _ -> false) in 
    match align, not_align with 
    | _ :: _, _ :: _ -> Error Invalid_align
    | _ :: _, [] -> sequence align
    | _ -> Error Not_align

  let to_paragraph : Ast.inline_element Loc.with_location list -> Ast.nestable_block_element Loc.with_location list 
    = fun words ->
        let span = Loc.span @@ List.map Loc.location words in 
        [ Loc.at span (`Paragraph words) ]

  (* Merges inline elements within a cell into a single paragraph element, and tags cells w/ tag *)
  let merged_tagged_row tag : Ast.inline_element Loc.with_location list list -> Ast.nestable_block_element Ast.row  
    = List.map (fun elts -> to_paragraph elts, tag)
  let as_data = merged_tagged_row `Data
  let as_header = merged_tagged_row `Header

  let media_kind_of_target = function
    | Audio -> `Audio
    | Video -> `Video
    | Image -> `Image

  let href_of_media = function
    | Reference name -> `Reference name
    | Link uri -> `Link uri

  let split_simple_media Loc.{ location; value = (media, target) } = 
    (Loc.at location media, target)

  let split_replacement_media Loc.{ location; value = (media, target, content) } =
    (Loc.at location media, target, content)

%}

%token SPACE NEWLINE
%token RIGHT_BRACE
%token RIGHT_CODE_DELIMITER

%token <string> Blank_line
%token <string> Single_newline
%token <string> Space

%token <string> Word

%token MINUS PLUS BAR

%token <Ast.style> Style
%token <Ast.alignment> Paragraph_style

%token <string> Modules

%token <string> Math_span 
%token <string> Math_block

%token <string option * string> Raw_markup

%token <Ast.code_block> Code_block
%token <string> Code_span 

%token <Ast.list_kind> List
%token <Ast.list_item> List_item

%token TABLE_LIGHT
%token TABLE_HEAVY 
%token TABLE_ROW 
%token <Ast.table_cell_kind> Table_cell

%token <int * string option> Section_heading

%token <Parser_aux.tag> Tag

%token <string> Simple_ref 
%token <string> Ref_with_replacement 
%token <string> Simple_link 
%token <string> Link_with_replacement
%token <Parser_aux.media * Parser_aux.media_target> Media 
%token <Parser_aux.media * Parser_aux.media_target * string> Media_with_replacement
%token <string> Verbatim

%token END

%start <Ast.t> main 

%%

(* Utility which wraps the return value of a producer in `Loc.with_location` *)
let located(rule) == inner = rule; { wrap_location $loc inner }

(* ENTRY-POINT *)

let main :=  
  | ~ = located(toplevel)+; END; <>
  | _ = whitespace; END; { [] }
  | END; { [] }

let toplevel ==
  | ~ = tag; whitespace*; <>
  | block = nestable_block_element; whitespace*; { (block :> Ast.block_element) }
  | ~ = heading; whitespace*; <>

let whitespace := 
  | SPACE; { `Space " " } 
  | NEWLINE; { `Space "\n" }
  | ~ = Space; <`Space>
  | ~ = Single_newline; <`Space>

let heading := 
  | (num, title) = Section_heading; children = list(located(inline_element)); RIGHT_BRACE; 
    { `Heading (num, title, children) }

let tag := 
  | inner_tag = Tag; children = located(nestable_block_element)+; { tag_with_element children inner_tag }
  | inner_tag = Tag; { tag_bare $loc inner_tag }

(* INLINE ELEMENTS *)

let inline_element := 
  | ~ = whitespace; <>
  | ~ = Word; <`Word>
  | ~ = Code_span; <`Code_span>
  | ~ = Raw_markup; <`Raw_markup>
  | style = Style; inner = located(inline_element)+; RIGHT_BRACE; { `Styled (style, inner) }
  | math = Math_span; 
    {
      let start, end_ = $loc(math) in
      let open Lexing in
      Printf.printf "col start: %d, col end: %d\n" start.pos_cnum end_.pos_cnum;
      `Math_span math 
    }
  | ~ = ref; <>
  | ~ = link; <>

let ref := 
  | ref_body = located(Simple_ref); children = located(inline_element)*; 
    { `Reference (`Simple, ref_body, children) }

  | ref_body = located(Ref_with_replacement); 
    children = located(inline_element)*; 
    RIGHT_BRACE;
    { `Reference (`With_text, ref_body, children) }

let link := 
  | link_body = Simple_link; children = located(inline_element)+; RIGHT_BRACE; { `Link (link_body, children) }
  | link_body = Link_with_replacement; children = located(inline_element)+; RIGHT_BRACE; { `Link (link_body, children) }

(* LIST *)

let list_light := 
  | MINUS; unordered_items = separated_list(NEWLINE; SPACE?; MINUS, located(nestable_block_element)); 
    { `List (`Unordered, `Light, [ unordered_items ]) }

  | PLUS; ordered_items = separated_list(NEWLINE; SPACE?; PLUS, located(nestable_block_element)); 
    { `List (`Ordered, `Light, [ ordered_items ]) }

(* NOTE: (@FayCarsons) `List_item` is [ `Li | `Dash ], not sure how that's useful though. Can't find '{li' syntax in Odoc docs *)
let item_heavy == _ = List_item; whitespace*; ~ = located(nestable_block_element)*; whitespace*; RIGHT_BRACE; whitespace*; <>
let list_heavy := 
    | list_kind = List; whitespace*; items = item_heavy*; whitespace*; RIGHT_BRACE;
    { `List (list_kind, `Heavy, items) }

let list_element := 
  | ~ = list_light; <>
  | ~ = list_heavy; <>

(* TABLES *)

let cell_heavy := cell_kind = Table_cell; whitespace?; children = located(nestable_block_element)*; whitespace?; RIGHT_BRACE; whitespace?; { (children, cell_kind) }
let row_heavy == TABLE_ROW; whitespace?; cells = list(cell_heavy); RIGHT_BRACE; whitespace?;  { cells } 
let table_heavy == TABLE_HEAVY; whitespace?; grid = row_heavy*; RIGHT_BRACE; 
  { 
    let abstract = (grid, None) in 
    (abstract, `Heavy) 
  }

let cell_light == BAR?; ~ = located(inline_element)+; <> (* Ast.cell *)
let row_light := ~ = cell_light+; BAR?; NEWLINE; <>  (* Ast.row *)

let table_light :=
  (* If the first row is the alignment row then the rest should be data *)
  | TABLE_LIGHT; align = row_light; data = row_light+; whitespace?; RIGHT_BRACE;
    {
      let data = List.map as_data data in
      match valid_align_row align with
      | Ok alignment -> ((data, Some alignment), `Light)
      | Error Invalid_align -> ((data, None), `Light)
      | Error Not_align ->  
        let rows = as_data align :: data in
        ((rows, None), `Light)
    }

  (* Otherwise the first should be the headers, the second align, and the rest data *)
  | TABLE_LIGHT; header = row_light; align = row_light; data = row_light+;
    whitespace?; RIGHT_BRACE;
    { 
      let data = List.map as_data data 
      and header = as_header header in
      match valid_align_row align with
      | Ok alignment -> (header :: data, Some alignment), `Light
      | Error Invalid_align -> 
        (header :: data, None), `Light
      | Error Not_align -> 
        let rows = header :: as_data align :: data  in
        (rows, None), `Light
    }

  (* If there's only one row and it's not the align row, then it's data *)
  | TABLE_LIGHT; data = row_light+; whitespace?; RIGHT_BRACE; 
    { (List.map as_data data, None), `Light }

  (* If there's nothing inside, return an empty table *)
  | TABLE_LIGHT; SPACE*; RIGHT_BRACE; 
    { ([[]], None), `Light }

let table := 
  | ~ = table_heavy; <`Table>
  | ~ = table_light; <`Table>

(* MEDIA *)

let media := 
  | media = located(Media); whitespace*; 
    { let (located_media_kind, media_href) = split_simple_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      `Media (`Simple, wrapped_located_kind, "", media_kind_of_target media_href) }
  | media = located(Media_with_replacement); whitespace*;
    { let (located_media_kind, media_href, content) = split_replacement_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      `Media (`With_text, wrapped_located_kind, content, media_kind_of_target media_href) }

(* TOP-LEVEL ELEMENTS *)

let nestable_block_element := 
  | ~ = Verbatim; <`Verbatim>
  | ~ = located(inline_element)+; <`Paragraph>
  | ~ = Code_block; <`Code_block>
  | ~ = located(Modules)+; <`Modules>
  | ~ = list_element; <>
  | ~ = table; <> 
  | ~ = media; <>
  | ~ = Math_block; <`Math_block>

