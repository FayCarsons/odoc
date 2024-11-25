%{
  open Parser_aux

  open Writer.Prelude
  let ( let+ ) = Fun.flip Writer.map
  let wrap_location : lexspan -> 'a -> 'a Loc.with_location =
  fun loc value ->
    let location = Parser_aux.to_location loc in
    { location; value }

  let not_empty : 'a list -> bool = function _ :: _ -> true | _ -> false

  type align_error =
    | Invalid_align (* An invalid align cell *)
    | Not_align (* Not an align cell *)

  (* This could be made a bit more specific by allowing Space elements only at
    the beginning and end *)
  let valid_elements (cell : Ast.inline_element list) : string option =
    let rec go acc = function
      | `Word _ :: _ when Option.is_some acc -> None
      | `Word word :: rest -> go (Some word) rest
      | `Space _ :: rest -> go acc rest
      | _ :: _ -> None
      | [] -> acc
    in
    go None cell

  let valid_word word =
    match String.length word with
    | 0 -> Ok None
    | 1 -> (
        match word.[0] with
        | ':' -> Ok (Some `Center)
        | '-' -> Ok None
        | _ -> Error Not_align)
    | len ->
        if String.for_all (Char.equal '-') (String.sub word 1 (len - 2)) then
          match (word.[0], word.[pred len]) with
          | ':', '-' -> Ok (Some `Left)
          | '-', ':' -> Ok (Some `Right)
          | ':', ':' -> Ok (Some `Center)
          | '-', '-' -> Ok None
          | _ -> Error Invalid_align
        else Error Not_align

  let valid_align_cell (cell : Ast.inline_element Loc.with_location list) =
    match List.map Loc.value cell |> valid_elements with
    | Some word -> valid_word word
    | None -> Error Not_align

  let sequence : ('elt, 'err) result list -> ('elt list, 'err) result =
  fun list ->
    let rec go acc : ('elt, 'err) result list -> ('elt list, 'err) result =
      function
      | Ok x :: xs -> go (x :: acc) xs
      | Error err :: _ -> Error err
      | [] -> Ok (List.rev acc)
    in
    go [] list

  (* NOTE: (@FayCarsons)
    When we get something that doesn't look like an align at all, we check to see if we've gotten
    any valid aligns, if so we assume that the cell being considered is supposed to be an align and treat it as an error,
    otherwise we assume the row is not supposed to be an align row *)
  let valid_align_row (row : Ast.inline_element Loc.with_location list list) :
      (Ast.alignment option list, align_error) result =
    let align, not_align =
      List.map valid_align_cell row
      |> List.partition (function
          | Ok _ | Error Invalid_align -> true
          | _ -> false)
    in
    match (align, not_align) with
    | _ :: _, _ :: _ -> Error Invalid_align
    | _ :: _, [] -> sequence align
    | _ -> Error Not_align

  let to_paragraph :
      Ast.inline_element Loc.with_location list ->
      Ast.nestable_block_element Loc.with_location =
  fun words ->
    let span = Loc.span @@ List.map Loc.location words in
    Loc.at span (`Paragraph words)

  (* Merges inline elements within a cell into a single paragraph element, and tags cells w/ tag *)
  let merged_tagged_row tag : 'a Loc.with_location list list -> 'b =
    List.map (fun elts -> ([ to_paragraph elts ], tag))
  let as_data = merged_tagged_row `Data
  let as_header = merged_tagged_row `Header

  let mktable data header align =
    match valid_align_row align with
    | Ok alignment -> `Table ((header :: data, Some alignment), `Light)
    | Error Invalid_align -> `Table ((header :: data, None), `Light)
    | Error Not_align ->
        let rows = header :: as_data align :: data in
        `Table ((rows, None), `Light)

  let construct_table :
      ?header:Ast.inline_element Loc.with_location list list Writer.t ->
      ?align:Ast.inline_element Loc.with_location list list Writer.t ->
      Ast.inline_element Loc.with_location list list list Writer.t ->
      Ast.nestable_block_element Writer.t =
  fun ?header ?align data ->
    let* data = Writer.map (List.map as_data) data in
    let header = Option.value ~default:(Writer.return []) header in
    let* header = Writer.map as_header header in
    match align with
    | Some align ->
        let* table = Writer.map (mktable data header) align in
        Writer.return table
    | None -> Writer.return @@ mktable data header []

  let unclosed_table
      ?(data :
        Ast.inline_element Loc.with_location list list list Writer.t option)
      warning : Ast.nestable_block_element Writer.t =
    let node =
      match data with
      | Some data ->
          Writer.map
            (fun data -> `Table ((List.map as_data data, None), `Light))
            data
      | None -> Writer.return @@ `Table (([], None), `Light)
    in
    Writer.warning warning node

  let media_kind_of_target =
    let open Tokens in
    function Audio -> `Audio | Video -> `Video | Image -> `Image

  let href_of_media =
    let open Tokens in
    function Reference name -> `Reference name | Link uri -> `Link uri

  let split_simple_media Loc.{ location; value = media, target } =
    (Loc.at location media, target)

  let split_replacement_media Loc.{ location; value = media, target, content } =
    (Loc.at location media, target, content)

  let tag t = `Tag t
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
%token LI DASH

%token TABLE_LIGHT
%token TABLE_HEAVY 
%token TABLE_ROW 
%token <Ast.table_cell_kind> Table_cell

%token <int * string option> Section_heading

(* Tags *)
%token <string> Author
%token DEPRECATED
%token <string> Param
%token <string> Raise
%token RETURN
%token <[ `Url | `File | `Document ] * string> See
%token <string> Since
%token <string> Before
%token <string> Version
%token <string> Canonical
%token INLINE OPEN CLOSED HIDDEN

%token <string> Simple_ref 
%token <string> Ref_with_replacement 
%token <string> Simple_link 
%token <string> Link_with_replacement
%token <Tokens.media * Tokens.media_target> Media 
%token <Tokens.media * Tokens.media_target * string> Media_with_replacement
%token <string> Verbatim

%token END
%start <Ast.t Writer.t> main 

%%

(* Utility which wraps the return value of a producer in `Loc.with_location` *)
let locatedM(rule) == inner = rule; { Writer.map (wrap_location $loc)  inner }
let located(rule) == | inner = rule; { wrap_location $sloc inner }

let sequence(rule) == xs = rule*; { Writer.sequence xs }
let sequence_nonempty(rule) == xs = rule+; { Writer.sequence xs }
let separated_sequence_nonempty(separator, X) := 
  | x = X; { let* x = x in return [ x ] } 
  | x = X; separator; xs = separated_sequence_nonempty(separator, X);
    {
      let* x = x in 
      let* xs = xs in
      return @@ x :: xs
    }

(* ENTRY-POINT *)

(* A comment can either contain block elements, block elements and then tags, or
   just tags, but not tags and then block elements *)
let main :=  
  | nodes = sequence_nonempty(locatedM(toplevel)); whitespace?; END; { nodes }
  | whitespace?; END; { Writer.return @@ [] }

let toplevel :=
  | block = nestable_block_element; { Writer.map (fun b -> (b :> Ast.block_element) ) block }
  | ~ = tag; <>
  | ~ = heading; <>

let horizontal_whitespace := 
  | SPACE; { `Space " " } 
  | ~ = Space; <`Space>

let newline := NEWLINE; {} | Single_newline; {} 

let whitespace := 
  | horizontal_whitespace; {}
  | newline; {}

let heading := 
  | (num, title) = Section_heading; children = sequence_nonempty(locatedM(inline_element)); RIGHT_BRACE; 
    { 
      let* children = children in 
      return @@ `Heading (num, title, children) 
    }

let tag == 
  | t = tag_with_content; { Writer.map tag t }
  | t = tag_bare; { Writer.map tag t }

let tag_with_content := 
  | version = Before; children = sequence_nonempty(locatedM(nestable_block_element)); 
    { Writer.map (fun c -> `Before (version, c)) children }
  | DEPRECATED; children = sequence_nonempty(locatedM(nestable_block_element)); 
    { Writer.map (fun c -> `Deprecated c) children }
  | RETURN; children = sequence_nonempty(locatedM(nestable_block_element));
    { Writer.map (fun c -> `Return c) children }
  | ident = Param; children = sequence_nonempty(locatedM(nestable_block_element));
    { Writer.map (fun c -> `Param (ident, c)) children }
  | exn = Raise; children = sequence_nonempty(locatedM(nestable_block_element));
    { Writer.map (fun c -> `Raise (exn, c)) children }
  | (kind, href) = See; horizontal_whitespace?; children = sequence_nonempty(locatedM(nestable_block_element)); 
    {
      let* children = children in
      return @@ `See (kind, href, children)
    }

let tag_bare :=
  | version = Version; { return @@ `Version version }
  | version = Since; { return @@ `Since version }
  | impl = located(Canonical); { return @@ `Canonical impl }
  | version = Author; { return @@ `Author version }
  | OPEN; { return `Open }
  | INLINE; { return `Inline }
  | CLOSED; { return `Closed }
  | HIDDEN; { return `Hidden }

(* INLINE ELEMENTS *)

let inline_element := 
  | s = horizontal_whitespace; { return s } 
  | w = Word; { return @@ `Word w }
  | c = Code_span; { return @@ `Code_span c }
  | m = Raw_markup; { return @@ `Raw_markup m }
  | style = Style; children = sequence_nonempty(locatedM(inline_element)); RIGHT_BRACE; 
    { Writer.map (fun c -> `Styled (style, c)) children }
  | style = Style; RIGHT_BRACE; 
    {
      let location = Parser_aux.to_location $sloc in
      let node = `Styled (style, [ Loc.at location (`Word "") ]) in
      let what = Tokens.describe @@ Style style in
      let warning = fun ~filename -> 
        let span = Parser_aux.to_location ~filename $sloc in
        Parse_error.should_not_be_empty ~what span 
      in
      Writer.with_warning node warning
    } 
  | m = Math_span; { return @@ `Math_span m }
  | ~ = reference; <>
  | ~ = link; <>

let reference := 
  | ref_body = located(Simple_ref); children = sequence(locatedM(inline_element)); 
    {
      let+ children = children in
      let ref_body = Loc.nudge_map_start (String.length "{!") ref_body in
      `Reference (`Simple, ref_body, children)
    }
  | ref_body = located(Ref_with_replacement); children = sequence_nonempty(locatedM(inline_element)); RIGHT_BRACE;
    { 
      let+ children = children in 
      let ref_body = Loc.nudge_map_start (String.length "{{!") ref_body in
      `Reference (`With_text, ref_body, children)
    }
  | ref_body = located(Ref_with_replacement); RIGHT_BRACE;
    {
      let span = 
        Parser_aux.to_location $sloc
        |> Loc.nudge_start (String.length "{{!") 
      in
      let node = `Reference (`With_text, ref_body, []) in
      let what = Tokens.describe @@ Ref_with_replacement (Loc.value ref_body) in
      let warning = fun ~filename:_f -> 
        Parse_error.should_not_be_empty ~what span 
      in
      Writer.with_warning node warning
    }

let link := | link_body = Simple_link; RIGHT_BRACE; 
    { 
      let node = `Link (link_body, []) in
      let url = String.trim link_body in
      if "" = url then 
        let what = Tokens.describe @@ Simple_link link_body in
        let span = Parser_aux.to_location $sloc in
        let warning = fun ~filename:_f -> 
          Parse_error.should_not_be_empty ~what span
        in
        Writer.with_warning node warning
      else
        return node
    }
  | link_body = Link_with_replacement; children = sequence_nonempty(locatedM(inline_element)); RIGHT_BRACE; 
    { 
      let* c = children in 
      let node = `Link (link_body, c) in
      if "" = link_body then
        let what = Tokens.describe @@ Link_with_replacement link_body in
        let span = Parser_aux.to_location $sloc in
        let warning = fun ~filename:_f -> 
          Parse_error.should_not_be_empty ~what span
        in
        Writer.with_warning node warning
      else 
        return node
    }
  | link_body = Link_with_replacement; whitespace?; RIGHT_BRACE;
    {
      let span = 
        Parser_aux.to_location $sloc
        |> Loc.nudge_start (String.length "{{!") 
      in
      let node = `Link (link_body, []) in
      let what = Tokens.describe @@ Link_with_replacement link_body in
      let warning = fun ~filename:_f -> 
        Parse_error.should_not_be_empty ~what span 
      in
      Writer.with_warning node warning
    }

(* LIST *)

let list_light_item_unordered == 
  | MINUS; ~ = locatedM(nestable_block_element); <>
  | horizontal_whitespace; MINUS; item = locatedM(nestable_block_element);
    { 
      let warning = fun ~filename -> 
        let span = Parser_aux.to_location ~filename $sloc in
        Parse_error.should_begin_on_its_own_line ~what:(Tokens.describe MINUS) span
      in
      Writer.warning warning item 
    }

let list_light_item_ordered == 
  | PLUS; ~ = locatedM(nestable_block_element); <>
  | horizontal_whitespace; PLUS; item = locatedM(nestable_block_element);
    { 
      let warning = fun ~filename -> 
        let span = Parser_aux.to_location ~filename $sloc in
        Parse_error.should_begin_on_its_own_line ~what:(Tokens.describe MINUS) span
      in
      Writer.warning warning item 
    }

let list_light := 
  | children = separated_nonempty_list(newline, list_light_item_ordered); 
    { let* children = Writer.sequence children in return @@ `List (`Ordered, `Light, [ children ]) }
  | children = separated_nonempty_list(newline, list_light_item_unordered); 
    { let* children = Writer.sequence children in return @@ `List (`Unordered, `Light, [ children ]) }

(* TODO: Refactor `List_item` into two tokens - Li and Dash - so that we can
   handle the case where '{li' is *not* followed by whitespace in our Menhir
   rule as opposed to it's semantic action *)
let item_heavy ==
    | LI; whitespace; items = sequence(locatedM(nestable_block_element)); RIGHT_BRACE; whitespace?; 
      {
        let warning = fun ~filename -> 
          let span = Parser_aux.to_location ~filename $sloc in 
          Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span 
        in
        Writer.ensure not_empty warning items 
      }
    | LI; items = sequence(locatedM(nestable_block_element)); RIGHT_BRACE; whitespace?; 
      {
        let warning = fun ~filename -> 
          let span = Parser_aux.to_location ~filename $sloc in
          Parse_error.should_be_followed_by_whitespace ~what:(Tokens.describe LI) span
        in
        let writer = 
          let warning = fun ~filename -> 
            let span = Parser_aux.to_location ~filename $sloc in 
            Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span 
          in
          Writer.ensure not_empty warning items 
        in
        Writer.warning warning writer
      }
    | DASH; whitespace?; items = sequence(locatedM(nestable_block_element)); RIGHT_BRACE; whitespace?; 
      {
        let warning = fun ~filename -> 
          let span = Parser_aux.to_location ~filename $sloc in 
          Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span 
        in
        Writer.ensure not_empty warning items 
      }
    | LI; whitespace?; items = sequence(locatedM(nestable_block_element))?; END;
      {
        let warning = fun ~filename -> 
          let span = Parser_aux.to_location ~filename $sloc in 
          Parse_error.end_not_allowed ~in_what:(Tokens.describe LI) span
        in
        match items with 
        | Some items -> 
          let writer = 
            let warning = fun ~filename -> 
              let span = Parser_aux.to_location ~filename $sloc in 
              Parse_error.should_not_be_empty ~what:(Tokens.describe LI) span 
            in
            Writer.ensure not_empty warning items 
          in
          Writer.warning warning writer
        | None ->
          Writer.with_warning [] warning 
      }
    | DASH; whitespace?; items = sequence(locatedM(nestable_block_element))?; END;
      {
        let warning = fun ~filename -> 
          let span = Parser_aux.to_location ~filename $sloc in 
          Parse_error.end_not_allowed ~in_what:(Tokens.describe DASH) span
        in
        match items with 
        | Some items -> 
          let writer = 
            let warning = fun ~filename -> 
              let span = Parser_aux.to_location ~filename $sloc in 
              Parse_error.should_not_be_empty ~what:(Tokens.describe DASH) span 
            in
            Writer.ensure not_empty warning items 
          in
          Writer.warning warning writer
        | None ->
          Writer.with_warning [] warning 
      }

let list_heavy := 
    | list_kind = List; whitespace?; items = sequence(item_heavy); whitespace?; RIGHT_BRACE;
      { Writer.map (fun items -> `List (list_kind, `Heavy, items)) items }

let list_element := 
  | ~ = list_light; <>
  | ~ = list_heavy; <>

(* TABLES *)

let cell_heavy := cell_kind = Table_cell; whitespace?; children = sequence(locatedM(nestable_block_element)); whitespace?; RIGHT_BRACE;
  { Writer.map (fun c -> (c, cell_kind)) children }
let row_heavy == TABLE_ROW; whitespace?; ~ = sequence_nonempty(cell_heavy); RIGHT_BRACE; <>
let table_heavy == TABLE_HEAVY; whitespace?; grid = sequence_nonempty(row_heavy); RIGHT_BRACE; 
  { Writer.map (fun g -> `Table ((g, None), `Heavy)) grid }

let cell_light := ~ = sequence_nonempty(locatedM(inline_element)); <>
let row_light := BAR?; cells = separated_sequence_nonempty(BAR, cell_light); BAR?; <> 
let rows_light := ~ = separated_sequence_nonempty(newline, row_light); <>
let table_light :=
  (* If the first row is the alignment row then the rest should be data *)
  | TABLE_LIGHT; align = row_light; newline; data = rows_light; whitespace?; RIGHT_BRACE;
    { construct_table ~align data }
  (* Otherwise the first should be the headers, the second align, and the rest data *)
  | TABLE_LIGHT; header = row_light; newline; align = row_light; newline; data = rows_light; whitespace?; RIGHT_BRACE;
    { construct_table ~header ~align data }
  (* If there's only one row and it's not the align row, then it's data *)
  | TABLE_LIGHT; data = rows_light; whitespace?; RIGHT_BRACE; { construct_table data }
  (* If there's nothing inside, return an empty table *)
  | TABLE_LIGHT; whitespace?; RIGHT_BRACE; 
    { return @@ `Table (([[]], None), `Light) }
  | TABLE_LIGHT; whitespace?; END; 
    {
      let in_what = Tokens.describe TABLE_LIGHT in
      let warning = fun ~filename -> 
        let span = Parser_aux.to_location ~filename $sloc in
        Parse_error.end_not_allowed ~in_what span 
      in
      unclosed_table warning
    }
  | TABLE_LIGHT; data = rows_light; whitespace?; END; 
    {
      let in_what = Tokens.describe TABLE_LIGHT in
      let warning = fun ~filename -> 
        let span = Parser_aux.to_location ~filename $sloc in
        Parse_error.end_not_allowed ~in_what span
      in
      unclosed_table ~data warning
    }

let table := 
  | ~ = table_heavy; <>
  | ~ = table_light; <>

(* MEDIA *)

(* TODO: Naming needs some work here, multiple fields labeled as `kind` which
   doesn't make sense *)
let media := 
  | media = located(Media); whitespace*; 
    { 
      let (located_media_kind, media_href) = split_simple_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      let kind = media_kind_of_target media_href in
      return @@ `Media (`Simple, wrapped_located_kind, "", kind)  
    }
  | media = located(Media_with_replacement); whitespace*;
    { 
      let (located_media_kind, media_href, content) = split_replacement_media media in 
      let wrapped_located_kind = Loc.map href_of_media located_media_kind in 
      let kind = media_kind_of_target media_href in
      return @@ `Media (`With_text, wrapped_located_kind, content, kind) 
    }

(* TOP-LEVEL ELEMENTS *)

let nestable_block_element := 
  | v = Verbatim; { return (`Verbatim v) }
  | items = sequence_nonempty(locatedM(inline_element)); 
    { Writer.map (fun i -> `Paragraph i) items }
  | c = Code_block; { return (`Code_block c) }
  | modules = located(Modules)+; { return (`Modules modules) }
  | ~ = list_element; <>
  | ~ = table; <> 
  | ~ = media; <>
  | m = Math_block; { return (`Math_block m) }

