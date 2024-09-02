%{
  [@@@warning "-32"]
  open Error
  open Parser_types

  let point_of_position Lexing.{ pos_lnum; pos_cnum; _ } = 
    Loc.{ line = pos_lnum; column = pos_cnum }

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

  let throw : lexspan -> Error.parser_error -> unit = fun loc error -> 
    raise @@ Parser_error (wrap_location loc error)


  exception Debug of [ `DEBUG ] Loc.with_location
  let raise_unimplemented : only_for_debugging:lexspan -> 'a = 
    fun ~only_for_debugging:loc -> 
      raise @@ Debug (wrap_location loc `DEBUG) 

  let exn_location : only_for_debugging:lexspan -> exn = 
    fun ~only_for_debugging:loc  -> Debug (wrap_location loc `DEBUG)  

  let tag : Ast.tag -> Ast.block_element = fun tag -> `Tag tag 

  let tag_with_element loc children = function 
  | Before version -> tag @@ `Before (version, [ wrap_location loc children ]) 
  | Deprecated -> tag @@ `Deprecated [ wrap_location loc children ]
  | Return -> tag @@ `Return [ wrap_location loc children ]
  | Param param_name -> tag @@ `Param (param_name, [ wrap_location loc children ])
  | Raise exn -> tag @@ `Raise (exn, [ wrap_location loc children ])
  | See (kind, href) -> tag @@ `See (kind, href, [ wrap_location loc children ])
  | _ -> raise @@ exn_location ~only_for_debugging:( loc )

  let tag_bare loc = function 
  | Version version -> tag @@ `Version version 
  | Since version -> tag @@ `Since version 
  | Canonical implementation -> tag @@ `Canonical (wrap_location loc implementation)
  | Author author -> tag @@ `Author author
  | Inline -> `Tag `Inline 
  | Open -> `Tag `Open 
  | Closed -> `Tag `Closed
  | Hidden -> `Tag `Hidden
  | _ -> raise @@ exn_location ~only_for_debugging:( loc )

%}

%token SPACE NEWLINE
%token RIGHT_BRACE
%token RIGHT_CODE_DELIMITER
%token COMMENT

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

%token Table_light
%token Table_heavy 
%token Table_row 
%token <Ast.table_cell_kind> Table_cell

%token <int * string option> Section_heading

%token <Parser_types.tag> Tag

%token <string> Simple_ref 
%token <string> Ref_with_replacement 
%token <string> Simple_link 
%token <string> Link_with_replacement
%token <Parser_types.(media * media_target)> Media 
%token <Parser_types.(media * media_target * string)> Media_with_replacement
%token <string> Verbatim

%token END

%start <Ast.t> main 

%%

let located(rule) == value = rule; { wrap_location $loc value }

let main :=  
  | _ = whitespace; { [] }
  | tag_ = tag; { [ wrap_location $sloc tag_ ]}
  | END; { [] }
  | _ = error; { raise @@ exn_location ~only_for_debugging:( $loc ) }

let whitespace := 
  | SPACE; { `Space " " } 
  | NEWLINE; { `Space "\n" }
  | ~ = Space; <`Space>
  | ~ = Blank_line; <`Space>
  | ~ = Single_newline; <`Space>

let inline_element := 
  | ~ = Word; <`Word>
  | ~ = Code_span; <`Code_span>
  | ~ = Raw_markup; <`Raw_markup>
  | style = Style; inner = inline_element; { `Style (style, wrap_location $loc inner)  }
  | ~ = Math_span; <`Math_span>
  | _ = ref; <>

(* TODO: Determine how we want to handle recursive elements like refs and some of the tags that have nestable_block inners
   Currently, this is broken *)
let ref := 
  | ref_body = Simple_ref; children = inline_element; { `Reference (`Simple, ref_body, wrap_location $loc children) }
  | ref_body = Ref_with_replacement; children = inline_element; { `Reference (`Replacement, ref_body, wrap_location $loc children) }

let list_light := 
  | MINUS; unordered_items = separated_list(NEWLINE; MINUS, nestable_block_element); { `List (`Unordered, `Light, unordered_items) }
  | PLUS; ordered_items = separated_list(NEWLINE; PLUS, nestable_block_element); { `List (`Ordered, `Light, unordered_items) }
 
let nestable_block_element := 
  | code = Verbatim; { `Verbatim code }
  | error; { raise_unimplemented ~only_for_debugging:($loc) }

let tag := 
  | inner_tag = Tag; children = nestable_block_element; { tag_with_element $loc children inner_tag }
  | inner_tag = Tag; { tag_bare $loc inner_tag }

let style := ~ = Style; <>
let paragraph_style := ~ = Paragraph_style; <>
