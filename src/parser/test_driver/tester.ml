  let text = "\nfoo\n"

  let _ =
    let location = Lexing.dummy_pos in
    let _ = Odoc_parser.parse_comment ~text ~location in ()
