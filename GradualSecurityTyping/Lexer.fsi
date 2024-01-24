module Lexer

/// Rule token
val token: lexbuf: LexBuffer<char> -> token
/// Rule comment
val comment: lexbuf: LexBuffer<char> -> token
/// Rule read_string
val read_string: str: obj -> ignorequote: obj -> lexbuf: LexBuffer<char> -> token
