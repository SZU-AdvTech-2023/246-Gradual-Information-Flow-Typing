// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | RPAREN
  | LPAREN
  | SEMICOLON
  | IN
  | LET
  | ELSE
  | THEN
  | IF
  | GREATER
  | LESS
  | EQUAL
  | DIV
  | MULT
  | PLUS
  | MINUS
  | SEMI_COLON of (Syntax.Position)
  | BACK_DASH
  | ARROW
  | DOT
  | HASH_STRING
  | HASH_PRINT
  | DOUBLE_ARROW of (Syntax.Position)
  | ANGLE
  | EXCL
  | LEVEL of (SecurityLevel)
  | STRING of (string)
  | TYPE_IDENT of (string)
  | IDENT of (string)
  | INT of (int)
  | BOOL of (bool)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_SEMICOLON
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_ELSE
    | TOKEN_THEN
    | TOKEN_IF
    | TOKEN_GREATER
    | TOKEN_LESS
    | TOKEN_EQUAL
    | TOKEN_DIV
    | TOKEN_MULT
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_SEMI_COLON
    | TOKEN_BACK_DASH
    | TOKEN_ARROW
    | TOKEN_DOT
    | TOKEN_HASH_STRING
    | TOKEN_HASH_PRINT
    | TOKEN_DOUBLE_ARROW
    | TOKEN_ANGLE
    | TOKEN_EXCL
    | TOKEN_LEVEL
    | TOKEN_STRING
    | TOKEN_TYPE_IDENT
    | TOKEN_IDENT
    | TOKEN_INT
    | TOKEN_BOOL
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprogram
    | NONTERM_program
    | NONTERM_term
    | NONTERM_simple_term
    | NONTERM_raw_value
    | NONTERM_labeled_value
    | NONTERM_constant
    | NONTERM_raw_type
    | NONTERM_labeled_type
    | NONTERM_args
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val program : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Term) 
