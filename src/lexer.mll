(*
 * This code originated as part of the McGill COMP520 course requirements.
 * Any student examining the code or using any part of the code for their coursework must clearly
 * document what code they examined and give clear credit to any code that they used.
 *
 * Original authors:
 * Rohan Jacob-Rao (rohanjr)
 * Steven Thephsourinthone (stheph)
 * Shawn Otis 
 *)

{
open Lexing
open Parser

exception LexerError of string
exception ParsingError of string * Lexing.position

let last_token_on_line : token option ref = ref None

let ret_token (tok : token) : token =
  last_token_on_line := Some tok; tok

let need_semicolon () : bool =
  match !last_token_on_line with
    | None -> false
    | Some tok -> begin match tok with
      | ID _ -> true | INT | FLOAT64 | BOOL | RUNE | STRING
      | INTLIT _ | FLOATLIT _ | RUNELIT _ | STRLIT _
      | BREAK | CONTINUE | FALLTHROUGH | RETURN
      | INC | DEC | RPAREN | RBRCKT | RBRACE -> true
      | _ -> false end

 let count_newlines str =
  let rec string_explode s = match s with
    | "" -> []
    | s' -> (String.get s' 0)::(string_explode (String.sub s' 1 ((String.length s') - 1)))
  in let chars = string_explode str in
    let binarray = List.map (fun x -> if x = '\n' then 1 else 0) chars in
      List.fold_right ( + ) binarray 0

  let rec multi_new_line n lexbuf = 
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + n}
}

let whitesp = [' ' '\t']+
let lcomm = "//" [^'\n']* (* line comment *)
let comm_text = ([^'*'] | '*' [^'/'])*
let comm_text_no_nl = ([^'*''\n'] | '*' [^'/''\n'])*
let comm_nl = '\n' | "*\n"
(* multiline comments with and without newlines inside *)
let mlcomm_no_nl = "/*" comm_text_no_nl "*/"
let mlcomm_nl = "/*" comm_text comm_nl comm_text "*/"

let dec_digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let dec_lit = ['1'-'9'] dec_digit*
let oct_lit = '0' oct_digit*
let hex_lit = '0' ['x' 'X'] hex_digit+
let float_lit1 = dec_digit+ '.' dec_digit* (* like float literals in OCaml *)
let float_lit2 = '.' dec_digit+ (* need to add '0' before conversion to float *)

let esc_char = ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\']
let esc_char_lit = '\'' '\\' (esc_char | '\'') '\''
let nonesc_char_lit = '\'' [^'\\' '\'' '\n'] '\''
let rune_lit = nonesc_char_lit | esc_char_lit

let interp_str_lit = '"' ([^'\\' '"' '\n'] | '\\' esc_char | '\\' '"')* '"'
let raw_str_lit = '`' [^ '`']* '`'

let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter (letter | dec_digit)*

rule token = parse
  | whitesp | mlcomm_no_nl          { token lexbuf }
  | ('\n' | lcomm '\n' | mlcomm_nl) as str { multi_new_line (count_newlines str) lexbuf; if need_semicolon () then
                                      (last_token_on_line := None; SEMICOLON) else (last_token_on_line := None; token lexbuf) }
  (* literals *)
  | dec_lit | hex_lit as str        { ret_token (INTLIT (int_of_string str)) }
  | oct_lit as str                  { ret_token (INTLIT (int_of_string ("0o" ^ str))) }
  | float_lit1 as str               { ret_token (FLOATLIT (float_of_string str)) }
  | float_lit2 as str               { ret_token (FLOATLIT (float_of_string ("0" ^ str))) }
  | rune_lit as str                 { ret_token (RUNELIT str) }
  | interp_str_lit as str           { ret_token (STRLIT str) }
  | raw_str_lit as str              { ret_token (STRLIT str) } (* TODO remove carriage returns *)  
  (* Go keywords *)
  | "break"                         { ret_token BREAK }
  | "case"                          { ret_token CASE }
  | "chan"                          { ret_token CHAN }
  | "const"                         { ret_token CONST }
  | "continue"                      { ret_token CONTINUE }
  | "default"                       { ret_token DEFAULT }
  | "defer"                         { ret_token DEFER }
  | "else"                          { ret_token ELSE }
  | "fallthrough"                   { ret_token FALLTHROUGH }
  | "for"                           { ret_token FOR }
  | "func"                          { ret_token FUNC }
  | "go"                            { ret_token GO }
  | "goto"                          { ret_token GOTO }
  | "if"                            { ret_token IF }
  | "import"                        { ret_token IMPORT }
  | "interface"                     { ret_token INTERFACE }
  | "map"                           { ret_token MAP }
  | "package"                       { ret_token PACKAGE }
  | "range"                         { ret_token RANGE }
  | "return"                        { ret_token RETURN }
  | "select"                        { ret_token SELECT }
  | "struct"                        { ret_token STRUCT }
  | "switch"                        { ret_token SWITCH }
  | "type"                          { ret_token TYPE }
  | "var"                           { ret_token VAR }
  (* GoLite keywords *)
  | "int"                           { ret_token INT }
  | "float64"                       { ret_token FLOAT64 }
  | "bool"                          { ret_token BOOL }
  | "rune"                          { ret_token RUNE }
  | "string"                        { ret_token STRING }
  | "print"                         { ret_token PRINT }
  | "println"                       { ret_token PRINTLN }
  | "append"                        { ret_token APPEND }
  (* operators *)
  | '+'                             { ret_token PLUS }
  | '-'                             { ret_token MINUS }
  | '*'                             { ret_token TIMES }
  | '/'                             { ret_token DIV }
  | '%'                             { ret_token MOD }
  | '&'                             { ret_token BITAND }
  | '|'                             { ret_token BITOR }
  | '^'                             { ret_token BITXOR }
  | "<<"                            { ret_token LSHIFT }
  | ">>"                            { ret_token RSHIFT }
  | "&^"                            { ret_token BITCLR }
  | "+="                            { ret_token PLUSEQ }
  | "-="                            { ret_token MINUSEQ }
  | "*="                            { ret_token TIMESEQ }
  | "/="                            { ret_token DIVEQ }
  | "%="                            { ret_token MODEQ }
  | "&="                            { ret_token ANDEQ }
  | "|="                            { ret_token OREQ }
  | "^="                            { ret_token XOREQ }
  | "<<="                           { ret_token LSHIFTEQ }
  | ">>="                           { ret_token RSHIFTEQ }
  | "&^="                           { ret_token CLREQ }
  | "&&"                            { ret_token LAND }
  | "||"                            { ret_token LOR }
  | "<-"                            { ret_token RCV }
  | "++"                            { ret_token INC }
  | "--"                            { ret_token DEC }
  | "=="                            { ret_token CMPEQ }
  | '<'                             { ret_token LT }
  | '>'                             { ret_token GT }
  | '='                             { ret_token EQUALS }
  | '!'                             { ret_token LNOT }
  | "!="                            { ret_token NOTEQ }
  | "<="                            { ret_token LTE }
  | ">="                            { ret_token GTE }
  | ":="                            { ret_token DECLEQ }
  (* delimiters *)
  | "..."                           { ret_token VARPARAM }
  | '('                             { ret_token LPAREN }
  | ')'                             { ret_token RPAREN }
  | '['                             { ret_token LBRCKT }
  | ']'                             { ret_token RBRCKT }
  | '{'                             { ret_token LBRACE }
  | '}'                             { ret_token RBRACE }
  | ','                             { ret_token COMMA }
  | ';'                             { ret_token SEMICOLON }
  | '.'                             { ret_token DOT }
  | ':'                             { ret_token COLON }
  (* identifiers *)
  | ident as str                    { ret_token (ID str) }
  (* end of file and unknown characters *)
  | eof                             { if need_semicolon () then (last_token_on_line := None; SEMICOLON) else (last_token_on_line := None; EOF) }
  | _ as c                          { raise (LexerError ("Line " ^ string_of_int lexbuf.lex_curr_p.pos_lnum
                                                                 ^ ": unexpected char " ^ Char.escaped c)) }
