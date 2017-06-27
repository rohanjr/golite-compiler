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

open Printf
open Lexing
open Pretty
module E = Error

type pretty_print_mode = No_pp | PP_no_types | PP_with_types

type output_mode = {
  pp_mode : pretty_print_mode;
  symtbl_dump : bool;
  c_gen : bool;
}

let default_output_mode = {
  pp_mode = PP_no_types;
  symtbl_dump = false;
  c_gen = true;
}

type info_mode = Help | Version

type op_mode = Info of info_mode | Compile of string option * output_mode

let process_arg (mode : op_mode) (arg : string) : op_mode =
  if arg.[0] = '-' then
    match mode, arg with
    | _, "-h" | _, "--help" -> Info Help
    | _, "-v" | _, "--version" -> Info Version
    | Compile (fname, out_mode), "-nopretty" ->
        Compile (fname, {out_mode with pp_mode = No_pp})
    | Compile (fname, out_mode), "-pptype" ->
        Compile (fname, {out_mode with pp_mode = PP_with_types})
    | Compile (fname, out_mode), "-dumpsymtab" ->
        Compile (fname, {out_mode with symtbl_dump = false})
    | Compile (fname, out_mode), "-nocompile" ->
        Compile (fname, {out_mode with c_gen = false})
    | _ -> failwith "Invalid combination of flags"
  else
    match mode with
    | Info _ -> mode
    | Compile (None, out_mode) -> Compile (Some arg, out_mode)
    | Compile (Some _, _) -> failwith "More than one input file name given"

let get_op_mode (args: string array) : op_mode =
  (* Ignore first argument which is the executable name *)
  let args' = Array.sub args 1 (Array.length args - 1) in
  let default_op_mode = Compile (None, default_output_mode) in
  Array.fold_left process_arg default_op_mode args'

let () = try
    match get_op_mode Sys.argv with
    | Info Help -> fprintf stdout 
                     "Name: Group15 GoLite -> C Compiler\nSynopsis: ./main.native [option]... [file]\nDescription: Compile GoLite code into C11.\n-dumpsymtab : Dump the symbol table while compiling.\n-pptype : Pretty print with types.\n-nopretty : Suppress the creation of a pretty printed file.\n-nocompile : Suppress the compilation (for debug).\n--help, -h : Help.\n--version, -v : Compiler version.\n" 
    | Info Version -> fprintf stdout "Group15 GoLite v1 -> C11 Compiler v1\n"
    | Compile (None, _) -> failwith "No input file to compile"
    | Compile (Some fname, out_mode) -> begin
        Check.SymTbl.filename := fname;
        if out_mode.symtbl_dump then Check.SymTbl.clear_file ();
        let lexbuf = Lexing.from_channel (open_in fname) in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = fname};
        let p = Parser.prog Lexer.token lexbuf in 
        let weeded_one = Weeder.ForSwitchWeeding.weed p in
        (if out_mode.pp_mode = PP_no_types then
          let oc = open_out (String.sub fname 0 (String.rindex fname '.') ^ ".pretty.go") in
          fprintf oc "%s" (Pretty.pretty weeded_one));
        Check.check_prog weeded_one;
        let weeded_two = Weeder.ReturnWeeding.weed weeded_one; weeded_one in
        (if out_mode.pp_mode = PP_with_types then
           let oc' = open_out (String.sub fname 0 (String.rindex fname '.') ^ ".pptype.go") in
           fprintf oc' "%s" (Pretty.pretty weeded_two));
        (if out_mode.c_gen then
           let oc' = open_out (String.sub fname 0 (String.rindex fname '.') ^ ".c") in
           fprintf oc' "%s" (Codegen.gen_prog weeded_two));
        flush stdout
      end
  with
  | Lexer.LexerError msg -> fprintf stderr "Lexer error: %s\n" msg
  | Parser.Error -> fprintf stderr "Syntax error.\n"
  | E.ParsingError (s, p) ->
      let string_of_pos pos = sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
      fprintf stderr "%s\n" (string_of_pos p ^ ": error: " ^ s)
  | E.Error s -> fprintf stderr "Error: %s\n" s
  | Weeder.WeedError s -> fprintf stderr "error: %s\n" s
  | Check.TypeError s -> fprintf stderr "Type error: %s\n" s
  | Check.DeclError s -> fprintf stderr "Decl error: %s\n" s
  | Check.InternalError s -> fprintf stderr "Internal error: %s\n" s
