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

%{

open Ast
module E = Error

%}

(* literals *)
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> RUNELIT
%token <string> STRLIT

(* identifiers *)
%token <string> ID

(* Go keywords *)
%token BREAK
%token CASE
%token CHAN
%token CONST
%token CONTINUE
%token DEFAULT
%token DEFER
%token ELSE
%token FALLTHROUGH
%token FOR
%token FUNC
%token GO
%token GOTO
%token IF
%token IMPORT
%token INTERFACE
%token MAP
%token PACKAGE
%token RANGE
%token RETURN
%token SELECT
%token STRUCT
%token SWITCH
%token TYPE
%token VAR

(* GoLite keywords *)
%token INT
%token FLOAT64
%token BOOL
%token RUNE
%token STRING
%token PRINT
%token PRINTLN
%token APPEND

%token LOR
%token LAND
%token CMPEQ NOTEQ LT GT LTE GTE
%token PLUS MINUS BITOR BITXOR
%token TIMES DIV MOD BITAND BITCLR LSHIFT RSHIFT
%token LNOT

(* operator statements *)
%token PLUSEQ
%token MINUSEQ
%token TIMESEQ
%token DIVEQ
%token MODEQ
%token ANDEQ
%token OREQ
%token XOREQ
%token LSHIFTEQ
%token RSHIFTEQ
%token CLREQ
%token RCV
%token INC
%token DEC
%token EQUALS
%token DECLEQ

(* delimiters *)
%token VARPARAM
%token LPAREN
%token RPAREN
%token LBRCKT
%token RBRCKT
%token LBRACE
%token RBRACE
%token COMMA
%token SEMICOLON
%token DOT
%token COLON

(* end of file *)
%token EOF

(* operators - lowest to highest precedence *)
%left LOR
%left LAND
%left CMPEQ NOTEQ LT GT LTE GTE
%left PLUS MINUS BITOR BITXOR
%left TIMES DIV MOD BITAND BITCLR LSHIFT RSHIFT
%nonassoc UPLUS UMINUS LNOT UBITXOR

%start prog
%type <Ast.prog> prog

%%

prog:
  | p = packagedecl; td = topleveldecl*; EOF { Prog ($startpos(p), p, td) }
  | p = packagedecl; error { raise (E.ParsingError ("expected declaration", $endpos(p))) } 
  | err = error { raise (E.ParsingError ("program must start with package clause", $startpos(err))) }

packagedecl:
  | p = PACKAGE; i = ID; SEMICOLON { Package ($startpos(p), i) }
  | p = PACKAGE; error { raise (E.ParsingError ("package name mmust be an identifier", $endpos(p))) }

topleveldecl:
  | f = funcdecl; SEMICOLON { f }
  | s = stmt { Stmt ($startpos(s), s) }

stmt:
  | s = stmtbody; SEMICOLON { s }

stmtbody:
  | s = simplestmt { Simple ($startpos(s), s) }
  | s = blockstmt { Block ($startpos(s), s) }
  | s = declstmt { Decl ($startpos(s), s) }  
  | s = ifstmt { If ($startpos(s), s) }
  | s = switchstmt { Switch ($startpos(s), s) }
  | s = forstmt { For ($startpos(s), s) }
  | s = printstmt { Print ($startpos(s), s) }  
  | b = BREAK { Break $startpos(b) }
  | c = CONTINUE { Continue $startpos(c) }
  | r = RETURN; e = expr? { Return ($startpos(r), e) }

simplestmt:
  | e = expr { Expr ($startpos(e), e) }
  | e = expr; INC { Inc ($startpos(e), e) }
  | e = expr; DEC { Dec ($startpos(e), e) }
  | lvs = separated_nonempty_list(COMMA, lvalue); op = EQUALS; es = exprlist {if (List.length lvs = List.length es) then AssignEquals ($startpos(lvs), lvs, es) 
									 else raise (E.ParsingError ("number of variables does not match number of values", $endpos(op)))}
  | lv = lvalue; op = assignop; e = expr {Assign ($startpos(lv), op, lv, e)}
  | vs = varlist; op = EQUALS; es = exprlist {if (List.length vs = List.length es) then AssignVarEquals ($startpos(vs), vs, es) 
					      else raise (E.ParsingError ("number of variables does not match number of values", $endpos(op)))}
  | v = ID; op = assignop; e = expr { AssignVar ($startpos(v), op, v, e) }
  | vs = varlist; op = DECLEQ; es = exprlist 
    { if (List.length vs) = (List.length es) then ShortVarDecl ($startpos(vs), vs, es, ref None)
      else raise (E.ParsingError ("wrong number of initializations", $startpos(op))) }
  | lvs = separated_nonempty_list(COMMA, lvalue); op = assignop; error  {raise (E.ParsingError ("expected operand", $endpos(op)))}
  | v = varlist; op = assignop; error  {raise (E.ParsingError ("expected operand", $endpos(op)))}
  | v = varlist; op = DECLEQ; error  {raise (E.ParsingError ("expected operand", $endpos(op)))}

blockstmt: 
  | LBRACE; s = stmt*; RBRACE { s }
  | LBRACE; s = stmt*; error { raise (E.ParsingError ("expected \"}\"", $endpos(s))) }

declstmt:
  | v = vardecl { v }
  | t = typedecl { t }

vardecl:
  | var = VAR; v = varspec { VarDecls ($startpos(var), [v]) }
  | var = VAR; LPAREN ; vs = varspecsemi*; RPAREN { VarDecls ($startpos(var), vs) }
  | VAR; LPAREN ; v = varspecsemi*; error { raise (E.ParsingError ("missing \")\"", $endpos(v))) }

varspecsemi:
  | v = varspec; SEMICOLON {v}

varspec:  (* Note that we have to ensure vs and es have the same size *)
  | v = varspecsimp { VarSpecTp ($startpos(v), v, None) }
  | v = varspecsimp; op = EQUALS; es = exprlist
    { let (vs,_) = v in
      if (List.length vs) = (List.length es) then VarSpecTp ($startpos(v), v, Some(es)) 
      else raise (E.ParsingError ("wrong number of initializations", $startpos(op))) }
  | vs = varlist; op = EQUALS; es = exprlist
    { if (List.length vs) = (List.length es) then VarSpecNoTp ($startpos(vs), vs, es) 
      else raise (E.ParsingError ("wrong number of initializations", $startpos(op))) }
  | v = varspecsimp; op = EQUALS; error {raise (E.ParsingError ("expected operand", $endpos(op)))}
  | vs = varlist; op = EQUALS; error {raise (E.ParsingError ("expected operand", $endpos(op)))}

varspecsimp:
  | vs = varlist; tp = tp { (vs, tp) }

varlist:
  vs = separated_nonempty_list(COMMA, ID) { vs }

typedecl:
  | tp = TYPE; ts = typespec { TypeDecls ($startpos(tp), [ts]) }
  | tp = TYPE; LPAREN; tsl = typespecsemi*; RPAREN { TypeDecls ($startpos(tp), tsl) }
  | TYPE; LPAREN; ts = typespec; error { raise (E.ParsingError ("missing \")\"", $endpos(ts))) }
  | TYPE; lp = LPAREN; error { raise (E.ParsingError ("missing \")\"", $endpos(lp))) }

typespecsemi:
  | ts = typespec; SEMICOLON {ts}

typespec:
  | t = ID; tp = tp { TpSpec ($startpos(t), t, tp) }

varspecsimpsemi:
  | vs = varspecsimp; SEMICOLON {vs}

tp:
  | b = basetp { b }
  | lb = LBRCKT; RBRCKT; t = tp { TSlice t }
  | lb = LBRCKT; i = INTLIT; RBRCKT; t = tp { TArray (i, t) }
  | st = STRUCT; LBRACE; vs = varspecsimpsemi*; RBRACE {TStruct vs}
  | t = ID { TypeVar (t, ref None) }
  | STRUCT; LBRACE; vs = varspecsimpsemi*; error { raise (E.ParsingError ("expected \"}\"", $endpos(vs))) }

basetp:
  | i = INT { Int }
  | f = FLOAT64 { Float64 }
  | b = BOOL { Bool }
  | r = RUNE { Rune }
  | s = STRING { String }

funcdecl:
  | f = FUNC; i = ID; LPAREN; p = separated_nonempty_list(COMMA, varspecsimp)?; RPAREN; t = tp?; b = blockstmt { FuncDecl ($startpos(f), i, p, t, b) }
  | FUNC; i = ID; LPAREN; p = separated_nonempty_list(COMMA, varspecsimp)?; error { raise (E.ParsingError ("expected \")\"", $endpos(p))) }
  | FUNC; i = ID; error { raise (E.ParsingError ("expected \"(\"", $endpos(i))) }
  | f = FUNC; error { raise (E.ParsingError ("expected function name", $endpos(f))) }

ifstmt:
  | iftok = IF; ic = ifcond; b = blockstmt { IfOnly ($startpos(iftok), ic, b) }
  | iftok = IF; ic = ifcond; b1 = blockstmt; ELSE; b2 = blockstmt { IfElse ($startpos(iftok), ic, b1, b2) }
  | iftok = IF; ic = ifcond; b = blockstmt; ELSE; is = ifstmt { IfElseIf ($startpos(iftok), ic, b, is) }

ifcond:
  | e = expr { IfCond ($startpos(e), None, e) }
  | s = simplestmt?; SEMICOLON; e = expr { IfCond ($startpos(s), s, e) }
  | err = error { raise (E.ParsingError ("missing condition in if statement", $startpos(err)))}

switchstmt:
  sw = SWITCH; sco = switchcond?; LBRACE; ecc = exprcaseclause*; RBRACE { SwitchStmt ($startpos(sw), sco, ecc) }

switchcond:
  | e = expr { SwitchCond ($startpos(e), None, Some e) }
  | s = simplestmt; SEMICOLON { SwitchCond ($startpos(s), Some s, None) }
  | s = simplestmt; SEMICOLON; e = expr { SwitchCond ($startpos(s), Some s, Some e) }
  | s = simplestmt; error { raise (E.ParsingError("expected \';\' after statement in switch expression", $endpos(s)))}

exprcaseclause:
  esc = exprswitchcase; COLON; s = stmt* { ExprCaseClause ($startpos(esc), esc, s) }

exprswitchcase:
  | c = CASE; e = exprlist { Case ($startpos(c), e) }
  | d = DEFAULT { Default $startpos(d) }

forstmt:
  | f = FOR; b = blockstmt { InfLoop ($startpos(f), b) } (* Infinite *)
  | f = FOR; e = expr; b = blockstmt { WhileLoop ($startpos(f), e, b) } (* While *)
  | f = FOR; s1 = simplestmt?; SEMICOLON; e = expr?; SEMICOLON;
         s2 = simplestmt?; b = blockstmt { ForLoop ($startpos(f), s1, e, s2, b) } (* Classic *)
  | FOR; s1 = simplestmt?; SEMICOLON; e = expr?; err = error { raise (E.ParsingError("expected semicolon", $endpos(e)))}
  | FOR; s1 = simplestmt?; err = error { raise (E.ParsingError(" parse error in for statement", $endpos(s1)))}


printstmt:
  | p = PRINT; LPAREN; e = exprlist?; RPAREN { PrintStmt ($startpos(p), e) }
  | p = PRINTLN; LPAREN; e = exprlist?; RPAREN { PrintlnStmt ($startpos(p), e) }
  | PRINT; LPAREN; e = exprlist?; error { raise (E.ParsingError("expected \")\"", $endpos(e))) }
  | PRINTLN; LPAREN; e = exprlist?; error { raise (E.ParsingError("expected \")\"", $endpos(e))) }
  | p = PRINT; error { raise (E.ParsingError("expected \"(\"", $endpos(p))) }
  | p = PRINTLN; error { raise (E.ParsingError("expected \"(\"", $endpos(p))) }  

expr:
  | u = unaryexpr { Unary ($startpos(u), u, ref None) }
  (* Have to write each binop explicitly or the precedence directives don't kick in *)
  | e1 = expr; LOR; e2 = expr { Binary ($startpos(e1), LOr, e1, e2, ref None) }
  | e1 = expr; LAND; e2 = expr { Binary ($startpos(e1), LAnd, e1, e2, ref None) }
  | e1 = expr; CMPEQ; e2 = expr { Binary ($startpos(e1), CmpEq, e1, e2, ref None) }
  | e1 = expr; NOTEQ; e2 = expr { Binary ($startpos(e1), NotEq, e1, e2, ref None) }
  | e1 = expr; LT; e2 = expr { Binary ($startpos(e1), LT, e1, e2, ref None) }
  | e1 = expr; GT; e2 = expr { Binary ($startpos(e1), GT, e1, e2, ref None) }
  | e1 = expr; LTE; e2 = expr { Binary ($startpos(e1), LTE, e1, e2, ref None) }
  | e1 = expr; GTE; e2 = expr { Binary ($startpos(e1), GTE, e1, e2, ref None) }
  | e1 = expr; PLUS; e2 = expr { Binary ($startpos(e1), Plus, e1, e2, ref None) }
  | e1 = expr; MINUS; e2 = expr { Binary ($startpos(e1), Minus, e1, e2, ref None) }
  | e1 = expr; BITOR; e2 = expr { Binary ($startpos(e1), BitOr, e1, e2, ref None) }
  | e1 = expr; BITXOR; e2 = expr { Binary ($startpos(e1), BitXOr, e1, e2, ref None) }
  | e1 = expr; TIMES; e2 = expr { Binary ($startpos(e1), Times, e1, e2, ref None) }
  | e1 = expr; DIV; e2 = expr { Binary ($startpos(e1), Div, e1, e2, ref None) }
  | e1 = expr; MOD; e2 = expr { Binary ($startpos(e1), Mod, e1, e2, ref None) }
  | e1 = expr; BITAND; e2 = expr { Binary ($startpos(e1), BitAnd, e1, e2, ref None) }
  | e1 = expr; BITCLR; e2 = expr { Binary ($startpos(e1), BitClr, e1, e2, ref None) }
  | e1 = expr; LSHIFT; e2 = expr { Binary ($startpos(e1), LShift, e1, e2, ref None) }
  | e1 = expr; RSHIFT; e2 = expr { Binary ($startpos(e1), RShift, e1, e2, ref None) }

unaryexpr:
  | p = primaryexpr { Primary ($startpos(p), p, ref None) }
  | op = unaryop; u = unaryexpr { UnaryOp ($startpos(op), op, u, ref None) } 

primaryexpr: (* TODO add error cases for append/casting for invalid args *)
  | o = operand { Operand ($startpos(o), o, ref None) }
  | p = primaryexpr; DOT; x = ID { Sel ($startpos(p), p, x, ref None) } (* selection *)
  | p = primaryexpr; LBRCKT; e = expr; RBRCKT { ArrAccess ($startpos(p), p, e, ref None) } (* array access *)
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr?; RBRCKT { Slice($startpos(p), p, e1, e2, ref None) } (* slice *)
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr; COLON; e3 = expr; RBRCKT { SliceCap($startpos(p), p, e1, e2, e3, ref None) } (* slice *)
  | p = primaryexpr; LPAREN; el = exprlist?; RPAREN { FunApp($startpos(p), p, Util.oget [] el, ref None) } (* Functions *) 
  | a = APPEND; LPAREN; i = ID; COMMA; e = expr; RPAREN { Append($startpos(a), i, e, ref None) } (* Append *)
  | t = basetp; LPAREN; e = expr; RPAREN { Cast($startpos(t), t, e, ref None) } (* Type casting *)
  | p = primaryexpr; d = DOT; error { raise (E.ParsingError("expected identifier", $endpos(d))) }
  | p = primaryexpr; LBRCKT; e = expr; error { raise (E.ParsingError ("missing \']\'", $endpos(e))) }
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr?; error { raise (E.ParsingError ("missing \']\'", $endpos(e2))) }
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr; COLON; e3 = expr; error { raise (E.ParsingError ("missing \']\'", $endpos(e3))) }
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr; c = COLON; error { raise (E.ParsingError ("expected operand", $endpos(c))) }  
  | APPEND; LPAREN; i = ID; COMMA; e = expr; error { raise (E.ParsingError("expected \")\"", $endpos(e))) }
  | t = basetp; LPAREN; e = expr; error { raise (E.ParsingError("expected \")\"", $endpos(e))) }
  | a = APPEND; error { raise (E.ParsingError("expected \"(\"", $endpos(a))) }
  | t = basetp; error { raise (E.ParsingError("expected \"(\"", $endpos(t))) }


operand:
  | l = LPAREN; e = expr; RPAREN { Parens ($startpos(l), e, ref None) }
  | i = ID { Var ($startpos(i), i, ref None) }
  | i = INTLIT { IntLit ($startpos(i), i, ref None) }
  | f = FLOATLIT { FloatLit ($startpos(f), f, ref None) }
  | r = RUNELIT { RuneLit ($startpos(r), r, ref None) }
  | s = STRLIT { StrLit ($startpos(s), s, ref None) }
  | LPAREN; e = expr; err = error { raise (E.ParsingError ("missing \"(\"", $endpos(e)))}

lvalue: (* TODO Errors here? *)
  | p = primaryexpr; DOT; x = ID { LSel ($startpos(p), p, x, ref None) } (* selection *)
  | p = primaryexpr; LBRCKT; e = expr; RBRCKT { LArrAccess ($startpos(p), p, e, ref None) } (* array access *)
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr?; RBRCKT { LSlice($startpos(p), p, e1, e2, ref None) } (* slice *)
  | p = primaryexpr; LBRCKT; e1 = expr?; COLON; e2 = expr; COLON; e3 = expr; RBRCKT { LSliceCap($startpos(p), p, e1, e2, e3, ref None) } (* slice *)

exprlist:
  es = separated_nonempty_list(COMMA, expr) { es }

unaryop:
  | PLUS    { UPlus }
  | MINUS   { UMinus }
  | LNOT    { LNot }
  | BITXOR  { UBitXor }

(* Can't use this or the precedence rules fail
binop:
  | LOR { LOr }
  | LAND { LAnd }
  | CMPEQ  { CmpEq }
  | NOTEQ  { NotEq }
  | LT  { LT }
  | GT  { GT }
  | LTE  { LTE }
  | GTE { GTE }
  | PLUS  { Plus }
  | MINUS  { Minus }
  | BITOR  { BitOr }
  | BITXOR { BitXOr }
  | TIMES  { Times }
  | DIV  { Div }
  | MOD  { Mod }
  | BITAND  { BitAnd }
  | BITCLR  { BitClr }
  | LSHIFT  { LShift }
  | RSHIFT { RShift }
*)

assignop:
  | PLUSEQ { PlusEq }
  | MINUSEQ { MinusEq }
  | TIMESEQ { TimesEq }
  | DIVEQ { DivEq }
  | MODEQ { ModEq }
  | ANDEQ { AndEq }
  | OREQ { OrEq }
  | XOREQ { XOrEq }
  | LSHIFTEQ { LShiftEq }
  | RSHIFTEQ { RShiftEq }
  | CLREQ { ClrEq }
