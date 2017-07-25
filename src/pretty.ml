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

(* TODO Change order of parameters in pretty functions:
  * better to have level before node to print
  * Also use concatmap instead of pattern matching everywhere.
  * And using "nat" is kind of strange. *)

open Core.Std

exception Problem of string
open Ast

type nat =
  | Zero
  | Succ of nat

let rec indent : nat -> string = function
  | Zero -> ""
  | Succ level -> "   " ^ indent level

let pretty_option ~(f : 'a -> string) : 'a option -> string = Option.value_map ~default:"" ~f

let pretty_varlist : id list -> string = function
  | [] -> raise (Problem "Varlists should be non-empty.")
  | l -> String.concat ~sep:", " l

let rec pretty_tp ?(level=Zero) : tp -> string = function
  | Void -> "void"
  | Int -> "int"
  | Float64 -> "float64"
  | Bool -> "bool"
  | Rune -> "rune"
  | String -> "string"
  | FuncTp (tl, rt) ->
    "(" ^ begin match tl with 
    | [] -> "void"
    | _ -> String.concat ~sep:" * " (List.map ~f:(pretty_tp ~level) tl) end
    ^ ") -> " ^ pretty_tp rt ~level
  | TypeVar (id, _) -> id
  | TSlice t -> "[]" ^ pretty_tp t ~level
  | TArray (size, t) -> "[" ^ string_of_int size ^ "]" ^ pretty_tp t ~level
  | TStruct vssl -> "struct {\n" ^ pretty_struct vssl ~level:(Succ level) ^ indent level ^ "}"

and pretty_struct ~level (vssl : varspecsimp list) : string =
  String.concat (List.map ~f:(fun vss -> indent level ^ pretty_varspecsimp vss ~level ^ "\n") vssl)

and pretty_varspecsimp ~level : varspecsimp -> string = function
  | (varlist, tp) -> pretty_varlist varlist ^ " " ^ pretty_tp tp ~level

let pretty_tp_option ~level : tp option -> string = pretty_option ~f:(fun t -> pretty_tp t ~level ^ " ")

let pretty_comment_tp : tp option -> string = pretty_option ~f:(fun t -> " /* : " ^ pretty_tp t ^ "*/ ")

let pretty_varspecsimp_list ~level (vssl : varspecsimp list) : string =
  String.concat ~sep:", " (List.map ~f:(pretty_varspecsimp ~level) vssl)

let pretty_binop : binop -> string = function
  | LOr -> "||"
  | LAnd -> "&&"
  | CmpEq -> "=="
  | NotEq -> "!="
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | BitOr -> "|"
  | BitXOr -> "^"
  | Times -> "*"
  | Div -> "/"
  | Mod -> "%"
  | BitAnd -> "&"
  | BitClr -> "&^"
  | LShift -> "<<"
  | RShift -> ">>"

let pretty_uop : uop -> string = function
  | UPlus -> "+"
  | UMinus -> "-"
  | LNot -> "!"
  | UBitXor -> "^"

let rec pretty_expr ?(level=Zero) : expr -> string = function
  | Unary (_, u, tpor) -> pretty_unary u level
  | Binary (_, op, e1, e2, tpor) ->
    pretty_expr ~level e1 ^ " " ^ pretty_binop op ^ " " ^
    pretty_expr ~level e2 ^ pretty_comment_tp !tpor

and pretty_expr_option ~level : expr option -> string = pretty_option ~f:(pretty_expr ~level)

and pretty_exprlist ~level (el: expr list) : string =
  String.concat ~sep:", " (List.map ~f:(pretty_expr ~level) el)

and pretty_unary (u : unaryexpr) level = match u with
  | Primary (_, prim, tpor) -> pretty_primary prim level
  | UnaryOp (_, op, u, tpor) ->
    pretty_uop op ^ pretty_unary u level ^ pretty_comment_tp !tpor

and pretty_primary (prim : primaryexpr) level = match prim with
  | Operand (_, oper, tpor) -> pretty_operand oper level
  | Sel (_, prim, id, tpor) -> pretty_primary prim level ^ "." ^ id ^ pretty_comment_tp !tpor
  | ArrAccess (_, prim, e, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr ~level e ^ "]" ^ pretty_comment_tp !tpor
  | Slice (_, prim, e1op, e2op, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option ~level e1op ^ " : " ^ pretty_expr_option ~level e2op ^ "]" ^ pretty_comment_tp !tpor
  | SliceCap (_, prim, eop, e1, e2, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option ~level eop ^ " : " ^ pretty_expr ~level e1 ^ " : " ^ pretty_expr ~level e2 ^ "]" ^ pretty_comment_tp !tpor
  | FunApp (_, prim, el, tpor) -> pretty_primary prim level ^ "(" ^ pretty_exprlist ~level el ^ ")" ^ pretty_comment_tp !tpor
  | Append (_, x, e, tpor) -> "append" ^ "(" ^ x ^ ", " ^ pretty_expr ~level e ^ ")" ^ pretty_comment_tp !tpor
  | Cast (_, tp, e, tpor) -> pretty_tp tp ~level ^ "(" ^ pretty_expr ~level e ^ ")" ^ pretty_comment_tp !tpor

and pretty_operand (oper : operand) level = match oper with
  | Parens (_, e, tpor) -> "(" ^ pretty_expr ~level e ^ ")"  ^ pretty_comment_tp !tpor
  | Var (_, id, tpor) -> id ^ pretty_comment_tp !tpor
  | IntLit (_, i, tpor) -> string_of_int i ^ pretty_comment_tp !tpor
  | FloatLit (_, f, tpor) -> string_of_float f ^ pretty_comment_tp !tpor
  | RuneLit (_, r, tpor) -> r ^ pretty_comment_tp !tpor
  | StrLit (_, s, tpor) -> s ^ pretty_comment_tp !tpor

let pretty_lvalue ~level : lvalue -> string = function
  | LSel (_, prim, id, tpor) -> pretty_primary prim level ^ "." ^ id ^ pretty_comment_tp !tpor
  | LArrAccess (_, prim, e, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr ~level e ^ "]" ^ pretty_comment_tp !tpor
  | LSlice (_, prim, eop1, eop2, tpor) ->
    pretty_primary prim level ^ "[" ^ pretty_expr_option ~level eop1 ^ " : " ^ pretty_expr_option ~level eop2 ^ "]" ^ pretty_comment_tp !tpor
  | LSliceCap (_, prim, eop, e1, e2, tpor) ->
    pretty_primary prim level ^ "[" ^ pretty_expr_option ~level eop ^ " : " ^ pretty_expr ~level e1 ^ " : " ^ pretty_expr ~level e2 ^ "]" ^ pretty_comment_tp !tpor

let pretty_lvaluelist ~level : lvalue list -> string = function
  | [] -> raise (Problem " Lvaluelist should not be empty.")
  | l -> String.concat ~sep:", " (List.map ~f:(pretty_lvalue ~level) l)

let pretty_assignop : assignop -> string = function
  | PlusEq -> "+="
  | MinusEq -> "-="
  | TimesEq -> "*="
  | DivEq -> "/="
  | ModEq -> "%="
  | AndEq -> "&="
  | OrEq -> "|="
  | XOrEq -> "^="
  | LShiftEq -> "<<="
  | RShiftEq -> ">>="
  | ClrEq -> "&^="

let pretty_simplestmt ~level : simplestmt -> string = function
  | Expr (_, e) -> pretty_expr ~level e
  | Inc (_, e) -> pretty_expr ~level e ^ "++"
  | Dec (_, e) -> pretty_expr ~level e ^ "--"
  | AssignEquals (_, lvl, el) -> pretty_lvaluelist ~level lvl ^ " = " ^ pretty_exprlist ~level el
  | Assign (_, assop, lv, e) -> pretty_lvalue ~level lv ^ " " ^ pretty_assignop assop ^ " " ^ pretty_expr ~level e
  | AssignVarEquals (_, vl, el) -> pretty_varlist vl ^ " = " ^ pretty_exprlist ~level el
  | AssignVar (_, assop, v, e) -> v ^ " " ^ pretty_assignop assop ^ " " ^ pretty_expr ~level e
  | ShortVarDecl (_, idlist, el, dlor) -> pretty_varlist idlist ^ " := " ^ pretty_exprlist ~level el

let pretty_varspec ~level : varspec -> string = function
  | VarSpecTp (_, vss, elo) -> pretty_varspecsimp ~level vss ^ pretty_option ~f:(fun el -> " = " ^ pretty_exprlist ~level el) elo ^ "\n"
  | VarSpecNoTp (_, idl, el) -> pretty_varlist idl ^ " = " ^ pretty_exprlist ~level el ^ "\n"

let pretty_typespec ~level : typespec -> string = function
  | TpSpec (_, t, tp) -> t ^ " " ^ pretty_tp ~level tp ^ "\n"

let pretty_varspecsemi_list (vsl : varspec list) level =
  String.concat (List.map ~f:(fun vs -> indent level ^ pretty_varspec ~level vs) vsl)

let pretty_typespecsemi_list (tsl : typespec list) level =
  String.concat (List.map ~f:(fun vs -> indent level ^ pretty_typespec ~level vs) tsl)

let pretty_declstmt ~level : declstmt -> string = function
  | VarDecls (_, [varspec]) -> "var " ^ pretty_varspec ~level varspec
  | TypeDecls (_, [typespec]) -> "type " ^ pretty_typespec ~level typespec
  | VarDecls (_, vslist) -> "var (\n"  ^ pretty_varspecsemi_list vslist (Succ level) ^ indent level ^ ")\n"
  | TypeDecls (_, tsl) -> "type ( \n" ^ pretty_typespecsemi_list tsl (Succ level) ^ indent level ^ ")\n"

let pretty_ifcond ~level : ifcond -> string = function
  | IfCond (_, sso, e) -> pretty_option ~f:(fun ss -> pretty_simplestmt ~level ss ^ "; ") sso ^ pretty_expr ~level e

let pretty_switch_cond ~level : switchcond -> string = function
  | SwitchCond (_, None, None) -> (* TODO handle this case: isn't it legal? *)
    raise (Problem "Switch condition lacks both statement and expression")
  | SwitchCond (_, None, eo) -> pretty_expr_option ~level eo
  | SwitchCond (_, Some stmt, eo) -> pretty_simplestmt ~level stmt ^ " ; " ^ pretty_expr_option ~level eo

let pretty_exprswitchcase ~level : exprswitchcase -> string = function
  | Default _ -> indent level ^  "default"
  | Case (_, el) -> indent level ^ "case " ^ pretty_exprlist ~level el

let pretty_printstmt ~level : printstmt -> string = function
  | PrintStmt (_, elo) -> "print(" ^ pretty_option ~f:(pretty_exprlist ~level) elo ^ ")\n"
  | PrintlnStmt (_, elo) -> "println(" ^ pretty_option ~f:(pretty_exprlist ~level) elo ^ ")\n"

let rec pretty_stmt ~level : stmt -> string = function
  | Decl (_, declstmt) -> pretty_declstmt ~level declstmt
  | Simple (_, simplestmt) -> pretty_simplestmt ~level simplestmt ^ "\n"
  | Return (_, eo) -> "return" ^ pretty_option ~f:(fun e -> " " ^ pretty_expr ~level e) eo ^ "\n"
  | Break _ -> "break\n"
  | Continue _ -> "continue\n"
  | Block (_, sl) -> pretty_block ~level sl ^ "\n"
  | If (_, ifstmt) -> "if " ^ pretty_ifstmt ~level ifstmt
  | Switch (_, switchstmt) -> pretty_switchstmt ~level switchstmt
  | For (_, forstmt) -> "for " ^ pretty_forstmt ~level forstmt
  | Print (_, printstmt) -> pretty_printstmt ~level printstmt

and pretty_stmtlist ~level (sl: stmt list) : string =
  String.concat (List.map ~f:(fun s -> indent level ^ pretty_stmt ~level s) sl)

and pretty_block ~level (sl: stmt list) : string =
  "{\n" ^ pretty_stmtlist ~level:(Succ level) sl ^ indent level ^ "}"

and pretty_ifstmt ~level : ifstmt -> string = function
  | IfOnly (p, ifcond, sl) -> pretty_ifcond ~level ifcond ^ " " ^ pretty_block ~level sl ^ "\n"
  | IfElse (p, ifcond, b1, b2) ->
    pretty_ifcond ~level ifcond ^ " " ^ pretty_block ~level b1 ^
    " else " ^ pretty_block ~level b2 ^ "\n"
  | IfElseIf (p, ifcond, block, ifstmt) ->
    pretty_ifcond ~level ifcond ^ " " ^ pretty_block ~level block ^
    " else if " ^ pretty_ifstmt ~level ifstmt

and pretty_switchstmt ~level : switchstmt -> string = function
  | SwitchStmt (_, sco, eccl) ->
    "switch " ^ pretty_option ~f:(pretty_switch_cond ~level) sco ^ "{\n" ^
    String.concat (List.map ~f:(pretty_exprcaseclause ~level:(Succ level)) eccl) ^
    indent level ^ "}\n"

and pretty_exprcaseclause ~level : exprcaseclause -> string = function
  | ExprCaseClause (_, esc, stmtlist) ->
    pretty_exprswitchcase ~level esc ^ " : " ^ pretty_stmtlist ~level stmtlist

and pretty_forstmt ~level : forstmt -> string = function
  | InfLoop (p, stmtlist) -> pretty_block ~level stmtlist ^ "\n"
  | WhileLoop (p, e, stmtlist) -> pretty_expr ~level e ^ " " ^ pretty_block ~level stmtlist ^ "\n"
  | ForLoop (p, s1, eo, s2, stmtlist) ->
    pretty_option ~f:(pretty_simplestmt ~level) s1 ^ "; " ^
    pretty_expr_option ~level eo ^ "; " ^
    pretty_option ~f:(pretty_simplestmt ~level) s2 ^ pretty_block ~level stmtlist ^ "\n"

let pretty_package : package -> string = function
  | Package (_, name) -> "package " ^ name ^ "\n\n"

let pretty_topleveldecl ~level : topleveldecl -> string = function
  | FuncDecl (p, i, vsslo, tpo, sl) ->
    "func " ^ i ^ "(" ^ pretty_option ~f:(pretty_varspecsimp_list ~level) vsslo ^ ") " ^
    pretty_tp_option ~level tpo ^ pretty_block ~level sl ^ "\n"
  | Stmt (_, stmt) -> pretty_stmt ~level stmt

let pretty_prog : prog -> string = function
  | Prog (_, package, tldl) ->
    pretty_package package ^
    String.concat (List.map ~f:(pretty_topleveldecl ~level:Zero) tldl)
