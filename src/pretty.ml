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

let pretty_option (f : 'a -> string) : 'a option -> string = Option.value_map ~default:"" ~f

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

let pretty_tp_option ~level : tp option -> string = pretty_option (fun t -> pretty_tp t ~level ^ " ")

let pretty_comment_tp : tp option -> string = pretty_option (fun t -> " /* : " ^ pretty_tp t ^ "*/ ")

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

let rec pretty_expr (e : expr) ~level = match e with
  | Unary (_, u, tpor) -> pretty_unary u level
  | Binary (_, op, e1, e2, tpor) ->
    pretty_expr e1 ~level ^ " " ^ pretty_binop op ^ " " ^ pretty_expr e2 ~level ^ pretty_comment_tp !tpor

and pretty_expr_option (eo : expr option) level =
  pretty_option (pretty_expr ~level) eo

and pretty_exprlist (exprlist: expr list) level =
  String.concat ~sep:", " (List.map exprlist ~f:(pretty_expr ~level))

and pretty_unary (u : unaryexpr) level = match u with
  | Primary (_, prim, tpor) -> pretty_primary prim level
  | UnaryOp (_, op, u, tpor) ->
    pretty_uop op ^ pretty_unary u level ^ pretty_comment_tp !tpor

and pretty_primary (prim : primaryexpr) level = match prim with
  | Operand (_, oper, tpor) -> pretty_operand oper level
  | Sel (_, prim, id, tpor) -> pretty_primary prim level ^ "." ^ id ^ pretty_comment_tp !tpor
  | ArrAccess (_, prim, expr, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr expr ~level ^ "]" ^ pretty_comment_tp !tpor
  | Slice (_, prim, e1op, e2op, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option e1op level ^ " : " ^ pretty_expr_option e2op level ^ "]" ^ pretty_comment_tp !tpor
  | SliceCap (_, prim, eop, e1, e2, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option eop level ^ " : " ^ pretty_expr e1 ~level ^ " : " ^ pretty_expr e2 ~level ^ "]" ^ pretty_comment_tp !tpor
  | FunApp (_, prim, el, tpor) -> pretty_primary prim level ^ "(" ^ pretty_exprlist el level ^ ")" ^ pretty_comment_tp !tpor
  | Append (_, x, expr, tpor) -> "append" ^ "(" ^ x ^ ", " ^ pretty_expr expr ~level ^ ")" ^ pretty_comment_tp !tpor
  | Cast (_, tp, expr, tpor) -> pretty_tp tp ~level ^ "(" ^ pretty_expr expr ~level ^ ")" ^ pretty_comment_tp !tpor

and pretty_operand (oper : operand) level = match oper with
  | Parens (_, expr, tpor) -> "(" ^ pretty_expr expr ~level ^ ")"  ^ pretty_comment_tp !tpor
  | Var (_, id, tpor) -> id ^ pretty_comment_tp !tpor
  | IntLit (_, i, tpor) -> string_of_int i ^ pretty_comment_tp !tpor
  | FloatLit (_, f, tpor) -> string_of_float f ^ pretty_comment_tp !tpor
  | RuneLit (_, r, tpor) -> r ^ pretty_comment_tp !tpor
  | StrLit (_, s, tpor) -> s ^ pretty_comment_tp !tpor

let pretty_lvalue ~level : lvalue -> string = function
  | LSel (_, prim, id, tpor) -> pretty_primary prim level ^ "." ^ id ^ pretty_comment_tp !tpor
  | LArrAccess (_, prim, expr, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr expr level ^ "]" ^ pretty_comment_tp !tpor
  | LSlice (_, prim, eop1, eop2, tpor) ->
    pretty_primary prim level ^ "[" ^ pretty_expr_option eop1 level ^ " : " ^ pretty_expr_option eop2 level ^ "]" ^ pretty_comment_tp !tpor
  | LSliceCap (_, prim, eop, e1, e2, tpor) ->
    pretty_primary prim level ^ "[" ^ pretty_expr_option eop level ^ " : " ^ pretty_expr e1 level ^ " : " ^ pretty_expr e2 level ^ "]" ^ pretty_comment_tp !tpor

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
  | Expr (_, expr) -> pretty_expr expr level
  | Inc (_, expr) -> pretty_expr expr level ^ "++"
  | Dec (_, expr) -> pretty_expr expr level ^ "--"
  | AssignEquals (_, lvl, el) -> pretty_lvaluelist ~level lvl ^ " = " ^ pretty_exprlist el level
  | Assign (_, assop, lv, e) -> pretty_lvalue ~level lv ^ " " ^ pretty_assignop assop ^ " " ^ pretty_expr e level
  | AssignVarEquals (_, vl, el) -> pretty_varlist vl ^ " = " ^ pretty_exprlist el level
  | AssignVar (_, assop, v, e) -> v ^ " " ^ pretty_assignop assop ^ " " ^ pretty_expr e level
  | ShortVarDecl (_, idlist, exprlist, dlor) -> pretty_varlist idlist ^ " := " ^ pretty_exprlist exprlist level

let pretty_varspec ~level : varspec -> string = function
  | VarSpecTp (_, vss, elo) -> pretty_varspecsimp ~level vss ^ pretty_option (fun el -> " = " ^ pretty_exprlist el level) elo ^ "\n"
  | VarSpecNoTp (_, idl, el) -> pretty_varlist idl ^ " = " ^ pretty_exprlist el level ^ "\n"

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
  | IfCond (_, sso, expr) -> pretty_option (fun ss -> pretty_simplestmt ~level ss ^ "; ") sso ^ pretty_expr expr level

let rec pretty : prog -> string = function
    Prog (_, package, tld) -> pretty_package package ^ pretty_topleveldecl_list tld ~level:Zero

and pretty_package : package -> string = function
    Package (_, name) -> "package " ^ name ^ "\n\n"

and pretty_topleveldecl_list (tldl : topleveldecl list) ~level =
  String.concat (List.map tldl ~f:(pretty_topleveldecl ~level))

and pretty_topleveldecl (tld : topleveldecl) ~level : string = match tld with
  | FuncDecl (p, i, vsslo, tpo, sl) -> "func " ^ i ^ "(" ^ pretty_option (pretty_varspecsimp_list ~level) vsslo ^ ") " ^ pretty_tp_option tpo ~level ^ pretty_stmt (Block (p, sl)) ~level
  | Stmt (_, stmt) -> pretty_stmt stmt ~level

and pretty_stmtlist (stmtlist: stmt list) ~level =
  String.concat (List.map stmtlist ~f:(fun sl -> indent level ^ pretty_stmt sl ~level))

(* Pretty printing statements *)
and pretty_stmt (stmt : stmt) ~level = match stmt with
  | Decl (_, declstmt) -> pretty_declstmt ~level declstmt
  | Simple (_, simplestmt) -> pretty_simplestmt ~level simplestmt ^ "\n"
  | Return (_, eop) -> "return " ^ pretty_expr_option eop level ^ "\n"
  | Break _ -> "break\n"
  | Continue _ -> "continue\n"
  | Block (_, stmtlist) -> "{\n" ^ pretty_stmtlist stmtlist (Succ level) ^ indent level ^ "}\n"
  | If (_, ifstmt) -> "if " ^ pretty_if ifstmt level
  | Switch (_, switchstmt) -> "switch " ^ pretty_switch switchstmt level
  | For (_, forstmt) -> "for " ^ pretty_for forstmt level
  | Print (_, printstmt) -> pretty_print printstmt level

and pretty_exprlist_option (e : expr list option) level = match e with
  | None -> ""
  | Some exprlist -> pretty_exprlist exprlist level


(* Pretty printing if statements *)
and pretty_if (ifstmt : ifstmt) level = (match ifstmt with
  | IfOnly (p, ifcond, block) -> pretty_ifcond ~level ifcond ^ " " ^ pretty_stmt (Block (p, block)) level
  | IfElse (p, ifcond, b1, b2) -> (let block = pretty_stmt (Block (p, b1)) level in
				pretty_ifcond ~level ifcond ^ " " ^ String.sub block 0 ((String.length block) -1) ^ " else " ^ pretty_stmt (Block (p, b2)) level )
  | IfElseIf (p, ifcond, block, ifstmt) -> (let block = pretty_stmt (Block (p, block)) level in pretty_ifcond ~level ifcond ^ " " ^ String.sub block 0 ((String.length block) -1) ^ indent level ^ "else if " ^ pretty_if (ifstmt) level)
			     )

and pretty_simple_option (so: simplestmt option) level = (match so with
  | None -> ""
  | Some s -> pretty_simplestmt ~level s
						  )

(* Pretty printing switch statements *)
and pretty_switch (switchstmt : switchstmt) level = (match switchstmt with
  | SwitchStmt (_, sco, ecclist) -> pretty_switch_cond_option sco level ^ "{\n" ^ pretty_expr_caseclause_list ecclist (Succ level) ^ indent (level) ^ "}\n"
				     )

and pretty_switch_cond_option (sco : switchcond option) level = (match sco with
  | None -> ""
  | Some sc -> pretty_switch_cond sc level
				    )

and pretty_switch_cond (sc : switchcond) level = (match sc with
  | SwitchCond (_, None, None) -> raise (Problem "Switch Cond lacks both") (* Should have either the expression of the simple stmt *)
  | SwitchCond (_, None, eo) -> pretty_expr_option eo level
  | SwitchCond (_, Some stmt, eo) -> pretty_simplestmt ~level stmt ^ " ; " ^ pretty_expr_option eo level
			    )

and pretty_expr_caseclause_list (ecclist : exprcaseclause list ) level = (match ecclist with
  | [] -> ""
  | h :: t -> pretty_expr_caseclause h level ^ pretty_expr_caseclause_list t level
						)


and pretty_expr_caseclause (ecc : exprcaseclause) level = (match ecc with
  | ExprCaseClause (_, esc, stmtlist) -> pretty_switch_case esc level ^ " : " ^ pretty_stmtlist stmtlist level
				 )

and pretty_switch_case (esc : exprswitchcase) level = (match esc with
  | Default _ -> indent level ^  "default"
  | Case (_, exprlist) -> indent level ^ "case " ^ pretty_exprlist exprlist level
			     )

(* Pretty printing for statements *)
and pretty_for (forstmt : forstmt) level = (match forstmt with
  | InfLoop (p, stmtlist) -> pretty_stmt (Block (p, stmtlist)) level
  | WhileLoop (p, expr, stmtlist) -> pretty_expr expr level ^ " " ^ pretty_stmt (Block (p, stmtlist)) level
  | ForLoop (p, s1, expr, s2, stmtlist) -> pretty_simple_option s1 level ^ "; " ^ pretty_expr_option expr level ^ "; " ^ pretty_simple_option s2 level ^ pretty_stmt (Block (p, stmtlist)) level
			      )

and pretty_print (printstmt : printstmt) level = (match printstmt with
  | PrintStmt (_, exprlistoption) -> "print(" ^ pretty_exprlist_option exprlistoption level ^ ")\n"
  | PrintlnStmt (_, exprlistoption) -> "println(" ^ pretty_exprlist_option exprlistoption level ^ ")\n"
)
