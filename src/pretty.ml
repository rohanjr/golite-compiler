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
exception Problem of string
open Ast

type nat =
  | Zero
  | Succ of nat

let rec indent level = (match level with
  | Zero -> ""
  | Succ level -> "   " ^ indent level
	       )

let rec pretty (ast : prog) = (let Prog (_, package, tld) = ast in
  pretty_package package ^ pretty_topleveldecl_list tld Zero
				  )

and pretty_package (package: package) = (let Package (_, name) = package in
			     "package "^ name ^"\n\n"
			     )

and pretty_topleveldecl_list (tldl : topleveldecl list) level = (match tldl with
  | [] -> ""
  | h :: t -> pretty_topleveldecl h level ^ pretty_topleveldecl_list t level
								)

and pretty_topleveldecl (tld : topleveldecl) level = (match tld with
  | FuncDecl (p, i, vsslo, tpo, sl) -> "func " ^ i ^ "(" ^ pretty_varspecsimp_list_option vsslo level ^ ") " ^ pretty_tp_option tpo level ^ pretty_stmt (Block (p, sl)) level
  | Stmt (_, stmt) -> pretty_stmt stmt level
						     )

and pretty_stmtlist (stmtlist: stmt list) level = ( match stmtlist with
  | [] -> ""
  | stmt :: t -> indent level ^ (pretty_stmt stmt level) ^ (pretty_stmtlist t level)
				     )

(* Pretty printing statements *)
and pretty_stmt (stmt : stmt) level = (match stmt with
  | Decl (_, declstmt) -> pretty_decl declstmt level
  | Simple (_, simplestmt) -> pretty_simple simplestmt level ^ "\n"
  | Return (_, eop) -> "return " ^ pretty_expr_option eop level ^ "\n"
  | Break _ -> "break\n"
  | Continue _ -> "continue\n"
  | Block (_, stmtlist) -> "{\n" ^ pretty_stmtlist stmtlist (Succ level) ^ indent level ^ "}\n"
  | If (_, ifstmt) -> "if " ^ pretty_if ifstmt level
  | Switch (_, switchstmt) -> "switch " ^ pretty_switch switchstmt level
  | For (_, forstmt) -> "for " ^ pretty_for forstmt level
  | Print (_, printstmt) -> pretty_print printstmt level
			     )

(* Pretty printing declarations  *)
and pretty_decl (declstmt : declstmt ) level = (match declstmt with
  | VarDecls (_, [varspec]) -> "var " ^ pretty_var_decl varspec level
  | TypeDecls (_, [typespec]) -> "type " ^ pretty_type_decl typespec level
  | VarDecls (_, vslist) -> "var (\n"  ^ pretty_varspecsemi_list vslist (Succ level) ^ indent level ^ ")\n"
  | TypeDecls (_, tsl) -> "type ( \n" ^ pretty_typespecsemi_list tsl (Succ level) ^ indent level ^ ")\n"
					       )

and pretty_varspecsemi_list (vsl : varspec list) level = (match vsl with
  | [] -> ""
  | h :: t -> indent level ^ pretty_var_decl h level ^ pretty_varspecsemi_list t level
				 )

and pretty_typespecsemi_list (tsl : typespec list) level = (match tsl with
  | [] -> ""
  | h :: t -> indent level ^ pretty_type_decl h level ^ pretty_typespecsemi_list t level
				 )

(* Pretty printing variable declarations *)
and pretty_var_decl (varspec : varspec) level = (match varspec with
  | VarSpecTp (_, varspecsimp, None) -> pretty_varspecsimp varspecsimp level ^ "\n"
  | VarSpecTp (_, varspecsimp, Some exprlist) -> pretty_varspecsimp varspecsimp level ^ " = " ^ pretty_exprlist exprlist level ^ "\n"
  | VarSpecNoTp (_, varlist, exprlist) -> pretty_varlist varlist ^ " = " ^ pretty_exprlist exprlist level ^ "\n"
				    )

and pretty_varspecsimp (varspecsimp : varspecsimp) level = ( match varspecsimp with
  | (varlist, tp) -> pretty_varlist varlist ^ " " ^ pretty_tp tp level
)

and pretty_tp_option (tpo : tp option) level = (match tpo with
  | None -> ""
  | Some t -> pretty_tp t level ^ " "
					     )

and pretty_tp (tp : tp) level = (match tp with
  | Void -> "void"
  | Int -> "int"
  | Float64 -> "float64"
  | Bool -> "bool"
  | Rune -> "rune"
  | String -> "string"
  | FuncTp (tl, rt) -> "(" ^ (match tl with 
			      | [] -> "void"             
			      | _ -> String.concat " * " (List.map (fun t -> pretty_tp t level) tl))
                       ^ ") -> " ^ pretty_tp rt level
  | TypeVar (id, _) -> id
  | TSlice (t) -> "[]" ^ pretty_tp t level
  | TArray (size, t) -> "[" ^ string_of_int size ^ "]" ^ pretty_tp t level
  | TStruct (vsslist) -> "struct {\n" ^ pretty_struct vsslist (Succ level) ^ indent level ^ "}"
			 )

and pretty_varspecsimp_list_option (vsslo : varspecsimp list option) level = (match vsslo with
  | None -> ""
  | Some vssl -> pretty_varspecsimp_list vssl level
)

and pretty_varspecsimp_list (vssl : varspecsimp list) level = (match vssl with
  | [] -> ""
  | [vss] -> pretty_varspecsimp vss level
  | h::t -> pretty_varspecsimp h level ^ ", " ^ pretty_varspecsimp_list t level
							 )

and pretty_varlist (varlist: id list) = (match varlist with
  | [] -> raise (Problem "Varlists should be non-empty.") (* Varlists should be non-empty *)
  | [x] -> x
  | h::t -> h ^ ", " ^ pretty_varlist t
					)

(* Pretty printing expressions*)
and pretty_exprlist (exprlist: expr list) level = (match exprlist with
  | [] -> ""
  | [expr] -> pretty_expr expr level
  | h :: t -> pretty_expr h level ^ ", " ^ pretty_exprlist t level
					    )
and pretty_expr (expr: expr) level = (match expr with
  | Unary (_, u, tpor) -> pretty_unary u level(* ^ (match !tpor with
		     | None -> ""
		     | Some t -> "/* : " ^ pretty_tp t Zero ^ "*/"
		    )*)
  | Binary (_, op, e1, e2, tpor) -> pretty_expr e1 level ^ " " ^ pretty_binop op ^ " " ^ pretty_expr e2 level ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
			       )

and pretty_unary (u: unaryexpr) level = (match u with
  | Primary (_, prim, tpor) -> pretty_primary prim level (* ^ (match !tpor with
		     | None -> ""
		     | Some t -> "/* : " ^ pretty_tp t Zero ^ "*/"
		    )*)
  | UnaryOp (_, op, u, tpor) -> pretty_uop op ^ pretty_unary u level ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
				  )

and pretty_uop (op: uop) = (match op with
  | UPlus -> "+"
  | UMinus -> "-"
  | LNot -> "!"
  | UBitXor -> "^"
			   )

and pretty_binop (op: binop) = (match op with
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
			       )

and pretty_primary (prim : primaryexpr) level = (match prim with
  | Operand (_, oper, tpor) -> pretty_operand oper level (* ^ (match !tpor with
		     | None -> ""
		     | Some t -> "/* : " ^ pretty_tp t Zero ^ "*/"
		    )*)
  | Sel (_, prim, id, tpor) -> pretty_primary prim level ^ "." ^ id ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | ArrAccess (_, prim, expr, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr expr level ^ "]" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | Slice (_, prim, e1op, e2op, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option e1op level ^ " : " ^ pretty_expr_option e2op level ^ "]" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | SliceCap (_, prim, eop, e1, e2, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option eop level ^ " : " ^ pretty_expr e1 level ^ " : " ^ pretty_expr e2 level ^ "]" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | FunApp (_, prim, el, tpor) -> pretty_primary prim level ^ "(" ^ pretty_exprlist el level ^ ")" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | Append (_, x, expr, tpor) -> "append" ^ "(" ^ x ^ ", " ^ pretty_expr expr level ^ ")" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | Cast (_, tp, expr, tpor) -> pretty_tp tp level ^ "(" ^ pretty_expr expr level ^ ")" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
			       )

and pretty_operand (oper : operand) level = (match oper with
  | Parens (_, expr, tpor) -> "(" ^ pretty_expr expr level ^ ")"  ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | Var (_, id, tpor) -> id ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | IntLit (_, i, tpor) -> string_of_int i ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | FloatLit (_, f, tpor) -> string_of_float f ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | RuneLit (_, r, tpor) -> r ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | StrLit (_, s, tpor) -> s ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
				      )

and pretty_expr_option (e : expr option) value = (match e with
  | Some expr -> pretty_expr expr value
  | None -> ""
					    )

and pretty_exprlist_option (e : expr list option) level = (match e with
  | Some exprlist -> pretty_exprlist exprlist level
  | None -> ""
						   )


(* Pretty printing type declarations *)
and pretty_type_decl (typespec : typespec) level = (match typespec with
  | TpSpec (_,t, tp) -> t ^ " " ^ pretty_tp tp level ^ "\n"
						   )

and pretty_struct (vslist : varspecsimp list) level = (match vslist with
  | [] -> ""
  | h :: t -> indent level ^ pretty_varspecsimp h level ^ "\n" ^ pretty_struct t level
				 )

(* Pretty printing simple statements *)
and pretty_simple (simplestmt : simplestmt) level = (match simplestmt with
  | Expr (_, expr) -> pretty_expr expr level
  | Inc (_, expr) -> pretty_expr expr level ^ "++"
  | Dec (_, expr) -> pretty_expr expr level ^ "--"
  | AssignEquals (_, lvl, el) -> pretty_lvaluelist lvl level ^ " = " ^ pretty_exprlist el level
  | Assign (_, assop, lv, e) -> pretty_lvalue lv level ^ " " ^ pretty_assop assop ^ " " ^ pretty_expr e level
  | AssignVarEquals (_, vl, el) -> pretty_varlist vl ^ " = " ^ pretty_exprlist el level
  | AssignVar (_, assop, v, e) -> v ^ " " ^ pretty_assop assop ^ " " ^ pretty_expr e level
  | ShortVarDecl (_, idlist, exprlist, dlor) -> pretty_varlist idlist ^ " := " ^ pretty_exprlist exprlist level
				     )

and pretty_lvaluelist  (lvl : lvalue list) level = (match lvl with
  | [] -> raise (Problem " Lvaluelist should not be empty.") (* Should not be empty *)
  | [x] -> pretty_lvalue x level
  | h :: t -> pretty_lvalue h level ^ ", " ^ pretty_lvaluelist t level
					     )


and pretty_lvalue (lv : lvalue) level = (match lv with
  | LSel (_, prim, id, tpor) -> pretty_primary prim level ^ "." ^ id ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | LArrAccess (_, prim, expr, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr expr level ^ "]" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | LSlice (_, prim, eop1, eop2, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option eop1 level ^ " : " ^ pretty_expr_option eop2 level ^ "]" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
  | LSliceCap (_, prim, eop, e1, e2, tpor) -> pretty_primary prim level ^ "[" ^ pretty_expr_option eop level ^ " : " ^ pretty_expr e1 level ^ " : " ^ pretty_expr e2 level ^ "]" ^ (match !tpor with
		     | None -> ""
		     | Some t -> " /* : " ^ pretty_tp t Zero ^ "*/ "
		    )
					     )

and pretty_assop (assop : assignop) = (match assop with
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


(* Pretty printing if statements *)			 )
and pretty_if (ifstmt : ifstmt) level = (match ifstmt with
  | IfOnly (p, ifcond, block) -> pretty_ifcond ifcond level ^ " " ^ pretty_stmt (Block (p, block)) level
  | IfElse (p, ifcond, b1, b2) -> (let block = pretty_stmt (Block (p, b1)) level in
				pretty_ifcond ifcond level ^ " " ^ String.sub block 0 ((String.length block) -1) ^ " else " ^ pretty_stmt (Block (p, b2)) level )
  | IfElseIf (p, ifcond, block, ifstmt) -> (let block = pretty_stmt (Block (p, block)) level in pretty_ifcond ifcond level ^ " " ^ String.sub block 0 ((String.length block) -1) ^ indent level ^ "else if " ^ pretty_if (ifstmt) level)
			     )

and pretty_ifcond (ifcond : ifcond) level = (match ifcond with
  | IfCond (_, None, expr) -> pretty_expr expr level
  | IfCond (_, Some simplestmt, expr) -> pretty_simple simplestmt level ^ "; " ^ pretty_expr expr level
			   )

and pretty_simple_option (so: simplestmt option) level = (match so with
  | None -> ""
  | Some s -> pretty_simple s level
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
  | SwitchCond (_, Some stmt, eo) -> pretty_simple stmt level ^ " ; " ^ pretty_expr_option eo level
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
