/*
 * This code originated as part of the McGill COMP520 course requirements.
 * Any student examining the code or using any part of the code for their coursework must clearly
 * document what code they examined and give clear credit to any code that they used.
 *
 * Original authors:
 * Rohan Jacob-Rao (rohanjr)
 * Steven Thephsourinthone (stheph)
 * Shawn Otis
 */
open Core.Std;

exception Problem string;

open Ast;

let indentation = "   ";

type nat =
  | Zero
  | Succ nat;

let rec indent: nat => string =
  fun
  | Zero => ""
  | Succ level => indentation ^ indent level;

let pretty_option f::(f: 'a => string) :(option 'a => string) => Option.value_map default::"" ::f;

let pretty_idlist: list id => string =
  fun
  | [] => raise (Problem "Varlists should be non-empty.")
  | l => String.concat sep::", " l;

let rec pretty_tp ::level=Zero :(tp => string) =>
  fun
  | Void => "void"
  | Int => "int"
  | Float64 => "float64"
  | Bool => "bool"
  | Rune => "rune"
  | String => "string"
  | FuncTp tl rt =>
    "(" ^
    (
      switch tl {
      | [] => "void"
      | _ => String.concat sep::" * " (List.map f::(pretty_tp ::level) tl)
      }
    ) ^
    ") -> " ^ pretty_tp rt ::level
  | TypeVar id _ => id
  | TSlice t => "[]" ^ pretty_tp t ::level
  | TArray size t => "[" ^ string_of_int size ^ "]" ^ pretty_tp t ::level
  | TStruct vssl => "struct {\n" ^ pretty_struct vssl level::(Succ level) ^ indent level ^ "}"
and pretty_struct ::level (vssl: list varspecsimp) :string =>
  String.concat (
    List.map f::(fun vss => indent level ^ pretty_varspecsimp vss ::level ^ "\n") vssl
  )
and pretty_varspecsimp ::level :(varspecsimp => string) =>
  fun
  | (idl, tp) => pretty_idlist idl ^ " " ^ pretty_tp tp ::level;

let pretty_tp_option ::level :(option tp => string) =>
  pretty_option f::(fun t => pretty_tp t ::level ^ " ");

let pretty_comment_tp: option tp => string =
  pretty_option f::(fun t => " /* : " ^ pretty_tp t ^ "*/ ");

let pretty_varspecsimp_list ::level (vssl: list varspecsimp) :string =>
  String.concat sep::", " (List.map f::(pretty_varspecsimp ::level) vssl);

let pretty_binop: binop => string =
  fun
  | LOr => "||"
  | LAnd => "&&"
  | CmpEq => "=="
  | NotEq => "!="
  | LT => "<"
  | GT => ">"
  | LTE => "<="
  | GTE => ">="
  | Plus => "+"
  | Minus => "-"
  | BitOr => "|"
  | BitXOr => "^"
  | Times => "*"
  | Div => "/"
  | Mod => "%"
  | BitAnd => "&"
  | BitClr => "&^"
  | LShift => "<<"
  | RShift => ">>";

let pretty_uop: uop => string =
  fun
  | UPlus => "+"
  | UMinus => "-"
  | LNot => "!"
  | UBitXor => "^";

let rec pretty_expr ::level=Zero :(expr => string) =>
  fun
  | Unary _ u tpor => pretty_unary ::level u
  | Binary _ op e1 e2 tpor =>
    pretty_expr ::level e1 ^
    " " ^ pretty_binop op ^ " " ^ pretty_expr ::level e2 ^ pretty_comment_tp !tpor
and pretty_expr_option ::level :(option expr => string) => pretty_option f::(pretty_expr ::level)
and pretty_exprlist ::level (el: list expr) :string =>
  String.concat sep::", " (List.map f::(pretty_expr ::level) el)
and pretty_unary ::level :(unaryexpr => string) =>
  fun
  | Primary _ pe tpor => pretty_primaryexpr ::level pe
  | UnaryOp _ op u tpor => pretty_uop op ^ pretty_unary ::level u ^ pretty_comment_tp !tpor
and pretty_primaryexpr ::level=Zero :(primaryexpr => string) =>
  fun
  | Operand _ oper tpor => pretty_operand ::level oper
  | Sel _ prim id tpor => pretty_primaryexpr ::level prim ^ "." ^ id ^ pretty_comment_tp !tpor
  | ArrAccess _ prim e tpor =>
    pretty_primaryexpr ::level prim ^ "[" ^ pretty_expr ::level e ^ "]" ^ pretty_comment_tp !tpor
  | Slice _ prim e1op e2op tpor =>
    pretty_primaryexpr ::level prim ^
    "[" ^
    pretty_expr_option ::level e1op ^
    " : " ^ pretty_expr_option ::level e2op ^ "]" ^ pretty_comment_tp !tpor
  | SliceCap _ prim eop e1 e2 tpor =>
    pretty_primaryexpr ::level prim ^
    "[" ^
    pretty_expr_option ::level eop ^
    " : " ^ pretty_expr ::level e1 ^ " : " ^ pretty_expr ::level e2 ^ "]" ^ pretty_comment_tp !tpor
  | FunApp _ prim el tpor =>
    pretty_primaryexpr ::level prim ^
    "(" ^ pretty_exprlist ::level el ^ ")" ^ pretty_comment_tp !tpor
  | Append _ x e tpor =>
    "append" ^ "(" ^ x ^ ", " ^ pretty_expr ::level e ^ ")" ^ pretty_comment_tp !tpor
  | Cast _ tp e tpor =>
    pretty_tp tp ::level ^ "(" ^ pretty_expr ::level e ^ ")" ^ pretty_comment_tp !tpor
and pretty_operand ::level :(operand => string) =>
  fun
  | Parens _ e tpor => "(" ^ pretty_expr ::level e ^ ")" ^ pretty_comment_tp !tpor
  | Var _ id tpor => id ^ pretty_comment_tp !tpor
  | IntLit _ i tpor => string_of_int i ^ pretty_comment_tp !tpor
  | FloatLit _ f tpor => string_of_float f ^ pretty_comment_tp !tpor
  | RuneLit _ r tpor => r ^ pretty_comment_tp !tpor
  | StrLit _ s tpor => s ^ pretty_comment_tp !tpor;

let pretty_lvalue ::level :(lvalue => string) =>
  fun
  | LSel _ prim id tpor => pretty_primaryexpr ::level prim ^ "." ^ id ^ pretty_comment_tp !tpor
  | LArrAccess _ prim e tpor =>
    pretty_primaryexpr ::level prim ^ "[" ^ pretty_expr ::level e ^ "]" ^ pretty_comment_tp !tpor
  | LSlice _ prim eop1 eop2 tpor =>
    pretty_primaryexpr ::level prim ^
    "[" ^
    pretty_expr_option ::level eop1 ^
    " : " ^ pretty_expr_option ::level eop2 ^ "]" ^ pretty_comment_tp !tpor
  | LSliceCap _ prim eop e1 e2 tpor =>
    pretty_primaryexpr ::level prim ^
    "[" ^
    pretty_expr_option ::level eop ^
    " : " ^
    pretty_expr ::level e1 ^ " : " ^ pretty_expr ::level e2 ^ "]" ^ pretty_comment_tp !tpor;

let pretty_lvaluelist ::level :(list lvalue => string) =>
  fun
  | [] => raise (Problem " Lvaluelist should not be empty.")
  | l => String.concat sep::", " (List.map f::(pretty_lvalue ::level) l);

let pretty_assignop: assignop => string =
  fun
  | PlusEq => "+="
  | MinusEq => "-="
  | TimesEq => "*="
  | DivEq => "/="
  | ModEq => "%="
  | AndEq => "&="
  | OrEq => "|="
  | XOrEq => "^="
  | LShiftEq => "<<="
  | RShiftEq => ">>="
  | ClrEq => "&^=";

let pretty_simplestmt ::level :(simplestmt => string) =>
  fun
  | Expr _ e => pretty_expr ::level e
  | Inc _ e => pretty_expr ::level e ^ "++"
  | Dec _ e => pretty_expr ::level e ^ "--"
  | AssignEquals _ lvl el => pretty_lvaluelist ::level lvl ^ " = " ^ pretty_exprlist ::level el
  | Assign _ assop lv e =>
    pretty_lvalue ::level lv ^ " " ^ pretty_assignop assop ^ " " ^ pretty_expr ::level e
  | AssignVarEquals _ idl el => pretty_idlist idl ^ " = " ^ pretty_exprlist ::level el
  | AssignVar _ assop v e => v ^ " " ^ pretty_assignop assop ^ " " ^ pretty_expr ::level e
  | ShortVarDecl _ idl el dlor => pretty_idlist idl ^ " := " ^ pretty_exprlist ::level el;

let pretty_varspec ::level :(varspec => string) =>
  fun
  | VarSpecTp _ vss elo =>
    pretty_varspecsimp ::level vss ^
    pretty_option f::(fun el => " = " ^ pretty_exprlist ::level el) elo ^ "\n"
  | VarSpecNoTp _ idl el => pretty_idlist idl ^ " = " ^ pretty_exprlist ::level el ^ "\n";

let pretty_typespec ::level :(typespec => string) =>
  fun
  | TpSpec _ t tp => t ^ " " ^ pretty_tp ::level tp ^ "\n";

let pretty_varspecsemi_list (vsl: list varspec) level =>
  String.concat (List.map f::(fun vs => indent level ^ pretty_varspec ::level vs) vsl);

let pretty_typespecsemi_list (tsl: list typespec) level =>
  String.concat (List.map f::(fun vs => indent level ^ pretty_typespec ::level vs) tsl);

let pretty_declstmt ::level :(declstmt => string) =>
  fun
  | VarDecls _ [varspec] => "var " ^ pretty_varspec ::level varspec
  | VarDecls _ vsl => "var (\n" ^ pretty_varspecsemi_list vsl (Succ level) ^ indent level ^ ")\n"
  | TypeDecls _ [typespec] => "type " ^ pretty_typespec ::level typespec
  | TypeDecls _ tsl =>
    "type ( \n" ^ pretty_typespecsemi_list tsl (Succ level) ^ indent level ^ ")\n";

let pretty_ifcond ::level :(ifcond => string) =>
  fun
  | IfCond _ sso e =>
    pretty_option f::(fun ss => pretty_simplestmt ::level ss ^ "; ") sso ^ pretty_expr ::level e;

let pretty_switch_cond ::level :(switchcond => string) =>
  fun
  | SwitchCond _ None None =>
    /* TODO handle this case: isn't it legal? */
    raise (Problem "Switch condition lacks both statement and expression")
  | SwitchCond _ None eo => pretty_expr_option ::level eo
  | SwitchCond _ (Some stmt) eo =>
    pretty_simplestmt ::level stmt ^ " ; " ^ pretty_expr_option ::level eo;

let pretty_exprswitchcase ::level :(exprswitchcase => string) =>
  fun
  | Default _ => indent level ^ "default"
  | Case _ el => indent level ^ "case " ^ pretty_exprlist ::level el;

let pretty_printstmt ::level :(printstmt => string) =>
  fun
  | PrintStmt _ elo => "print(" ^ pretty_option f::(pretty_exprlist ::level) elo ^ ")\n"
  | PrintlnStmt _ elo => "println(" ^ pretty_option f::(pretty_exprlist ::level) elo ^ ")\n";

let rec pretty_stmt ::level :(stmt => string) =>
  fun
  | Decl _ declstmt => pretty_declstmt ::level declstmt
  | Simple _ simplestmt => pretty_simplestmt ::level simplestmt ^ "\n"
  | Return _ eo => "return" ^ pretty_option f::(fun e => " " ^ pretty_expr ::level e) eo ^ "\n"
  | Break _ => "break\n"
  | Continue _ => "continue\n"
  | Block _ sl => pretty_block ::level sl ^ "\n"
  | If _ ifstmt => pretty_ifstmt ::level ifstmt
  | Switch _ switchstmt => pretty_switchstmt ::level switchstmt
  | For _ forstmt => pretty_forstmt ::level forstmt
  | Print _ printstmt => pretty_printstmt ::level printstmt
and pretty_stmtlist ::level (sl: list stmt) :string =>
  String.concat (List.map f::(fun s => indent level ^ pretty_stmt ::level s) sl)
and pretty_block ::level (sl: list stmt) :string =>
  "{\n" ^ pretty_stmtlist level::(Succ level) sl ^ indent level ^ "}"
and pretty_ifstmt ::level (is: ifstmt) :string =>
  "if " ^ (
    switch is {
    | IfOnly p ifcond sl => pretty_ifcond ::level ifcond ^ " " ^ pretty_block ::level sl ^ "\n"
    | IfElse p ifcond b1 b2 =>
      pretty_ifcond ::level ifcond ^
      " " ^ pretty_block ::level b1 ^ " else " ^ pretty_block ::level b2 ^ "\n"
    | IfElseIf p ifcond block ifstmt =>
      pretty_ifcond ::level ifcond ^
      " " ^ pretty_block ::level block ^ " else " ^ pretty_ifstmt ::level ifstmt
    }
  )
and pretty_switchstmt ::level :(switchstmt => string) =>
  fun
  | SwitchStmt _ sco eccl =>
    "switch " ^
    pretty_option f::(pretty_switch_cond ::level) sco ^
    "{\n" ^
    String.concat (List.map f::(pretty_exprcaseclause level::(Succ level)) eccl) ^
    indent level ^ "}\n"
and pretty_exprcaseclause ::level :(exprcaseclause => string) =>
  fun
  | ExprCaseClause _ esc stmtlist =>
    pretty_exprswitchcase ::level esc ^ " : " ^ pretty_stmtlist ::level stmtlist
and pretty_forstmt ::level (fs: forstmt) :string =>
  "for " ^ (
    switch fs {
    | InfLoop p stmtlist => pretty_block ::level stmtlist ^ "\n"
    | WhileLoop p e stmtlist => pretty_expr ::level e ^ " " ^ pretty_block ::level stmtlist ^ "\n"
    | ForLoop p s1 eo s2 stmtlist =>
      pretty_option f::(pretty_simplestmt ::level) s1 ^
      "; " ^
      pretty_expr_option ::level eo ^
      "; " ^ pretty_option f::(pretty_simplestmt ::level) s2 ^ pretty_block ::level stmtlist ^ "\n"
    }
  );

let pretty_package: package => string =
  fun
  | Package _ name => "package " ^ name ^ "\n\n";

let pretty_topleveldecl ::level :(topleveldecl => string) =>
  fun
  | FuncDecl p i vsslo tpo sl =>
    "func " ^
    i ^
    "(" ^
    pretty_option f::(pretty_varspecsimp_list ::level) vsslo ^
    ") " ^ pretty_tp_option ::level tpo ^ pretty_block ::level sl ^ "\n"
  | Stmt _ stmt => pretty_stmt ::level stmt;

let pretty_prog: prog => string =
  fun
  | Prog _ pkg tldl =>
    pretty_package pkg ^ String.concat (List.map f::(pretty_topleveldecl level::Zero) tldl);
