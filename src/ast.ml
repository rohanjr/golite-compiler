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

open Lexing

type id = string

type tp =
  | Int | Float64 | Bool | Rune | String | Void
  | FuncTp of tp list * tp
  | TypeVar of id * tp option ref
  | TArray of int * tp
  | TSlice of tp
  | TStruct of varspecsimp list

and varspecsimp = id list * tp

type binop =
  | LOr | LAnd
  | CmpEq | NotEq
  | LT | GT | LTE | GTE
  | Plus | Minus
  | Times | Div | Mod
  | BitOr | BitAnd
  | BitXOr | BitClr
  | LShift | RShift

type uop = UPlus | UMinus | LNot | UBitXor

type expr =
  | Unary of position * unaryexpr * tp option ref
  | Binary of position * binop * expr * expr * tp option ref

and unaryexpr =
  | Primary of position * primaryexpr * tp option ref
  | UnaryOp of position * uop * unaryexpr * tp option ref

and primaryexpr =
  | Operand of position * operand * tp option ref
  | Sel of position * primaryexpr * id * tp option ref
  | ArrAccess of position * primaryexpr * expr * tp option ref
  | Slice of position * primaryexpr * expr option * expr option * tp option ref
  | SliceCap of position * primaryexpr * expr option * expr * expr * tp option ref
  | FunApp of position * primaryexpr * expr list * tp option ref
  | Append of position * id * expr * tp option ref
  | Cast of position * tp * expr * tp option ref

and operand =
  | Parens of position * expr * tp option ref
  | Var of position * id * tp option ref
  | IntLit of position * int * tp option ref
  | FloatLit of position * float * tp option ref
  | RuneLit of position * string * tp option ref
  | StrLit of position * string * tp option ref

type lvalue =
  | LSel of position * primaryexpr * id * tp option ref
  | LArrAccess of position * primaryexpr * expr * tp option ref
  | LSlice of position * primaryexpr * expr option * expr option * tp option ref
  | LSliceCap of position * primaryexpr * expr option * expr * expr * tp option ref

type assignop = (* doesn't contain Equals because we handle it separately *)
  | PlusEq | MinusEq | TimesEq | DivEq | ModEq
  | AndEq | OrEq | XOrEq | LShiftEq | RShiftEq | ClrEq

type simplestmt =
  | Expr of position * expr
  | Inc of position * expr
  | Dec of position * expr
  | Assign of position * assignop * lvalue * expr
  | AssignEquals of position * lvalue list * expr list
  | AssignVar of position * assignop * id * expr
  | AssignVarEquals of position * id list * expr list
  | ShortVarDecl of position * id list * expr list * bool list option ref

type varspec =
  | VarSpecTp of position * varspecsimp * expr list option
  | VarSpecNoTp of position * id list * expr list

type typespec =
  | TpSpec of position * id * tp

type declstmt =
  | VarDecls of position * varspec list
  | TypeDecls of position * typespec list

type ifcond = IfCond of position * simplestmt option * expr

type switchcond = SwitchCond of position * simplestmt option * expr option

type exprswitchcase =
  | Case of position * expr list
  | Default of position

type printstmt =
  | PrintStmt of position * expr list option
  | PrintlnStmt of position * expr list option

type stmt =
  | Decl of position * declstmt
  | Simple of position * simplestmt
  | Return of position * expr option
  | Break of position
  | Continue of position
  | Block of position * stmt list
  | If of position * ifstmt
  | Switch of position * switchstmt
  | For of position * forstmt
  | Print of position * printstmt

and ifstmt =
  | IfOnly of position * ifcond * stmt list
  | IfElse of position * ifcond * stmt list * stmt list
  | IfElseIf of position * ifcond * stmt list * ifstmt

and switchstmt = SwitchStmt of position * switchcond option * exprcaseclause list

and exprcaseclause = ExprCaseClause of position * exprswitchcase * stmt list

and forstmt =
  | InfLoop of position * stmt list
  | WhileLoop of position * expr * stmt list
  | ForLoop of position * simplestmt option * expr option * simplestmt option * stmt list

type package = Package of position * id

type topleveldecl =
  | FuncDecl of position * id * varspecsimp list option * tp option * stmt list
  | Stmt of position * stmt

type prog = Prog of position * package * topleveldecl list
