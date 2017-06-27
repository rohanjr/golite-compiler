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

open Ast
open Pretty
open Printf
open Lexing

exception InternalError of string
exception Unimplemented of string

let string_of_pos pos = sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let headers = ["<stdlib.h>"; "<stdio.h>"; "<stdbool.h>"]

let slice = "typedef struct __slice {\n void *start;\n  int length;\n int cap;\n} __slice;\n\n__slice __new_slice(void *arr, int length, int cap)\n{\n  __slice s = {\n   arr,\n    length,\n   cap\n };\n\n  return s;\n}\n"

let rec gen_prog : prog -> string = function
  Prog (_, _, dl) -> Util.concatmap (fun h -> "#include " ^ h ^ "\n") headers ^ slice ^
                     Util.concatmap gen_topleveldecl dl

and gen_topleveldecl : topleveldecl -> string = function
  | FuncDecl (_, func_name, vsslo, tpo, sl) ->
      let gen_varspecsimp (il, t) =
        let tp_str = gen_tp t in
        String.concat ", " (List.map (fun i -> tp_str ^ " " ^ i) il)
      in let param_list = begin match vsslo with
        | None -> "void"
        | Some vssl -> String.concat ", " (List.map gen_varspecsimp vssl)
      end in let ret_tp_str = Util.omap gen_tp "void" tpo in
      ret_tp_str ^ " " ^ func_name ^ "(" ^ param_list ^ ") " ^ gen_block sl ^ "\n"
  | Stmt (_, s) -> gen_stmt s

and gen_stmt : stmt -> string = function
  | Decl (_, ds) -> gen_declstmt ds ^ "\n"
  | Simple (_, ss) -> gen_simplestmt ss ^ ";\n"
  | Return (_, eo) -> "return" ^ Util.omap (fun e -> " " ^ gen_expr e) "" eo ^ ";\n"
  | Break _ -> "break;\n"
  | Continue _ -> "continue;\n"
  | Block (_, sl) -> gen_block sl ^ "\n"
  | If (_, ifs) -> gen_ifstmt ifs
  | Switch (_, sws) -> gen_switchstmt sws
  | For (_, fs) -> gen_forstmt fs
  | Print (_, ps) -> gen_printstmt ps

and gen_declstmt : declstmt -> string = function
  | VarDecls (_, vsl) -> String.concat "\n" (List.flatten (List.map gen_varspec vsl))
  | TypeDecls (_, tsl) -> String.concat "\n" (List.map gen_typespec tsl)

and gen_varspec : varspec -> string list = function
  | VarSpecTp (_, vss, elo) -> 
    let varspecs = gen_varspecsimp vss in
    List.map2 (fun x y -> x ^ y)
      (varspecs)
      (Util.omap (List.map (fun x -> " = " ^ gen_expr x ^ ";")) (List.map (fun x -> ";") varspecs) elo)
  | VarSpecNoTp (_, il, el) -> 
    List.map2 (fun x (y,z) -> gen_tp z ^ " " ^ x ^ " = " ^ y ^ ";") 
      il 
      (List.combine (List.map gen_expr el) (List.map get_expr_type el))

and get_expr_type : expr -> tp = function
  | Unary (pos, _, tpo) -> 
    begin
      match !tpo with
      | None -> (raise (InternalError ("Unary expression not assigned type. " ^ string_of_pos pos ^ "\n")))
      | Some typ -> typ
    end
  | Binary (pos, _, _, _, tpo) -> 
    begin
      match !tpo with
      | None -> (raise (InternalError ("Binary expression not assigned type. " ^ string_of_pos pos ^ "\n")))
      | Some typ -> typ
    end

and gen_typespec : typespec -> string = function
  | TpSpec (_, id, tp) -> "typedef " ^ gen_tp tp ^ " " ^ id ^ ";"

and gen_varspecsimp (ids, t) : string list = 
  let gen_decl i = match t with
    | TArray (n, tt) ->
        gen_tp tt ^ " " ^ i ^ "[" ^ string_of_int n ^ "]"
    | _ -> gen_tp t ^ " " ^ i in
  List.map gen_decl ids

and gen_simplestmt : simplestmt -> string = function
  | Expr (_, e) -> gen_expr e
  | Inc (_, e) -> gen_expr e ^ "++"
  | Dec (_, e) -> gen_expr e ^ "--"
  | Assign (_, assop, lval, e) -> gen_assign (Some assop) (gen_lvalue lval) e
  | AssignEquals (_, lvall, el) -> String.concat ", " (List.map2 (fun lval e -> gen_assign None (gen_lvalue lval) e) lvall el)
  | AssignVar (_, assop, i, e) -> gen_assign (Some assop) i e
  | AssignVarEquals (_, idl, el) -> 
    String.concat ", " 
    (List.filter (fun x -> x <> "") (List.map2 (fun id e -> if id = "_" then "" else gen_assign None id e) idl el))
  | ShortVarDecl (_, idl, el, dlor) ->
      let dl = 
        begin
          match !dlor with
          | None -> raise (InternalError "ShortVarDecl lacks a declaration list")
          | Some dl' -> dl'
        end
      in
    String.concat "; "
    (List.filter (fun x -> x <> "") (* Remove empty string *)
    (List.map2 
      (fun (id, d) e -> 
        if id = "_" then "" else (* If underscore, then empty string *)
        if d 
        then gen_assign None id e 
        else 
          let t = get_expr_type e in
          gen_tp t ^ " " ^ gen_assign None id e
      ) 
    (List.combine idl dl) el))

and gen_assign (assopo : assignop option) (lv : string) (e : expr) : string =
  (* TODO what if assigning slices? C struct assignment should be sufficient? *)
  let op = Util.omap gen_assignop "=" assopo in
  lv ^ " " ^ op ^ " " ^ gen_expr e

and gen_ifstmt : ifstmt -> string =
  let gen_ifcond = function
    IfCond (_, sso, e) -> Util.omap (fun ss -> gen_simplestmt ss ^ ", ") "" sso ^ gen_expr e in
  let gen_if ic sl = "if (" ^ gen_ifcond ic ^ ") " ^ gen_block sl in
  function
  | IfOnly (_, ic, sl) -> gen_if ic sl ^ "\n"
  | IfElse (_, ic, sl1, sl2) -> gen_if ic sl1 ^ " else " ^ gen_block sl2 ^ "\n"
  | IfElseIf (_, ic, sl, is) -> gen_if ic sl ^ " else " ^ gen_ifstmt is
  
and gen_switchstmt : switchstmt -> string = function
  SwitchStmt (_, sco, eccl) -> "switch (" ^ Util.omap gen_switchcond "true" sco ^ ") {\n" ^
                               Util.concatmap gen_exprcaseclause eccl ^ "}\n"

and gen_switchcond : switchcond -> string = function
  SwitchCond (_, sso, eo) ->
    Util.omap (fun ss -> gen_simplestmt ss ^ "; ") "" sso ^
    Util.omap gen_expr "true" eo

and gen_exprcaseclause : exprcaseclause -> string = function
  ExprCaseClause (_, esc, sl) -> gen_exprswitchcase esc ^ Util.concatmap gen_stmt sl ^ "break;\n"

and gen_exprswitchcase : exprswitchcase -> string = function
  | Case (_, el) -> Util.concatmap (fun e -> "case " ^ gen_expr e ^ ":\n") el
  | Default _ -> "default:\n"

and gen_forstmt : forstmt -> string = function
  | InfLoop (_, sl) -> "while (1) " ^ gen_block sl ^ "\n"
  | WhileLoop (_, e, sl) -> "while (" ^ gen_expr e ^ ") " ^ gen_block sl ^ "\n"
  | ForLoop (_, sso1, eo, sso2, sl) ->
      "for (" ^ Util.omap gen_simplestmt "" sso1 ^ "; " ^ Util.omap gen_expr "" eo ^ "; " ^
                Util.omap gen_simplestmt "" sso2 ^ ") " ^ gen_block sl ^ "\n"

and gen_printstmt : printstmt -> string =
  (* TODO convert bool expr to string before printing *)
  function
  | PrintStmt (_, elo) ->
      begin match elo with
      | None -> ""
      | Some el ->
          let format_str = String.concat "" (List.map gen_fmt el) in
          "printf(\"" ^ format_str ^ "\", " ^ String.concat ", " (List.map gen_expr el) ^ ");\n"
      end
  | PrintlnStmt (_, elo) -> 
      begin match elo with
      | None -> "puts(\"\");\n"
      | Some el ->
          let format_str = String.concat " " (List.map gen_fmt el) in
          "printf(\"" ^ format_str ^ "\\n\", " ^ String.concat ", " (List.map gen_expr el) ^ ");\n"
      end

and gen_fmt (e : expr) : string = match e with
  Unary (_, _, tpor) | Binary (_, _, _, _, tpor) ->
    begin match !tpor with
    | None -> raise (InternalError ("Expression " ^ pretty_expr e Zero ^ " has no type during code generation."))
    | Some t -> 
      let rec f typ = 
      begin 
        match typ with
        | Int -> "%d"
        | Float64 -> "%f"
        | Bool -> "%d" (* TODO Need to convert the expression to a string before printing *)
        | Rune -> "%c" (* TODO Rune same as char? *)
        | String -> "%s"
        | TypeVar (tp, tor) -> 
          begin
            match !tor with
            | None -> raise (InternalError ("Type alias unresolved"))
            | Some t' -> f t'
          end
        | _ -> raise (InternalError ("Expression " ^ pretty_expr e Zero ^ " has wrong type for printing."))
      end
    in f t
  end

and gen_expr : expr -> string = function
  | Unary (_, ue, _) -> gen_unaryexpr ue
  | Binary (_, op, e1, e2, _) ->
      begin match op with
      | BitClr -> gen_expr e1 ^ " & (0xFF ^ " ^ gen_expr e2 ^ ")"
      | _ -> gen_expr e1 ^ " " ^ gen_binop op ^ " " ^ gen_expr e2
      end

and gen_unaryexpr : unaryexpr -> string = function
  | Primary (_, pe, _) -> gen_primaryexpr pe
  | UnaryOp (_, op, ue, _) -> gen_uop op ^ gen_unaryexpr ue

and gen_primaryexpr : primaryexpr -> string = function
  | Operand (_, o, _) -> gen_operand o
  | Sel (_, pe, i, _) -> gen_fieldsel pe i
  | ArrAccess (_, pe, e, _) -> gen_arrayaccess pe e
  | Slice (_, pe, eo1, eo2, _) -> gen_slice pe eo1 eo2 None
  | SliceCap (_, pe, eo, e1, e2, _) -> gen_slice pe eo (Some e1) (Some e2)
  | FunApp (_, pe, el, _) -> gen_primaryexpr pe ^ "(" ^ String.concat ", " (List.map gen_expr el) ^ ")"
  | Append (_, i, e, _) -> ""
    (* "{\n" ^
    "malloc (sizeof(" ^ (* type *) ") * (" ^ (* name *) ^ ".length + 1)),\n" ^
    (* name *) ^ ".length + 1,\n" ^
    (* name *) ^ ".cap + 1,\n" ^
    "}\n" ^
    "for(int i = 0; i < " ^ (* name *) ^ ".length; i++)\n" ^
    "{\n" ^
    "((" ^ (* type *) ^ "*) " ^ (* name *) ^ ".start)[i] = ((" ^ (* type *) ^ "*) " ^ (* name *) ^ ".start)[i];" ^
    "}\n"
    "((" ^ (* type *) ^ "*) " ^ (* name *) ^ ".start)[" ^ (* name *) ^ ".length] = " ^ (* thing to append *) ^ "\n" *)
  | Cast (_, t, e, _) -> "(" ^ gen_tp t ^ ") (" ^ gen_expr e ^ ")"

and gen_operand : operand -> string = function
  | Parens (_, e, _) -> "(" ^ gen_expr e ^ ")"
  | Var (_, i, _) -> i
  | IntLit (_, x, _) -> string_of_int x
  | FloatLit (_, f, _) -> string_of_float f
  | RuneLit (_, str, _) -> str
  | StrLit (_, str, _) -> (* raw string literals are backquoted in Go, so need to replace with usual double quotes *)
      try "\"" ^ String.sub str 1 (String.length str - 2) ^ "\""
      with Invalid_argument _ -> raise (InternalError ("String literal " ^ str ^ " does not have correct format."))

and gen_uop : uop -> string = function
  | UPlus -> "+"
  | UMinus -> "-"
  | LNot -> "!"
  | UBitXor -> "^"

and gen_binop : binop -> string = function
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
  | Times -> "*"
  | Div -> "/"
  | Mod -> "%"
  | BitOr -> "|"
  | BitAnd -> "&"
  | BitXOr -> "^"
  | BitClr -> raise (InternalError "Trying to generate code for BitClr operator which should be handled separately.")
  | LShift -> "<<"
  | RShift -> ">>"

and gen_assignop : assignop -> string = function
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

and gen_lvalue : lvalue -> string = function
  | LSel (_, pe, i, _) -> gen_fieldsel pe i
  | LArrAccess (_, pe, e, _) -> gen_arrayaccess pe e
  | LSlice (_, pe, eo1, eo2, _) -> gen_slice pe eo1 eo2 None
  | LSliceCap (_, pe, eo, e1, e2, _) -> gen_slice pe eo (Some e1) (Some e2)

and gen_fieldsel (pe : primaryexpr) (i : id) : string = gen_primaryexpr pe ^ "." ^ i

and tp_of_primaryexpr (pe : primaryexpr) : tp = match pe with
  | Operand (_, _, tpor) | Sel (_, _, _, tpor) | ArrAccess (_, _, _, tpor)
  | Slice (_, _, _, _, tpor) | SliceCap (_, _, _, _, _, tpor)
  | FunApp (_, _, _, tpor) | Append (_, _, _, tpor) | Cast (_, _, _, tpor) ->
      begin match !tpor with
      | None -> raise (InternalError ("Primary expression " ^ pretty_primary pe Zero ^ " has no type during code generation."))
      | Some t -> t
      end

and gen_arrayaccess (pe : primaryexpr) (e : expr) : string =
  let pe_str = gen_primaryexpr pe in
  let e_str = gen_expr e in
  match tp_of_primaryexpr pe with
  | TArray (_, _) -> pe_str ^ "[" ^ e_str ^ "]"
  | TSlice tp ->  "((" ^ gen_tp tp ^ "*) " ^ pe_str ^ ".start)[" ^ e_str ^ "]"
  | t -> raise (InternalError ("Primary expression " ^ pretty_primary pe Zero ^ " has type " ^ pretty_tp t Zero ^
                               ", but should have array type for use in an indexing expression."))

and gen_slice (pe : primaryexpr) (eo1 : expr option) (eo2 : expr option) (eo3 : expr option) : string =
  match tp_of_primaryexpr pe with
  | TArray (n, t) ->      
    let initial_cap = string_of_int n in
        let low =  Util.omap gen_expr "0" eo1 in        
        let high = Util.omap gen_expr initial_cap eo2 in
        let max = Util.omap gen_expr initial_cap eo3 in      
      "__new_slice(&" ^ gen_primaryexpr pe ^ "[" ^ low ^ "], " 
        ^ string_of_int ((int_of_string high) - (int_of_string low)) ^ ", "
       ^ string_of_int ((int_of_string max) - (int_of_string low)) ^  ")"
  | t -> raise (InternalError ("Primary expression " ^ pretty_primary pe Zero ^ " has type " ^ pretty_tp t Zero ^
                               ", but should have array type for use in a slice expression."))

and gen_block (sl : stmt list) : string = "{\n" ^ Util.concatmap gen_stmt sl ^ "}"

and gen_tp : tp -> string = function
  | Int -> "int"
  | Float64 -> "double"
  | Bool -> "bool"
  | Rune -> "char" (* TODO is rune the same as a char? *)
  | String -> "char *" (* TODO need to implement this *)
  | Void -> "void"
  | FuncTp _ -> raise (InternalError "Should not need to generate string for function type.")
  | TypeVar (i, _) -> i
  | TArray (_, t) -> gen_tp t ^ "[]" (* Should be handled beforehand, because the [] needs to go after the identifier *)
  | TSlice t -> "struct __slice"
  | TStruct vssl -> 
    let gen_varspecsimp (il, t) =
        let tp_str = gen_tp t in
        String.concat "; " (List.map (fun i -> tp_str ^ " " ^ i) il)
    in
    "struct {" ^ String.concat "; " (List.map gen_varspecsimp vssl) ^ "}"
