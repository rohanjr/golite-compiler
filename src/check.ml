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

open Core.Std

open Lexing
open Ast
open Pretty

exception TypeError of string
exception DeclError of string
exception InternalError of string

type 'a dump_info = { filename : string; string_of_data : 'a -> string }

module type Symtbl = sig
  type 'a t
  val create : unit -> 'a t
  val add : 'a t -> key:string -> data:'a -> [ `Duplicate | `Ok ]
  val find : 'a t -> string -> 'a option
  val dump : 'a t -> position -> 'a dump_info -> unit
end

module Hash_symtbl : Symtbl = struct

  type 'a t = 'a String.Table.t

  let create () = String.Table.create ()

  let add = Hashtbl.add

  let find = Hashtbl.find

  let to_string tbl f =
    let string_of_binding (sym, v) = sym ^ " -> " ^ f v ^ "\n" in
    Hashtbl.to_alist tbl |> List.map ~f:string_of_binding |> String.concat

  let dump tbl pos {filename; string_of_data} =
    let out_chan = Out_channel.create ~append:true filename in
    let pos_line = Printf.sprintf "Scope opened on line %d\n" pos.pos_lnum in
    Printf.fprintf out_chan "%s%s\n" pos_line (to_string tbl string_of_data);
    Out_channel.close out_chan

end

module type Symtbl_stack = sig
  type 'a t
  val create : 'a dump_info option -> 'a t
  val open_scope : 'a t -> unit
  val add_binding : 'a t -> name:string -> data:'a -> unit
  val lookup : 'a t -> string -> 'a option
  val lookup_topmost : 'a t -> string -> 'a option
  val close_scope : 'a t -> position -> unit
  (* dumps symbol table for closed scope if dump information is present *)
end

module Make_symtbl_stack(ST : Symtbl) = struct

  type 'a t = { stack : 'a ST.t Stack.t; info : 'a dump_info option }

  let create info = { stack = Stack.create (); info }

  let open_scope s = Stack.push s.stack (ST.create ())

  let add_binding s name data = match Stack.top s.stack with
    | None -> raise (InternalError "Tried to add binding with empty symbol table stack.")
    | Some tbl -> begin match ST.add tbl ~key:name ~data with
      | `Duplicate -> raise (DeclError (name ^ " is already declared in the current scope."))
      | `Ok -> () end

  let lookup s name = Stack.find_map s.stack (Fn.flip ST.find name)

  let lookup_topmost s name = match Stack.top s.stack with
    | None -> raise (InternalError "Tried to look up binding with empty symbol table stack.")
    | Some tbl -> ST.find tbl name

  let close_scope { stack; info } pos = match Stack.pop stack with
    | None -> raise (InternalError "Tried to exit global scope.")
    | Some tbl -> Option.iter info (ST.dump tbl pos)

end

module STS = Make_symtbl_stack(Hash_symtbl)

(* Type of data stored in our symbol table *)
type tp_or_id = Tp of tp | Id of tp | Fn of tp

let string_of_tp_or_id : tp_or_id -> string = function
  | Tp t | Id t | Fn t -> pretty_tp t Zero

(* Helper functions for typechecking *)

let rec inner_tp sts : tp -> tp option = function
  | TypeVar (name, _) -> begin match STS.lookup sts name with
    | Some (Tp t0) -> Some t0
    | _ -> None end
  | _ -> None

and base_tp sts (t : tp) : tp option = match t with
  | TypeVar (name, tor) -> begin match inner_tp sts t with
    | Some (TypeVar (t, tor) as t') -> base_tp sts t'
    | t -> t end
  | t -> Some t

let rec remove_underscores il el = match il, el with
  | [], [] -> [], []
  | i::il', e::el' ->
    if i = "_"
    then remove_underscores il' el'
    else let (il'', el'') = remove_underscores il' el' in i::il'', e::el''
  | _ -> raise (InternalError "Unequal list lengths in remove_underscores")

(* Verifies that a symbol was really a tp, return its type if it is, returns an error otherwise *)
let tp_symb i ti = match ti with
  | Tp t -> t
  | Fn _ -> raise (TypeError ("A type was expected, but " ^ i ^ " is a function."))
  | Id _ -> raise (TypeError ("A type was expected, but " ^ i ^ " is an id."))

(* Verifies that a symbol was really a fn, return its type if it is, returns an error otherwise *)
let fn_symb i ti = match ti with
  | Tp _ -> raise (TypeError ("A function was expected, but " ^ i ^ " is a type."))
  | Fn (FuncTp (t1, t2)) -> (FuncTp (t1, t2))
  | Fn _ -> raise (InternalError ("A function had a type not associated with functions"))
  | Id _ -> raise (TypeError ("A function was expected, but " ^ i ^ " is an id."))

(* Verifies that a symbol was really an id, return its type if it is, returns an error otherwise *)
let id_symb i ti = match ti with
  | Tp _ -> raise (TypeError ("An id was expected, but " ^ i ^ " is a type."))
  | Fn _ -> raise (TypeError ("An id was expected, but " ^ i ^ " is a function."))
  | Id t -> t

(* Type classes for checking operators *)
let rec comparable sts : tp -> bool = function
  | Bool | Int | Float64 | Rune | String -> true
  | TypeVar (i, tor) -> tor := (base_tp sts (TypeVar (i, tor)));
    (match STS.lookup sts i with
     | None -> raise (InternalError ("Type identifier " ^ i ^
                                     "should be in symbol table before checking properties."))
     | Some ti -> comparable sts (tp_symb i ti)
    )
  | TStruct vssl -> List.for_all vssl ~f:(fun x -> let (idl, t) = x in comparable sts t)
  | TArray (n, t) -> comparable sts t
  | TSlice _ -> false
  | FuncTp _ -> false
  | Void -> false (* TODO Check if Void is comparable *)

let rec ordered sts : tp -> bool = function
  | Bool -> false
  | Int | Float64 | Rune | String -> true
  | TypeVar (i, tor) -> tor := (base_tp sts (TypeVar (i, tor)));
    (match STS.lookup sts i with
     | None -> raise (InternalError ("Type identifier " ^ i ^
                                     "should be in symbol table before checking properties."))
     | Some ti -> ordered sts (tp_symb i ti)
    )
  | TStruct _ | TArray _ | TSlice _ | FuncTp _ | Void -> false (* TODO verify if Void is ordered *)

let rec numeric sts : tp -> bool = function
  | Int | Float64 | Rune -> true
  | TypeVar (i, tor) ->
    tor := (base_tp sts (TypeVar (i, tor)));
    begin match STS.lookup sts i with
    | None -> raise (InternalError ("Type identifier " ^ i ^
                                    "should be in symbol table before checking properties."))
    | Some ti -> numeric sts (tp_symb i ti)
    end
  | _ -> false

let rec integer sts : tp -> bool = function
  | Int | Rune -> true
  | TypeVar (i, tor) ->
    tor := base_tp sts (TypeVar (i, tor));
    begin match STS.lookup sts i with
    | None -> raise (InternalError ("Type identifier " ^ i ^
                                    "should be in symbol table before checking properties."))
    | Some ti -> integer sts (tp_symb i ti)
    end
  | _ -> false

(* Identifiers that should be predeclared at global scope *)
let primitives : (string * tp_or_id) list =
  [("int", Tp Int);
   ("float64", Tp Float64);
   ("bool", Tp Bool);
   ("rune", Tp Rune);
   ("string", Tp String);
   ("true", Id Bool);
   ("false", Id Bool)]

(* Typechecking *)
let rec check_prog (dump_filename : string option) : prog -> unit = function
    Prog (pos, _, dl) ->
    let dumpinfo = Option.map dump_filename (fun filename -> {filename; string_of_data = string_of_tp_or_id}) in
    let sts = STS.create dumpinfo in
    STS.open_scope sts; (* global scope *)
    List.iter primitives ~f:(fun (name, ti) -> STS.add_binding sts name ti);
    List.iter dl (check_topleveldecl sts);
    STS.close_scope sts pos

and check_topleveldecl sts : topleveldecl -> unit = function
  | FuncDecl (pos, i, vslo, tpo, sl) ->
    let get_arg_typ vss =
      let (vars, t) = vss in List.map vars (fun x -> t) in
    let tl = Option.value_map vslo ~default:[] ~f:(List.concat_map ~f:get_arg_typ) in
    let t = Option.value tpo ~default:Void in
    STS.add_binding sts i (Fn (FuncTp(tl, t)));
    STS.open_scope sts;
    let tl = Option.value_map vslo ~default:[] ~f:(List.concat_map ~f:(check_varspecsimp_func sts)) in
    let t = Option.value ~default:Void tpo in
    List.iter tl
      (* TODO factor out this checking function *)
      (fun x -> if not (test_tp sts x) then
          raise (TypeError ("Type " ^ pretty_tp x Zero ^ " is undeclared, but is an input type for function " ^ i ^ ".")));
    (if not (test_tp sts t) then
       raise (TypeError ("Type " ^ pretty_tp t Zero ^ " is undeclared, but is an input type for function " ^ i ^ ".")));
    List.iter sl ~f:(check_stmt sts);
    STS.close_scope sts pos;
  | Stmt (pos, s) -> check_stmt sts s

and test_tp sts (t : tp) : bool = match t with
  | TypeVar (tp, tor) ->
    tor := base_tp sts (TypeVar (tp, tor));
    let tio = STS.lookup sts tp in
    begin match tio with
    | None -> false
    | Some ti -> ignore (tp_symb tp ti); true end
  | _ -> true

and add_var sts (i :id) (t : tp) : unit =
  match test_tp sts t with
  | false -> raise (TypeError (i ^ " has type " ^ pretty_tp t Zero ^ " but it is undeclared."))
  | true -> STS.add_binding sts i (Id t)

and check_varspecsimp_func sts (vss : varspecsimp) : tp list =
  let (vars, t) = vss in
  List.iter vars (fun i -> add_var sts i t);
  List.map vars (fun x -> t)

and check_varspecsimp sts (vss : varspecsimp) : tp =
  let (vars, t) = vss in
  check_tp sts t;
  List.iter vars (fun i -> add_var sts i t);
  t

and check_stmt sts : stmt -> unit = function
  | Decl (pos, ds) -> check_declstmt sts ds
  | Simple (pos, ss) -> check_simplestmt sts ss
  | Return (pos, eo) -> Option.iter eo (fun e -> ignore (check_expr sts e))
  | Break pos -> ()
  | Continue pos -> ()
  | Block (pos, sl) -> check_block sts pos sl
  | If (pos, ifs) -> check_ifstmt sts ifs
  | Switch (pos, ss) -> check_switchstmt sts ss
  | For (pos, fs) -> check_forstmt sts fs
  | Print (pos, ps) -> check_printstmt sts ps

and check_block sts (pos : Lexing.position) (sl : stmt list) : unit =
  STS.open_scope sts;
  List.iter sl ~f:(check_stmt sts);
  STS.close_scope sts pos

and compare_tps2 sts ((t1, b1) : (tp * bool)) ((t2, b2) : tp * bool) : bool =
  let b = match base_tp sts t1 with
    | Some (TSlice tp) -> false
    | _ -> b1 && b2 in
  if b then t1 = t2 else compare_tps sts t1 t2

and check_simplestmt sts : simplestmt -> unit = function
  | Expr (pos, e) -> ignore (check_expr sts e)
  | Inc (pos, e) | Dec (pos, e) ->
    let (t, b) = check_expr sts e in
    if numeric sts t then
      (if b then () else raise (TypeError "Invalid inc/dec statement"))
    else raise (TypeError "Incrementing non-numeric type.")
  | AssignEquals (pos, ll, el) -> (* TODO: If the RHS has identifiers, it must be equal, otherwise, compare *)
    (try List.iter2_exn
           (List.map ll (Fn.compose fst (check_lvalue sts))) (List.map el (Fn.compose fst (check_expr sts)))
           (fun t1 t2 -> if not (compare_tps sts t1 t2) then
               raise (DeclError ("Cannot assign expression of type " ^ pretty_tp t2 Zero ^
                                 " to lvalue of type " ^ pretty_tp t1 Zero ^ ".")))
     with Invalid_argument _ -> raise (DeclError "Tried to assign list of expressions to list of different length."))
  | Assign (pos, op, l, e) -> check_assignop sts op (check_lvalue sts l) (check_expr sts e)
  | AssignVarEquals (pos, il, el) -> (* TODO: Reverify. If the RHS has identifiers, it must be equal, otherwise, compare *)
    let il, el = remove_underscores il el in
    (try List.iter2_exn
           (List.map il (check_id sts)) (List.map el (check_expr sts))
           (fun t1 p2 -> if not (compare_tps2 sts (t1, true) p2) then
               (* TODO: Reverify. Here, we consider that the LHS has an id, and thus it can be comparable iff the RHS has no id, equal otherwise *)
               raise (DeclError ("Cannot assign expression of type " ^ pretty_tp (fst p2) Zero ^
                                 " to lvalue of type " ^ pretty_tp t1 Zero ^ ".")))
     with Invalid_argument _ -> raise (DeclError "Tried to assign list of expressions to list of different length."))
  | AssignVar (pos, op, i, e) -> check_assignop sts op (check_id sts i, true) (check_expr sts e)
  | ShortVarDecl (pos, il, el, dlor) ->
    let declared = List.map il (fun i -> i = "_" || Option.is_some (STS.lookup_topmost sts i)) in
    dlor := Some declared;
    if List.for_all ~f:Fn.id declared then
      raise (DeclError "All variables in short declaration were already declared")
    else
      let exp_types = List.map el (check_expr sts) in
      (* TODO use zip and handle case of unequal lengths *)
      let combined = List.zip_exn (List.zip_exn il declared) exp_types in
      List.iter combined
        (fun ((i, dec), (t', b)) ->
           if dec then
             begin
               match STS.lookup_topmost sts i with
               | Some ti -> let t = id_symb i ti in if compare_tps2 sts (t, true) (t', b) then () else raise (DeclError ("Variable " ^ i ^ " is expected to have type " ^ pretty_tp t Zero ^ " but is assigned type " ^ pretty_tp t' Zero ^ ".")) (* TODO: Reanalyse how we compare the types if there is an identifier *)
               | None -> if i = "_" then () else raise (InternalError "A variable is claimed to be inside of the current scope, but it is not")
             end
           else if t' = Void then raise (TypeError ("Variable " ^ i ^ " assigned void type")) else add_var sts i t')

and check_lvalue sts : lvalue -> tp * bool = function
  | LSel (pos, pe, i, tor) -> check_fieldsel sts pe i tor
  | LArrAccess (pos, pe, e, tor) -> check_arrayaccess sts pe e tor
  | LSlice (pos, pe, eo1, eo2, tor) -> check_slice sts pe eo1 eo2 tor
  | LSliceCap (pos, pe, eo, e1, e2, tor) -> check_slicecap sts pe eo e1 e2 tor

and check_declstmt sts : declstmt -> unit = function
  | VarDecls (pos, vsl) -> List.iter vsl (check_varspec sts)
  | TypeDecls (pos, tsl) -> List.iter tsl (check_typespec sts) (* TODO What about redeclarations? *)

(* This function tests if e has expected type t, raises an error if it doesn't *)
and compare_exp_tp sts (e : expr) (t : tp) : unit =
  let (t', b) = check_expr sts e in  (* TODO: Reverify. Check if any case needs us to actually have the same type *)
  let b' = if b then t = t'
    else compare_tps sts t t' in
  (match b' with
   | true -> ()
   | false -> raise (TypeError ("Expression " ^ pretty_expr e Zero ^ " was expected to have type " ^ pretty_tp t Zero ^ " but had type " ^ pretty_tp t' Zero ^ "." )))

and compare_tps sts (t : tp) (t' : tp) : bool =
  if t = t' then true
  else (match inner_tp sts t with
    | Some t0 -> compare_tps sts t0 t'
    | None -> (match inner_tp sts t' with
      | Some t0 -> compare_tps sts t t0
      | None -> false
    ))

and check_varspec sts : varspec -> unit = function
  | VarSpecTp (pos, vss, elo) ->
    let t = check_varspecsimp sts vss in Option.iter elo (List.iter ~f:(fun e -> compare_exp_tp sts e t)) (* TODO: Recheck whether we should compare type *)
  | VarSpecNoTp (pos, il, el) ->
    try let l = List.zip_exn il el in
      List.iter l
        (fun (i,e) -> let tp = fst (check_expr sts e) in
          if tp = Void then
            raise (TypeError ("Cannot assign expression " ^ pretty_expr e Zero ^ " of type void to variable."))
          else add_var sts i tp)
    with Invalid_argument _ -> raise (InternalError "VarSpecNoTp has lists of different lengths")
(* TODO reverify validity *)

and check_typespec sts : typespec -> unit = function
    TpSpec (pos, name, t) ->
    check_tp sts t;
    if test_tp sts t then
      STS.add_binding sts name (Tp t)
    else
      raise (TypeError ("Created alias " ^ name ^ " for type " ^ pretty_tp t Zero ^ ", but t is undeclared."))

and check_tp sts : tp -> unit = function
  | Int | Float64 | Bool | Rune | String | Void -> ()
  | FuncTp (_, tp) -> check_tp sts tp
  | TypeVar (id, tor) ->
    tor := base_tp sts (TypeVar (id, tor));
    begin match STS.lookup sts id with
    | None -> raise (TypeError "Type alias of undeclared type")
    | Some ti -> check_tp sts (tp_symb id ti)
    end
  | TArray (_, tp) -> check_tp sts tp
  | TSlice tp -> check_tp sts tp
  | TStruct vssl ->
    let check_varspecsimp sts vss =
      let (vars, t) = vss in check_tp sts t
    in
    List.iter vssl (check_varspecsimp sts)

and check_ifcond sts : ifcond -> unit = function
    IfCond (pos, sso, e) -> Option.iter sso (check_simplestmt sts); check_cond sts e

and check_ifstmt sts (ifs : ifstmt) : unit =
  STS.open_scope sts;
  match ifs with
  | IfOnly (pos, ic, sl) ->
    check_ifcond sts ic;
    check_block sts pos sl;
    STS.close_scope sts pos
  | IfElse (pos, ic, sl1, sl2) ->
    check_ifcond sts ic;
    check_block sts pos sl1;
    check_block sts pos sl2;
    STS.close_scope sts pos
  | IfElseIf (pos, ic, sl, is) ->
    check_ifcond sts ic;
    check_block sts pos sl;
    check_ifstmt sts is;
    STS.close_scope sts pos

and check_switchcond sts : switchcond -> tp * bool = function
    SwitchCond (pos, sso, eo) ->
    Option.iter sso (check_simplestmt sts);
    let (tp, b) = Option.value_map eo ~default:(Bool, false) ~f:(check_expr sts) in
    if comparable sts tp then (tp, b)
    else raise (TypeError ("Cannot switch on an expression " ^ Option.value_map eo ~default:"INVALID EXPRESSION" ~f:(fun e -> pretty_expr e Zero) ^ " of incomparable type " ^ pretty_tp tp Zero))

and check_expr_clause sts ((t, b) : tp * bool) (ecc : exprcaseclause) : unit = match ecc with
  | ExprCaseClause (pos1, Case (pos2, el), sl) ->
    List.iter el (check_switch_expr sts t);
    List.iter sl (check_stmt sts)
  (* TODO: Reverify. Here, we make sure the types of the cases and the switch condition are comparable, and then make sure every case has the same type *)
  | ExprCaseClause (pos1, Default pos2, sl) -> List.iter sl (check_stmt sts)

and check_switch_expr sts (t : tp) (e : expr) : unit = let (t', _) = check_expr sts e in
  if (t = t') then ()
  else raise (TypeError ("Expected expression (in switch statement) of type " ^
                         pretty_tp t Zero ^ " but an expression of type " ^
                         pretty_tp t' Zero ^ " was obtained."))

and first_tp_eccl sts : exprcaseclause list -> (expr * (tp * bool)) option = function
  | [] -> None
  | ExprCaseClause (_, Case (_, e::tl), _) :: tl' -> Some (e, check_expr sts e)
  | h :: tl -> first_tp_eccl sts tl

and check_switchstmt sts : switchstmt -> unit = function
    SwitchStmt (pos, sco, eccl) ->
    let (t, b) = Option.value_map sco ~default:(Bool, false) ~f:(check_switchcond sts) in
    let (t, b) = match first_tp_eccl sts eccl with
      | Some (e, (t', b')) -> (if (compare_tps2 sts (t, b) (t', b)) then (t', b || b')
                               else raise (DeclError ("Expression " ^ pretty_expr e Zero ^ " is expected to have type " ^ pretty_tp t Zero ^ " but is assigned type " ^ pretty_tp t' Zero ^ ".")))
      | None -> (t, b)
    in
    List.iter eccl (check_expr_clause sts (t, b))

and check_cond sts (e : expr) : unit =
  let (t, b) = check_expr sts e in
  if compare_tps sts Bool t then ()
  else raise (TypeError ("Expected condition of type Bool, but received condition \"" ^ pretty_expr e Zero ^ "\" of type " ^ pretty_tp t Zero ^ "."))

and check_forstmt sts : forstmt -> unit = function
  | InfLoop (pos, sl) ->
    STS.open_scope sts;
    List.iter sl (check_stmt sts);
    STS.close_scope sts pos
  | WhileLoop (pos, e, sl) ->
    STS.open_scope sts;
    check_cond sts e;
    List.iter sl (check_stmt sts);
    STS.close_scope sts pos
  | ForLoop (pos, sso1, eo, sso2, sl) ->
    STS.open_scope sts;
    Option.iter sso1 (check_simplestmt sts); (* The first arg of the for is only in scope in the loop *)
    Option.iter eo (check_cond sts);
    Option.iter sso2 (check_simplestmt sts);
    List.iter sl (check_stmt sts);
    STS.close_scope sts pos

and check_printstmt sts : printstmt -> unit = function
  | PrintStmt (pos, elo) | PrintlnStmt (pos, elo) ->
    let check_printable (e : expr) : unit =
      let rec check_printable_tps t =
        (begin match t with
           | Int | Float64 | Bool | Rune | String -> ()
           | TypeVar (i, tor) -> tor := base_tp sts (TypeVar (i, tor));
             (match STS.lookup sts i with
              | None -> raise (InternalError ("Type identifier " ^ i ^
                                              "should be in symbol table before checking properties."))
              | Some ti -> check_printable_tps (tp_symb i ti)
             )
           | _ -> raise (TypeError ("Expression " ^ pretty_expr e Zero ^ " has wrong type for printing."))
         end ) in check_printable_tps (fst (check_expr sts e)) in
    Option.iter ~f:(List.iter ~f:check_printable) elo

and check_expr sts e = match e with
  | Unary (pos, ue, tor) ->  let (t, b) = check_unaryexpr sts ue in tor := Some t; (t,b)
  | Binary (pos, op, e1, e2, tor) ->
    let (t1, b1) = check_expr sts e1 in
    let (t2, b2) = check_expr sts e2 in
    if (((not b1 || not b2) && compare_tps sts t1 t2) || t1 = t2) then (* TODO: Should we compare the types or should they be equal? *)
      let (t, b) = check_binop sts op (t1, b1) (t2, b2) in tor := Some t; (t, b) (* TODO: modify check_binop *)
    else raise (TypeError ("Tried to perform binary operation " ^ pretty_binop op ^ " on arguments " ^
                           pretty_expr e1 Zero  ^ " and " ^ pretty_expr e2 Zero ^ " of different types, "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ ", at " ^
                           sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) ^ "."))


and check_unaryexpr sts : unaryexpr -> (tp * bool) = function
  | Primary (pos, pe, tor)  ->
    let (t, b) = check_primaryexpr sts pe in
    tor := Some t; (t, b)
  | UnaryOp (pos, op, ue, tor) ->
    let (t, b) = (check_uop sts op (check_unaryexpr sts ue)) in
    tor := Some t; (t, b)

and check_primaryexpr sts : primaryexpr -> tp * bool = function
  | Operand (pos, o, tor) -> let (t, b) = check_operand sts o in tor := Some t; (t, b)
  | Sel (pos, pe, i, tor) -> check_fieldsel sts pe i tor
  | ArrAccess (pos, pe, e, tor) -> check_arrayaccess sts pe e tor
  | Slice (pos, pe, eo1, eo2, tor) -> check_slice sts pe eo1 eo2 tor
  | SliceCap (pos, pe, eo, e1, e2, tor) -> check_slicecap sts pe eo e1 e2 tor
  | Cast (pos, t, e, tor) ->
    let rec castable t = match t with
      | Int -> ()
      | Float64 -> ()
      | Bool -> ()
      | Rune -> ()
      | TypeVar (i, tor) -> tor := base_tp sts (TypeVar (i, tor));
        let nt = STS.lookup sts i in
        (match nt with
         | None -> raise (TypeError ("Type of " ^ i ^" not found"))
         | Some typ -> castable (tp_symb i typ)
        )
      | _ -> raise (TypeError ("Casting expression " ^ pretty_expr e Zero ^ " with type " ^ pretty_tp t Zero ^ " which is not a proper casting type."))
    in castable t;
    let (t', b') = check_expr sts e in
    castable (t'); (* TODO: Reverify this, especially b' *)
    tor := Some t; (t, b')
  | FunApp (pos, pe, el, tor) ->
    (match pe with
     | Operand (_, Var (_, i, _) , _) -> (match check_id' sts i with

       | Tp t -> (match el with
         | [e] -> check_primaryexpr sts (Cast (pos, t, e, tor))(* Here we have type casting rather than function application *)
         | _ -> raise (TypeError ("Expression " ^ pretty_primary pe Zero ^
                                  " is not a function and cannot be applied.")))
       | _ -> check_funapp sts (check_primaryexpr sts pe) pe el tor)
     | _ -> check_funapp sts (check_primaryexpr sts pe) pe el tor)

  | Append (pos, i, e, tor) ->
    (match base_tp sts (check_id sts i) with
     | Some (TSlice t) ->
       let (tt, b) = check_expr sts e in
       if compare_tps sts tt t then (tor := Some (TSlice t); (TSlice t, true)) (* TODO: Reverify compare and whether we should have true *)
       else raise (TypeError ("Append to " ^ i ^ " of type Slice<" ^
                              pretty_tp t Zero ^ "> requires expression of type " ^
                              pretty_tp t Zero ^ " but received " ^ pretty_expr e Zero ^
                              " of type " ^ pretty_tp tt Zero ^ "."))
     | _ -> raise (TypeError ("Tried to append to identifier " ^ i ^ " of non-slice type."))
    )

and check_funapp sts (p : tp * bool) (pe : primaryexpr) (el : expr list) tor: tp * bool = match p with
  | (FuncTp (tl, rt), b) ->
    (try List.iter2_exn el tl
           (fun e t -> let (tt, b') = check_expr sts e in if compare_tps2 sts (tt, b') (t, b) then () (* TODO: Should we compare the tps? *)
             else raise (TypeError ("Function argument " ^
                                    pretty_expr e Zero ^ " has type " ^ pretty_tp tt Zero ^
                                    " but " ^ pretty_primary pe Zero ^ " expected type " ^
                                    pretty_tp t Zero ^ "."))
           )
     with Invalid_argument _ ->
       raise (TypeError ("Wrong number of arguments to " ^ pretty_primary pe Zero ^ "."))
    ); tor := Some rt; (rt, true) (* TODO: Should we consider function application as a variable for type aliases? *)
  | _ -> raise (TypeError ("Expression " ^ pretty_primary pe Zero ^
                           " is not a function and cannot be applied."))

and check_operand sts : operand -> (tp * bool) = function
  | Parens (pos, e, tor) -> let (t, b) = check_expr sts e in tor := Some t; (t,b)
  | Var (pos, i, tor) -> let t = check_id sts i in tor := Some t; (t, true)
  | IntLit (pos, _, tor) -> tor := Some Int; (Int, false)
  | FloatLit (pos, _, tor) -> tor := Some Float64; (Float64, false)
  (* TODO what about bool literals? *)
  | RuneLit (pos, _, tor) -> tor := Some Rune; (Rune, false)
  | StrLit (pos, _, tor) -> tor := Some String; (String, false)

and vss_lookup (i : id) (vss : varspecsimp) : tp option = match vss with
  | (il, t) -> if (List.mem il i) then (Some t) else None

and struct_lookup (i : id) (vssl : varspecsimp list) : tp = match vssl with
  | [] -> raise (TypeError ("Expected field " ^ i ^ "in structure " ^ pretty_varspecsimp_list vssl Zero ^ "."))
  | vss :: vssl -> (match vss_lookup i vss with
    | Some t -> t
    | None -> struct_lookup i vssl)

and check_fieldsel sts (pe : primaryexpr) (i : id) (tor : tp option ref) : tp * bool =
  let (t, b) = check_primaryexpr sts pe in
  let rec inner t = (match t with
    | TStruct vssl -> let t' = struct_lookup i vssl in tor := Some t'; t'
    | TypeVar (i, tor) -> tor := base_tp sts (TypeVar (i, tor)); let tio = STS.lookup sts i in (match tio with
      | None -> raise (TypeError ("Primary Expression " ^ pretty_primary pe Zero ^ " should have type " ^ i ^ ", but type " ^ i ^ " is not in the current scope."))
      | Some ti -> let t = tp_symb i ti in inner t)
    | _ -> raise (TypeError ("Tried to access field of non-struct expression "
                             ^ pretty_primary pe Zero ^ ".")))
  in (inner t, b) (* TODO: Reverify whether it should be b *)

and check_arrayaccess sts (pe : primaryexpr) (e : expr) (tor : tp option ref) : tp * bool =
  match check_primaryexpr sts pe with
  | (TArray (_, t), b) | (TSlice t, b) ->
    begin match check_expr sts e with
    | (Int, b') -> tor := Some t; (t, b) (* TODO: reverify this*)
    | _ -> raise (TypeError ("Incorrect attempt to access element of an array at non-integer position "
                             ^ pretty_expr e Zero ^ "."))
    end
  | _ -> raise (TypeError ("Tried to index non-array, non-slice expression " ^ pretty_primary pe Zero ^ "."))

and check_slice sts (pe : primaryexpr) (eo1 : expr option) (eo2 : expr option) (tor : tp option ref) : tp * bool =
  let (t, b) = check_primaryexpr sts pe in match base_tp sts t with
  | Some TArray (_, t) | Some TSlice t ->
    let f = Option.iter
              ~f:(fun e -> (match check_expr sts e with
                | (Int, b') -> ()
                | _ -> raise (TypeError ("Incorrect attempt to create a slice using non-integer index" ^
                                         pretty_expr e Zero ^ ".")))
              ) in
    f eo1; f eo2; tor := Some (TSlice t); (TSlice t, b)
  | _ -> raise (TypeError ("Incorrect attempt to slice a non-array, non-slice expression "
                           ^ pretty_primary pe Zero ^ "."))

and check_slicecap sts (pe : primaryexpr) (eo : expr option) (e1 : expr) (e2 : expr) (tor : tp option ref) : tp * bool =
  let (t, b) = check_primaryexpr sts pe in match base_tp sts t with
  | Some TArray (_, t) | Some TSlice t -> let f e =
                                            (match check_expr sts e with
                                             | (Int, b') -> ()
                                             | _ -> raise (TypeError ("Incorrect attempt to create a slice using non-integer index" ^
                                                                      pretty_expr e Zero ^ "."))
                                            ) in
    Option.iter eo f; f e1; f e2; tor := Some (TSlice t); (TSlice t, b)
  | _ -> raise (TypeError ("Incorrect attempt to slice a non-array, non-slice expression "
                           ^ pretty_primary pe Zero ^ "."))

and check_uop sts (op : uop) ((t, b) : tp * bool) : tp * bool = match op with
  | UPlus | UMinus ->
    if numeric sts t then (t, b)
    else raise (TypeError ("Unary operator " ^ pretty_uop op ^
                           " requires a numeric expression but was provided one of type "
                           ^ pretty_tp t Zero ^ "."))
  | LNot ->
    if compare_tps sts t Bool then (t, b)  (* TODO: Should we compare the tps? *)
    else raise (TypeError ("Unary operator " ^ pretty_uop op ^
                           " requires a bool but was provided a "
                           ^ pretty_tp t Zero ^ "."))
  | UBitXor ->
    if integer sts t then (t, b)
    else raise (TypeError ("Unary operator " ^ pretty_uop op ^
                           " requires an integer expression but was provided one of type "
                           ^ pretty_tp t Zero ^ "."))

and check_binop sts (op : binop) ((t1, b1) : tp * bool) ((t2, b2) : tp * bool) : tp * bool = match op with  (* TODO: Reverify validity *)
  | LOr | LAnd ->
    if compare_tps sts t1 Bool && compare_tps2 sts (t1, b1) (t2, b2) then let t = Bool in (t, false)
    else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                           " requires bool expressions but was provided ones of type "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | CmpEq | NotEq ->
    if comparable sts t1 && compare_tps2 sts (t1, b1) (t2, b2) then let t = Bool in (t, false)
    else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                           " requires comparable expressions but was provided ones of type "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | LT | GT | LTE | GTE ->
    if ordered sts t1 && compare_tps2 sts (t1, b1) (t2, b2) then let t = Bool in (t, false)
    else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                           " requires ordered expressions but was provided ones of type "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | Plus ->
    if (numeric sts t1 || compare_tps sts t1 String) && compare_tps2 sts (t1, b1) (t2, b2) then let t = if b1 then t1 else t2 in (t, b1 || b2)
    else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                           " requires numeric or string expressions but was provided ones of type "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | Minus | Times | Div | Mod ->
    if numeric sts t1 && compare_tps2 sts (t1, b1) (t2, b2) then let t = if b1 then t1 else t2 in (t, b1 || b2)
    else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                           " requires numeric expressions but was provided ones of type "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | BitOr | BitAnd | BitXOr | BitClr | LShift | RShift ->
    if integer sts t1 && compare_tps2 sts (t1, b1) (t2, b2) then let t = if b1 then t1 else t2 in (t, b1 || b2)
    else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                           " requires integer expressions but was provided ones of type "
                           ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))

and check_assignop sts (aop : assignop) (t1 : tp * bool)  (t2 : tp * bool) : unit =  (* TODO: Verify if what is commented out is needed *)
  let bop = match aop with
    | PlusEq -> Plus | MinusEq -> Minus | TimesEq -> Times | DivEq -> Div
    | ModEq -> Mod | AndEq -> BitAnd | OrEq -> BitOr | XOrEq -> BitXOr
    | LShiftEq -> LShift | RShiftEq -> RShift | ClrEq -> BitClr in
  (*let t =*) ignore (check_binop sts bop t1 t2) (*in
                                                   if compare_tps t t1 then ()
                                                   else raise (TypeError ("Assign operation " ^ pretty_assop aop ^ " has expressions of type " ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))*)

and check_id sts : id -> tp = fun name ->
  match STS.lookup sts name with
  | None -> raise (TypeError ("Identifier " ^ name ^ " undeclared."))
  | Some ti -> match ti with
    | Id t -> t
    | Fn t -> t
    | _ -> raise (TypeError ("1.A Var or Fn was expected, but " ^ name ^ " was received, which is a type."))

and check_id' sts : id -> tp_or_id = fun name ->
  match STS.lookup sts name with
  | None -> raise (TypeError ("Identifier " ^ name ^ " undeclared."))
  | Some ti -> ti
