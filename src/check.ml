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
open Ast
open Pretty
open Printf

exception TypeError of string
exception DeclError of string
exception InternalError of string

type tp_or_id = 
  | Tp of tp | Id of tp | Fn of tp

(* Assign an option reference and return the assigned value *)
let set (r : 'a option ref) (v : 'a) : 'a = r := Some v; v

module SymTbl = struct

  let dump = ref false
  let filename = ref ""

  exception Found of tp_or_id (* for search in symtable stack with early exit *)

  let line_of_pos pos = sprintf "Scope that begins on line %d" pos.pos_lnum

  let clear_file () = 
    let filename' = String.sub !filename 0 (String.rindex !filename '.') ^ ".symtab" in
    let oc = open_out_gen [Open_creat; Open_trunc] 0o666 filename'
    in close_out oc

	(* Create the symbol table stack and related functions *)
  let string_of_entry k v = match v with
    | Tp t | Id t | Fn t -> k ^ " -> " ^ pretty_tp t Zero

  let print_to_file pos s = 
    let filename' = String.sub !filename 0 (String.rindex !filename '.') ^ ".symtab" in
    let oc = open_out_gen [Open_append] 0o666 filename' in
      fprintf oc "%s\n%s\n\n" (line_of_pos pos) s;
      close_out oc

	let symTblStack = Stack.create ()

	let enter_scope () : unit = Stack.push (Hashtbl.create 0) symTblStack

	let exit_scope pos : unit =
    try     
      let tbl = Stack.pop symTblStack in 
        if (!dump) then
          let l = Hashtbl.fold (fun k v acc -> (k,v)::acc) tbl [] in
          print_to_file pos (String.concat "\n" (List.map (fun (k, v) -> string_of_entry k v) l))
        else ()
    with Stack.Empty -> raise (InternalError "Tried to exit global scope.")

	let add (name : id) (ti : tp_or_id) : unit =
    if Hashtbl.mem (Stack.top symTblStack) name then
      raise (DeclError (name ^ " is already declared in the current scope."))
    else
      try Hashtbl.add (Stack.top symTblStack) name ti
      with Stack.Empty -> raise (InternalError "Tried to add binding to empty symbol table stack.")

  let get_curr (name : id) : tp_or_id option = 
    try Some (Hashtbl.find (Stack.top symTblStack) name)
    with Not_found -> None

	let get (name : id) : tp_or_id option =
    let get_in_symtbl st =
      try let t = Hashtbl.find st name in 
        raise (Found t)
      with Not_found -> () in
    try Stack.iter get_in_symtbl symTblStack; None
    with Found t -> Some t

  (* Open universe block *)
  let _ = Stack.push (Hashtbl.create 0) symTblStack

  (* Add predeclared identifiers *)
  (* Types *)
  let _ = add "int" (Tp Int)
  let _ = add "float64" (Tp Float64)
  let _ = add "bool" (Tp Bool)
  let _ = add "rune" (Tp Rune)
  let _ = add "string" (Tp String)
  (* Constants *)
  let _ = add "true" (Id Bool)
  let _ = add "false" (Id Bool)


(*
	let top = Stack.top symTblStack
	let clear () = Stack.clear symTblStack
	let copy = Stack.copy symTblStack
	let is_empty = Stack.is_empty symTblStack
	let length = Stack.length symTblStack
	let iter f = Stack.iter f symTblStack
*)
end

let rec inner_tp (t : tp) : (tp option) = match t with
  | TypeVar (name, _) -> (match SymTbl.get name with 
         | Some Tp t0 -> Some t0
         | _ -> None
        )
  | _ -> None

and base_tp (t : tp) : (tp option) = match t with 
  | TypeVar (name, tor) -> (match inner_tp (TypeVar (name, tor))  with
         | None -> None
         | Some (TypeVar (t, tor)) -> base_tp (TypeVar (t, tor))
         | Some t -> Some t)
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
let rec comparable : tp -> bool = function
  | Bool | Int | Float64 | Rune | String -> true
  | TypeVar (i, tor) -> tor := (base_tp (TypeVar (i, tor)));
      (match SymTbl.get i with
       | None -> raise (InternalError ("Type identifier " ^ i ^
                        "should be in symbol table before checking properties."))
       | Some ti -> comparable (tp_symb i ti)
      )
  | TStruct vssl -> List.fold_right (fun a b -> a && b) (List.map (fun x -> let (idl, t) = x in comparable t) vssl) true (* TODO reverify validity *)
  | TArray (n, t) -> comparable t
  | TSlice _ -> false
  | FuncTp _ -> false
  | Void -> false (* TODO Check if Void is comparable *)

let rec ordered : tp -> bool = function
  | Bool -> false
  | Int | Float64 | Rune | String -> true
  | TypeVar (i, tor) -> tor := (base_tp (TypeVar (i, tor)));
      (match SymTbl.get i with
       | None -> raise (InternalError ("Type identifier " ^ i ^
                        "should be in symbol table before checking properties."))
       | Some ti -> ordered (tp_symb i ti)
      )
  | TStruct _ | TArray _ | TSlice _ | FuncTp _ | Void -> false (* TODO verify if Void is ordered *)

let rec numeric : tp -> bool = function
  | Int | Float64 | Rune -> true
  | TypeVar (i, tor) -> tor := (base_tp (TypeVar (i, tor)));
      (match SymTbl.get i with
       | None -> raise (InternalError ("Type identifier " ^ i ^
                        "should be in symbol table before checking properties."))
       | Some ti -> numeric (tp_symb i ti)
      )

  | _ -> false

let rec integer : tp -> bool = function
  | Int | Rune -> true
  | TypeVar (i, tor) -> tor := base_tp (TypeVar (i, tor));
      (match SymTbl.get i with
       | None -> raise (InternalError ("Type identifier " ^ i ^
                        "should be in symbol table before checking properties."))
       | Some ti -> integer (tp_symb i ti)
      )
  | _ -> false

(* Typechecking *)
let rec check_prog : prog -> unit = function
  Prog (pos, p, dl) -> List.iter check_topleveldecl dl;
  (* At the very end, exit the universal scope to dump the final symbol table *)
  SymTbl.exit_scope pos

and check_topleveldecl : topleveldecl -> unit = function
  | FuncDecl (pos, i, vslo, tpo, sl) ->
      let get_arg_typ vss =
        let (vars, t) = vss in List.map (fun x -> t) vars
      in let tl = Util.omap (fun vsl -> List.flatten (List.map get_arg_typ vsl)) [] vslo in
      let t = Util.omap (fun x -> x) Void tpo in
      SymTbl.add i (Fn (FuncTp(tl, t)));
     SymTbl.enter_scope ();
     let tl = Util.omap (fun vsl -> List.flatten (List.map check_varspecsimp_func vsl)) [] vslo in
     let t = Util.omap (fun x -> x) Void tpo in
     ignore (List.map (fun x -> if test_tp x then () else raise (TypeError ("Type " ^ pretty_tp x Zero ^ " is undeclared, but is an input type for function " ^ i ^ "."))) tl); ignore ((fun x -> if test_tp x then () else raise (TypeError ("Type " ^ pretty_tp x Zero ^ " is undeclared, but is an input type for function " ^ i ^ "."))) t);     
     List.iter check_stmt sl;
     SymTbl.exit_scope pos;
  | Stmt (pos, s) -> check_stmt s

and test_tp (t : tp) : bool =
(match t with
  | TypeVar (tp, tor) -> tor := base_tp (TypeVar (tp, tor));let tio = SymTbl.get tp in (match tio with
					     | None -> false
					     | Some ti -> ignore (tp_symb tp ti);true)
  | _ -> true
)

and add_var (i :id) (t : tp) : unit =
  match test_tp t with
  | false -> raise (TypeError (i ^ " has type " ^ pretty_tp t Zero ^ " but it is undeclared."))
  | true -> SymTbl.add i (Id t)

and check_varspecsimp_func (vss : varspecsimp) : tp list =
  let (vars, t) = vss in
    List.iter (fun i -> add_var i t) vars;
    List.map (fun x -> t) vars

and check_varspecsimp (vss : varspecsimp) : tp =
  let (vars, t) = vss in
    check_tp t;
    List.iter (fun i -> add_var i t) vars;
    t

and check_stmt : stmt -> unit = function
  | Decl (pos, ds) -> check_declstmt ds
  | Simple (pos, ss) -> check_simplestmt ss
  | Return (pos, eo) -> Util.omap (fun e -> ignore (check_expr e)) () eo
  | Break pos -> ()
  | Continue pos -> ()
  | Block (pos, sl) -> check_block pos sl
  | If (pos, ifs) -> check_ifstmt ifs
  | Switch (pos, ss) -> check_switchstmt ss
  | For (pos, fs) -> check_forstmt fs
  | Print (pos, ps) -> check_printstmt ps

and check_block (pos : Lexing.position) (sl : stmt list) : unit =
  SymTbl.enter_scope ();
  List.iter check_stmt sl;
  SymTbl.exit_scope pos

and compare_tps2 ((t1, b1) : (tp * bool)) ((t2, b2) : tp * bool) : bool =
  let b = match base_tp t1 with  
  | Some (TSlice tp) -> false
  | _ -> b1 && b2 in
  if b then t1 = t2
  else compare_tps t1 t2

and check_simplestmt : simplestmt -> unit = function
  | Expr (pos, e) -> ignore (check_expr e)
  | Inc (pos, e) | Dec (pos, e) -> let (t, b) = check_expr e in if numeric t then (if b then () else raise (TypeError "Invalid inc/dec statement")) else raise (TypeError "Incrementing non-numeric type.")
  | AssignEquals (pos, ll, el) -> (* TODO: If the RHS has identifiers, it must be equal, otherwise, compare *)      
      (try List.iter2
            (fun t1 t2 -> if compare_tps t1 t2 then ()
                          else raise (DeclError ("Cannot assign expression of type " ^ pretty_tp t2 Zero ^
                                                 " to lvalue of type " ^ pretty_tp t1 Zero ^ ".")))
            (List.map fst (List.map check_lvalue ll)) (List.map fst (List.map check_expr el))
       with Invalid_argument _ -> raise (DeclError "Tried to assign list of expressions to list of different length."))
  | Assign (pos, op, l, e) -> check_assignop op (check_lvalue l) (check_expr e)
  | AssignVarEquals (pos, il, el) -> (* TODO: Reverify. If the RHS has identifiers, it must be equal, otherwise, compare *)
      let il', el' = remove_underscores il el in
      (try List.iter2
            (fun t1 p2 -> if compare_tps2 (t1, true) (p2) then () (* TODO: Reverify. Here, we consider that the LHS has an id, and thus it can be comparable iff the RHS has no id, equal otherwise *)
                          else raise (DeclError ("Cannot assign expression of type " ^ pretty_tp (fst p2) Zero ^
                                                 " to lvalue of type " ^ pretty_tp t1 Zero ^ ".")))
            (List.map check_id il') (List.map check_expr el')
       with Invalid_argument _ -> raise (DeclError "Tried to assign list of expressions to list of different length."))
  | AssignVar (pos, op, i, e) -> check_assignop op (check_id i, true) (check_expr e)
  | ShortVarDecl (pos, il, el, dlor) -> 
    let declared = List.map (fun i -> if i = "_" then true else Util.omap (fun _ -> true) false (SymTbl.get_curr i)) il in
      let _ = dlor := Some declared in
      if (List.fold_left ( && ) true declared) then raise (DeclError "All variables in short declaration were already declared")
      else 
        let exp_types = List.map check_expr el in
        let combined = List.combine (List.combine il declared) exp_types in
          List.iter 
          (fun ((i, dec), (t', b)) ->
            if dec then
              begin		          
                match SymTbl.get_curr i with
                | Some ti -> let t = id_symb i ti in if compare_tps2 (t, true) (t', b) then () else raise (DeclError ("Variable " ^ i ^ " is expected to have type " ^ pretty_tp t Zero ^ " but is assigned type " ^ pretty_tp t' Zero ^ ".")) (* TODO: Reanalyse how we compare the types if there is an identifier *)
		            | None -> if i = "_" then () else raise (InternalError "A variable is claimed to be inside of the current scope, but it is not")
              end
            else if t' = Void then raise (TypeError ("Variable " ^ i ^ " assigned void type")) else add_var i t'
          )
          combined

and check_lvalue : lvalue -> tp * bool = function
  | LSel (pos, pe, i, tor) -> check_fieldsel pe i tor
  | LArrAccess (pos, pe, e, tor) -> check_arrayaccess pe e tor
  | LSlice (pos, pe, eo1, eo2, tor) -> check_slice pe eo1 eo2 tor
  | LSliceCap (pos, pe, eo, e1, e2, tor) -> check_slicecap pe eo e1 e2 tor

and check_declstmt : declstmt -> unit = function
  | VarDecls (pos, vsl) -> List.iter check_varspec vsl
  | TypeDecls (pos, tsl) -> List.iter check_typespec tsl (* TODO What about redeclarations? *)

(* This function tests if e has expected type t, raises an error if it doesn't *)
and compare_exp_tp (e : expr) (t : tp) : unit = let (t', b) = check_expr e in  (* TODO: Reverify. Check if any case needs us to actually have the same type *)
   let b' = if b then t = t'
	    else compare_tps t t' in
   (match b' with
   | true -> ()
   | false -> raise (TypeError ("Expression " ^ pretty_expr e Zero ^ " was expected to have type " ^ pretty_tp t Zero ^ " but had type " ^ pretty_tp t' Zero ^ "." )))

and compare_tps (t : tp) (t' : tp) : bool = 
  if t = t' then true
  else (match inner_tp t with
	     | Some t0 -> compare_tps t0 t'
	     | None -> (match inner_tp t' with
		     | Some t0 -> compare_tps t t0
		     | None -> false
		    ))

and check_varspec : varspec -> unit = function
  | VarSpecTp (pos, vss, elo) ->
    let t = check_varspecsimp vss in Util.omap (List.iter (fun e -> compare_exp_tp e t)) () elo (* TODO: Recheck whether we should compare type *)
  | VarSpecNoTp (pos, il, el) -> 
      try let l = List.combine il el in List.iter (fun (i,e) -> let tp = fst (check_expr e) in if tp = Void then raise (TypeError ("Cannot assign expression " ^ pretty_expr e Zero ^ " of type void to variable.")) else add_var i tp) l
			    with Invalid_argument _ -> raise (InternalError "VarSpecNoTp has lists of different lengths")
   (* TODO reverify validity *)

and check_typespec : typespec -> unit = 
  function TpSpec (pos, name, t) -> check_tp t;
  if test_tp t then SymTbl.add name (Tp t) else raise (TypeError ("Created alias " ^ name ^ " for type " ^ pretty_tp t Zero ^ ", but t is undeclared."))

and check_tp : tp -> unit = function
| Int | Float64 | Bool | Rune | String | Void -> ()
| FuncTp (_, tp) -> check_tp tp
| TypeVar (id, tor) -> tor := base_tp (TypeVar (id, tor));(match SymTbl.get id with
                 | None -> raise (TypeError "Type alias of undeclared type")
                 | Some ti -> check_tp (tp_symb id ti)
  )
| TArray (_, tp) -> check_tp tp
| TSlice tp -> check_tp tp
| TStruct vssl -> 
  let check_varspecsimp vss =
    let (vars, t) = vss in check_tp t
  in
  List.iter check_varspecsimp vssl

and check_ifcond : ifcond -> unit = function
  IfCond (pos, sso, e) -> Util.omap check_simplestmt () sso; check_cond e

and check_ifstmt (ifs : ifstmt) : unit =
  SymTbl.enter_scope ();
  match ifs with
  | IfOnly (pos, ic, sl) ->       
      check_ifcond ic; 
      check_block pos sl;
      SymTbl.exit_scope pos
  | IfElse (pos, ic, sl1, sl2) ->      
      check_ifcond ic;
      check_block pos sl1;
      check_block pos sl2;
      SymTbl.exit_scope pos      
  | IfElseIf (pos, ic, sl, is) ->      
      check_ifcond ic;
      check_block pos sl;
      check_ifstmt is;      
      SymTbl.exit_scope pos

and check_switchcond : switchcond -> tp * bool = function
  SwitchCond (pos, sso, eo) ->
    Util.omap check_simplestmt () sso;
    let (tp, b) = Util.omap check_expr (Bool, false) eo in if comparable tp then (tp, b) else raise (TypeError ("Cannot switch on an expression " ^ Util.omap (fun e -> pretty_expr e Zero) "INVALID EXPRESSION" eo ^ " of incomparable type " ^ pretty_tp tp Zero))

and check_expr_clause ((t, b) : tp * bool) (ecc : exprcaseclause) : unit = match ecc with
  | ExprCaseClause (pos1, Case (pos2, el), sl) ->
      List.iter (check_switch_expr t) el; List.iter check_stmt sl (* TODO: Reverify. Here, we make sure the types of the cases and the switch condition are comparable, and then make sure every case has the same type *)
  | ExprCaseClause (pos1, Default pos2, sl) -> List.iter check_stmt sl

and check_switch_expr (t : tp) (e : expr) : unit = let (t', _) = check_expr e in
  if (t = t') then ()
  else raise (TypeError ("Expected expression (in switch statement) of type " ^
                         pretty_tp t Zero ^ " but an expression of type " ^
                         pretty_tp t' Zero ^ " was obtained."))

and first_tp_eccl : exprcaseclause list -> (expr * (tp * bool)) option = function 
  | [] -> None
  | ExprCaseClause (_, Case (_, e::tl), _) :: tl' -> Some (e, check_expr e)
  | h :: tl -> first_tp_eccl tl

and check_switchstmt : switchstmt -> unit = function
  SwitchStmt (pos, sco, eccl) ->
    let (t, b) = Util.omap check_switchcond (Bool, false) sco in
    let (t, b) = match first_tp_eccl eccl with
    | Some (e, (t', b')) -> (if (compare_tps2 (t, b) (t', b)) then (t', b || b') 
         else raise (DeclError ("Expression " ^ pretty_expr e Zero ^ " is expected to have type " ^ pretty_tp t Zero ^ " but is assigned type " ^ pretty_tp t' Zero ^ "."))) 
    | None -> (t, b)
in    
    List.iter (check_expr_clause (t, b)) eccl

and check_cond (e : expr) : unit = 
  let (t, b) = check_expr e in
  if compare_tps Bool t then () 
  else raise (TypeError ("Expected condition of type Bool, but received condition \"" ^ pretty_expr e Zero ^ "\" of type " ^ pretty_tp t Zero ^ "."))

and check_forstmt : forstmt -> unit = function
  | InfLoop (pos, sl) -> 
      SymTbl.enter_scope ();
      List.iter check_stmt sl;
      SymTbl.exit_scope pos
  | WhileLoop (pos, e, sl) ->     
      SymTbl.enter_scope ();
      check_cond e; 
      List.iter check_stmt sl;
      SymTbl.exit_scope pos
  | ForLoop (pos, sso1, eo, sso2, sl) ->
      SymTbl.enter_scope ();      
      Util.omap check_simplestmt () sso1; (* The first arg of the for is only in scope in the loop *)
      Util.omap check_cond () eo;
      Util.omap check_simplestmt () sso2;
      List.iter check_stmt sl;
      SymTbl.exit_scope pos

and check_printstmt : printstmt -> unit = function
  | PrintStmt (pos, elo) | PrintlnStmt (pos, elo) ->
      let check_printable (e : expr) : unit =
	let rec check_printable_tps t =
        (begin match t with	
        | Int | Float64 | Bool | Rune | String -> ()
	| TypeVar (i, tor) -> tor := base_tp (TypeVar (i, tor));
	   (match SymTbl.get i with
	    | None -> raise (InternalError ("Type identifier " ^ i ^
					      "should be in symbol table before checking properties."))
	    | Some ti -> check_printable_tps (tp_symb i ti)
	   ) 
        | _ -> raise (TypeError ("Expression " ^ pretty_expr e Zero ^ " has wrong type for printing."))
        end ) in check_printable_tps (fst (check_expr e)) in
      Util.omap (List.iter check_printable) () elo

and check_expr e = match e with
  | Unary (pos, ue, tor) ->  let (t, b) = check_unaryexpr ue in tor := Some t; (t,b)
  | Binary (pos, op, e1, e2, tor) ->
      let (t1, b1) = check_expr e1 in
      let (t2, b2) = check_expr e2 in
      if (((not b1 || not b2) && compare_tps t1 t2) || t1 = t2) then (* TODO: Should we compare the types or should they be equal? *)
      let (t, b) = check_binop op (t1, b1) (t2, b2) in tor := Some t; (t, b) (* TODO: modify check_binop *)
      else raise (TypeError ("Tried to perform binary operation " ^ pretty_binop op ^ " on arguments " ^
                             pretty_expr e1 Zero  ^ " and " ^ pretty_expr e2 Zero ^ " of different types, "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ ", at " ^
                             sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) ^ "."))


and check_unaryexpr : unaryexpr -> (tp * bool) = function
  | Primary (pos, pe, tor)  ->
     let (t, b) = check_primaryexpr pe in
      tor := Some t; (t, b)
  | UnaryOp (pos, op, ue, tor) ->
     let (t, b) = (check_uop op (check_unaryexpr ue)) in
      tor := Some t; (t, b)

and check_primaryexpr : primaryexpr -> tp * bool = function
  | Operand (pos, o, tor) -> let (t, b) = check_operand o in tor := Some t; (t, b)
  | Sel (pos, pe, i, tor) -> check_fieldsel pe i tor
  | ArrAccess (pos, pe, e, tor) -> check_arrayaccess pe e tor
  | Slice (pos, pe, eo1, eo2, tor) -> check_slice pe eo1 eo2 tor
  | SliceCap (pos, pe, eo, e1, e2, tor) -> check_slicecap pe eo e1 e2 tor
  | Cast (pos, t, e, tor) -> 
     let rec castable t = match t with
      | Int -> ()
      | Float64 -> ()
      | Bool -> ()
      | Rune -> ()
      | TypeVar (i, tor) -> tor := base_tp (TypeVar (i, tor));         
        let nt = SymTbl.get i in        
        (match nt with
          | None -> raise (TypeError ("Type of " ^ i ^" not found"))
          | Some typ -> castable (tp_symb i typ)
        )
      | _ -> raise (TypeError ("Casting expression " ^ pretty_expr e Zero ^ " with type " ^ pretty_tp t Zero ^ " which is not a proper casting type."))
    in castable t;
    let (t', b') = check_expr e in
    castable (t'); (* TODO: Reverify this, especially b' *)
    tor := Some t; (t, b') 
  | FunApp (pos, pe, el, tor) -> 
     (match pe with 
     | Operand (_, Var (_, i, _) , _) -> (match check_id' i with
	
         | Tp t -> (match el with 
		    | [e] -> check_primaryexpr (Cast (pos, t, e, tor))(* Here we have type casting rather than function application *)
		    | _ -> raise (TypeError ("Expression " ^ pretty_primary pe Zero ^
					       " is not a function and cannot be applied.")))
	 | _ -> check_funapp (check_primaryexpr pe) pe el tor) 
     | _ -> check_funapp (check_primaryexpr pe) pe el tor)

  | Append (pos, i, e, tor) ->
      (match base_tp (check_id i) with
         | Some (TSlice t) ->
             let (tt, b) = check_expr e in
             if compare_tps tt t then (tor := Some (TSlice t); (TSlice t, true)) (* TODO: Reverify compare and whether we should have true *)
             else raise (TypeError ("Append to " ^ i ^ " of type Slice<" ^
                                    pretty_tp t Zero ^ "> requires expression of type " ^
                                    pretty_tp t Zero ^ " but received " ^ pretty_expr e Zero ^
                                    " of type " ^ pretty_tp tt Zero ^ "."))
         | _ -> raise (TypeError ("Tried to append to identifier " ^ i ^ " of non-slice type."))
      )

and check_funapp (p : tp * bool) (pe : primaryexpr) (el : expr list) tor: tp * bool = match p with
  | (FuncTp (tl, rt), b) ->
     (try List.iter2
            (fun e t -> let (tt, b') = check_expr e in if compare_tps2 (tt, b') (t, b) then () (* TODO: Should we compare the tps? *)
						       else raise (TypeError ("Function argument " ^
										pretty_expr e Zero ^ " has type " ^ pretty_tp tt Zero ^
										  " but " ^ pretty_primary pe Zero ^ " expected type " ^
										    pretty_tp t Zero ^ "."))
            )
            el tl
      with Invalid_argument _ ->
        raise (TypeError ("Wrong number of arguments to " ^ pretty_primary pe Zero ^ "."))
     ); tor := Some rt; (rt, true) (* TODO: Should we consider function application as a variable for type aliases? *)
  | _ -> raise (TypeError ("Expression " ^ pretty_primary pe Zero ^
                             " is not a function and cannot be applied.")) 									   

and check_operand : operand -> (tp * bool) = function
  | Parens (pos, e, tor) -> let (t, b) = check_expr e in tor := Some t; (t,b)
  | Var (pos, i, tor) -> let t = check_id i in tor := Some t; (t, true)
  | IntLit (pos, _, tor) -> tor := Some Int; (Int, false)
  | FloatLit (pos, _, tor) -> tor := Some Float64; (Float64, false)
  (* TODO what about bool literals? *)
  | RuneLit (pos, _, tor) -> tor := Some Rune; (Rune, false)
  | StrLit (pos, _, tor) -> tor := Some String; (String, false)

and vss_lookup (i : id) (vss : varspecsimp) : tp option = match vss with
  | (il, t) -> if (List.mem i il) then (Some t) else None

and struct_lookup (i : id) (vssl : varspecsimp list) : tp = match vssl with
  | [] -> raise (TypeError ("Expected field " ^ i ^ "in structure " ^ pretty_varspecsimp_list vssl Zero ^ "."))
  | vss :: vssl -> (match vss_lookup i vss with
		| Some t -> t
		| None -> struct_lookup i vssl)
		 
and check_fieldsel (pe : primaryexpr) (i : id) (tor : tp option ref) : tp * bool =
  let (t, b) = check_primaryexpr pe in
  let rec inner t = (match t with
    | TStruct vssl -> set tor (struct_lookup i vssl)
    | TypeVar (i, tor) -> tor := base_tp (TypeVar (i, tor)); let tio = SymTbl.get i in (match tio with
					     | None -> raise (TypeError ("Primary Expression " ^ pretty_primary pe Zero ^ " should have type " ^ i ^ ", but type " ^ i ^ " is not in the current scope."))
					     | Some ti -> let t = tp_symb i ti in inner t)
    | _ -> raise (TypeError ("Tried to access field of non-struct expression "
                             ^ pretty_primary pe Zero ^ ".")))
		      in (inner t, b) (* TODO: Reverify whether it should be b *)

and check_arrayaccess (pe : primaryexpr) (e : expr) (tor : tp option ref) : tp * bool =
  match check_primaryexpr pe with
    | (TArray (_, t), b) | (TSlice t, b) ->
        (match check_expr e with
           | (Int, b') -> tor := Some t; (t, b) (* TODO: reverify this*)
           | _ -> raise (TypeError ("Incorrect attempt to access element of an array at non-integer position "
                                    ^ pretty_expr e Zero ^ "."))
         )
    | _ -> raise (TypeError ("Tried to index non-array, non-slice expression " ^ pretty_primary pe Zero ^ "."))

and check_slice (pe : primaryexpr) (eo1 : expr option) (eo2 : expr option) (tor : tp option ref) : tp * bool =
  let (t, b) = check_primaryexpr pe in match base_tp t with
     | Some TArray (_, t) | Some TSlice t ->
         let f = Util.omap
           (fun e -> (match check_expr e with
              | (Int, b') -> () 
              | _ -> raise (TypeError ("Incorrect attempt to create a slice using non-integer index" ^
                            pretty_expr e Zero ^ ".")))
            ) () in
      f eo1; f eo2; tor := Some (TSlice t); (TSlice t, b)
     | _ -> raise (TypeError ("Incorrect attempt to slice a non-array, non-slice expression "
                              ^ pretty_primary pe Zero ^ "."))

and check_slicecap (pe : primaryexpr) (eo : expr option) (e1 : expr) (e2 : expr) (tor : tp option ref) : tp * bool =
  let (t, b) = check_primaryexpr pe in match base_tp t with
  | Some TArray (_, t) | Some TSlice t -> let f e =
					    (match check_expr e with
					     | (Int, b') -> () 
					     | _ -> raise (TypeError ("Incorrect attempt to create a slice using non-integer index" ^
									pretty_expr e Zero ^ "."))
					    ) in
					  Util.omap f () eo; f e1; f e2; tor := Some (TSlice t); (TSlice t, b)
  | _ -> raise (TypeError ("Incorrect attempt to slice a non-array, non-slice expression "
                             ^ pretty_primary pe Zero ^ "."))

and check_uop (op : uop) ((t, b) : tp * bool) : tp * bool = match op with
  | UPlus | UMinus ->
      if numeric t then (t, b)
      else raise (TypeError ("Unary operator " ^ pretty_uop op ^
                             " requires a numeric expression but was provided one of type "
                             ^ pretty_tp t Zero ^ "."))
  | LNot ->
      if compare_tps t Bool then (t, b)  (* TODO: Should we compare the tps? *)
      else raise (TypeError ("Unary operator " ^ pretty_uop op ^
                             " requires a bool but was provided a "
                             ^ pretty_tp t Zero ^ "."))
  | UBitXor ->
      if integer t then (t, b)
      else raise (TypeError ("Unary operator " ^ pretty_uop op ^
                             " requires an integer expression but was provided one of type "
                             ^ pretty_tp t Zero ^ "."))

and check_binop (op : binop) ((t1, b1) : tp * bool) ((t2, b2) : tp * bool) : tp * bool = match op with  (* TODO: Reverify validity *)
  | LOr | LAnd ->
      if compare_tps t1 Bool && compare_tps2 (t1, b1) (t2, b2) then let t = Bool in (t, false)
      else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                             " requires bool expressions but was provided ones of type "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | CmpEq | NotEq ->
      if comparable t1 && compare_tps2 (t1, b1) (t2, b2) then let t = Bool in (t, false)
      else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                             " requires comparable expressions but was provided ones of type "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | LT | GT | LTE | GTE ->
      if ordered t1 && compare_tps2 (t1, b1) (t2, b2) then let t = Bool in (t, false)
      else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                             " requires ordered expressions but was provided ones of type "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | Plus ->
      if (numeric t1 || compare_tps t1 String) && compare_tps2 (t1, b1) (t2, b2) then let t = if b1 then t1 else t2 in (t, b1 || b2)
      else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                             " requires numeric or string expressions but was provided ones of type "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | Minus | Times | Div | Mod ->
      if numeric t1 && compare_tps2 (t1, b1) (t2, b2) then let t = if b1 then t1 else t2 in (t, b1 || b2)
      else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                             " requires numeric expressions but was provided ones of type "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))
  | BitOr | BitAnd | BitXOr | BitClr | LShift | RShift ->
      if integer t1 && compare_tps2 (t1, b1) (t2, b2) then let t = if b1 then t1 else t2 in (t, b1 || b2)
      else raise (TypeError ("Binary operator " ^ pretty_binop op ^
                             " requires integer expressions but was provided ones of type "
                             ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))

and check_assignop (aop : assignop) (t1 : tp * bool)  (t2 : tp * bool) : unit =  (* TODO: Verify if what is commented out is needed *)
  let bop = match aop with
  | PlusEq -> Plus | MinusEq -> Minus | TimesEq -> Times | DivEq -> Div
  | ModEq -> Mod | AndEq -> BitAnd | OrEq -> BitOr | XOrEq -> BitXOr
  | LShiftEq -> LShift | RShiftEq -> RShift | ClrEq -> BitClr in
  (*let t =*) ignore (check_binop bop t1 t2) (*in
  if compare_tps t t1 then ()
  else raise (TypeError ("Assign operation " ^ pretty_assop aop ^ " has expressions of type " ^ pretty_tp t1 Zero ^ " and " ^ pretty_tp t2 Zero ^ "."))*)

and check_id : id -> tp = fun name ->
  match SymTbl.get name with
    | None -> raise (TypeError ("Identifier " ^ name ^ " undeclared."))
    | Some ti -> match ti with 
		 | Id t -> t
		 | Fn t -> t
		 | _ -> raise (TypeError ("1.A Var or Fn was expected, but " ^ name ^ " was received, which is a type."))

and check_id' : id -> tp_or_id = fun name -> 
  match SymTbl.get name with
    | None -> raise (TypeError ("Identifier " ^ name ^ " undeclared."))
    | Some ti -> ti
