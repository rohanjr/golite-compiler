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
open Printf
open Pretty
exception WeedError of string

module ForSwitchWeeding = struct

	let loop = ref 0

	let rec weed (t : prog) : prog =
		let Prog (pos, pkg, tlist) = t in
			Prog (pos, weed_package pkg, List.map weed_topleveldecl tlist)

	and weed_package (p : package) = p

	and weed_topleveldecl (t : topleveldecl) : topleveldecl = match t with
	| FuncDecl (pos, id, vspec, typ, ss) ->
		let vspec' =
			begin
				match vspec with
				| None -> None
				| Some vspec'' -> Some (List.map weed_varspecsimp vspec'')
			end
		in
		let typ' =
			begin
				match typ with
				| None -> None
				| Some typ'' -> Some (weed_tp typ'')
			end
		in FuncDecl(pos, id, vspec', typ', List.map weed_stmt ss)
	| Stmt (pos, s) -> Stmt (pos, weed_stmt s)

	and weed_stmt (s : stmt) : stmt = match s with
		| Decl (pos, ds) -> Decl (pos, weed_declstmt ds)
		| Simple (pos, ss) -> Simple (pos, weed_simplestmt ss)
		| Return (pos, e) ->
			let e' =
		  	begin
		  		match e with
		  		| None -> None
		  		| Some e'' -> Some (weed_expr e'')
		  	end
		 in Return (pos, e')
		| Break pos ->
			if !loop = 0 then raise (WeedError "invalid use of break statement")
			else Break pos
		| Continue pos ->
			if !loop = 0 then raise (WeedError "invalid use of continue statement")
			else Continue pos
		| Block (pos, ss) -> Block (pos, List.map (weed_stmt) ss)
		| If (pos, is) -> If (pos, weed_ifstmt is)
		| Switch (pos, ss) -> Switch (pos, weed_switchstmt ss)
		| For (pos, fs) -> For (pos, weed_forstmt fs)
		| Print (pos, ps) -> Print (pos, weed_printstmt ps)

	 and weed_simplestmt (ss : simplestmt) : simplestmt = match ss with
		| Expr (pos, e) -> Expr (pos, weed_expr e)
		| Inc (pos, e) -> Inc (pos, weed_expr e)
		| Dec (pos, e) -> Dec (pos, weed_expr e)
		| Assign (pos, op, lv, e) -> Assign (pos, op, weed_lvalue lv, weed_expr e)
		| AssignEquals (pos, lvs, es) -> AssignEquals (pos, List.map weed_lvalue lvs, List.map weed_expr es)
		| AssignVar (pos, op, id, e) -> AssignVar (pos, op, id, weed_expr e)
		| AssignVarEquals (pos, ids, es) -> AssignVarEquals (pos, ids, List.map weed_expr es)
		| ShortVarDecl (pos, ids, es, dlor) -> ShortVarDecl (pos, ids, List.map weed_expr es, dlor)

	and weed_lvalue (lval : lvalue)	: lvalue = match lval with
		| LSel (pos, pe, i, r) -> LSel (pos, weed_primaryexpr pe, i, r)
		| LArrAccess (pos, pe, e, r) -> LArrAccess (pos, weed_primaryexpr pe, weed_expr e, r)
		| LSlice (pos, pe, e1, e2, r)->
			let e1' =
				begin
				 	match e1 with
				 	| None -> None
				 	| Some e1'' -> Some (weed_expr e1'')
				 end
			in
			let e2' =
				begin
				 	match e2 with
				 	| None -> None
				 	| Some e2'' -> Some (weed_expr e2'')
				 end
			in
			LSlice (pos, weed_primaryexpr pe, e1', e2', r)
		| LSliceCap (pos, pe, e1, e2, e3, r) ->
			let e1' =
				begin
				 	match e1 with
				 	| None -> None
				 	| Some e1'' -> Some (weed_expr e1'')
				 end
			in
			LSliceCap (pos, weed_primaryexpr pe, e1', weed_expr e2, weed_expr e3, r)

	and weed_declstmt (decl : declstmt) : declstmt = match decl with
		| VarDecls (pos, vspecs) -> VarDecls (pos, List.map weed_varspec vspecs)
		| TypeDecls (pos, tspecs) -> TypeDecls (pos, List.map weed_typespec tspecs)

	and weed_varspec (vspec : varspec) : varspec = match vspec with
		| VarSpecTp (pos, vspecsim, es) ->
			let es' =
				begin
					match es with
					| None -> None
					| Some es'' -> Some (List.map weed_expr es'')
				end
			in VarSpecTp (pos, weed_varspecsimp vspecsim, es')
		| VarSpecNoTp (pos, ids, es) -> VarSpecNoTp (pos, ids, List.map weed_expr es)

	and weed_varspecsimp (vspecsim : varspecsimp) : varspecsimp =
		let (ids, typ) = vspecsim in (ids, weed_tp typ)

	and weed_tp (typ : tp) : tp = match typ with
	 	| Int -> Int
	 	| Float64 -> Float64
	 	| Bool -> Bool
	 	| Rune -> Rune
		| Void -> Void
	 	| String -> String
		| FuncTp (tl, rt) -> FuncTp (List.map weed_tp tl, weed_tp rt)
	 	| TypeVar (i, tor) -> TypeVar (i, tor)
	 	| TSlice (typ) -> TSlice (weed_tp typ)
	 	| TArray (n, typ) -> TArray (n, weed_tp typ)
	 	| TStruct (vspecsims) -> TStruct (List.map weed_varspecsimp vspecsims)

	and weed_typespec (tspec : typespec) : typespec =
		let TpSpec (pos, i, typ) = tspec in TpSpec (pos, i, weed_tp typ)

	and weed_ifstmt (is : ifstmt) : ifstmt = match is with
	  | IfOnly (pos, ic, ss) -> IfOnly (pos, weed_ifcond ic, List.map weed_stmt ss)
	  | IfElse (pos, ic, ss1, ss2) -> IfElse (pos, weed_ifcond ic, List.map weed_stmt ss1, List.map weed_stmt ss2)
	  | IfElseIf (pos, ic, ss, is') -> IfElseIf (pos, weed_ifcond ic, List.map weed_stmt ss, weed_ifstmt is')

	 and weed_ifcond (ic : ifcond) : ifcond =
	 	let IfCond (pos, ss, e) = ic in
	 		let ss' =
	 			begin
	 				match ss with
	 				| None -> None
	 				| Some ss'' -> Some (weed_simplestmt ss'')
	 			end
	 		in
	 		IfCond (pos, ss', weed_expr e)

	 and weed_switchstmt (ss : switchstmt) : switchstmt =
	 	let SwitchStmt (pos, sc, eccs) = ss in
	 		let esclist = List.map (fun x -> let ExprCaseClause (pos, esc, _) = x in esc) eccs in
	 			let deflist = List.filter (fun x -> x = Default pos) esclist in
	 		if (List.length deflist) > 1
	 		then raise (WeedError "invalid number of default cases in switch statement")
	 		else
	 			let sc' =
	 				begin
	 					match sc with
	 					| None -> None
	 					| Some sc'' -> Some (weed_switchcond sc'')
	 				end
	 			in
	 			SwitchStmt (pos, sc', List.map weed_exprcaseclause eccs)

	 and weed_switchcond (sc : switchcond) : switchcond =
	 	let SwitchCond(pos, ss, e) = sc in
	 		let ss' =
	 			begin
	 				match ss with
	 				| None -> None
	 				| Some ss'' -> Some (weed_simplestmt ss'')
	 			end
	 		in
	 		let e' =
	 			begin
	 				match e with
	 				| None -> None
	 				| Some e'' -> Some (weed_expr e'')
	 			end
	 		in SwitchCond (pos, ss', e')

	 and weed_exprcaseclause (ecc : exprcaseclause) : exprcaseclause =
	 	let ExprCaseClause (pos, esc, ss) = ecc in
	 		ExprCaseClause (pos, weed_exprswitchcase esc, List.map weed_stmt ss)

	and weed_exprswitchcase (esc : exprswitchcase) : exprswitchcase = match esc with
		| Case (pos, es) -> Case (pos, List.map weed_expr es)
		| Default pos -> Default pos

	and weed_forstmt (fs : forstmt) : forstmt = match fs with
		| InfLoop (pos, ss) ->
			let _ = loop := !loop + 1 in
			let ss' = List.map weed_stmt ss in
			let _ = loop := !loop - 1 in
			InfLoop (pos, ss')
		| WhileLoop (pos, e, ss) ->
			let _ = loop := !loop + 1 in
			let e' = weed_expr e in
			let ss' = List.map weed_stmt ss in
			let _ = loop := !loop - 1 in
			WhileLoop (pos, e', ss')
		| ForLoop (pos, s1, e, s2, ss) ->
			let _ = loop := !loop + 1 in
			let s1' =
				begin
					match s1 with
					| None -> None
					| Some s1'' -> Some (weed_simplestmt s1'')
				end
			in
			let e' =
				begin
					match e with
					| None -> None
					| Some e'' -> Some (weed_expr e'')
				end
			in
			let s2' =
				begin
					match s2 with
					| None -> None
					| Some s2'' -> Some (weed_simplestmt s2'')
				end
			in
			let ss' = List.map weed_stmt ss in
			let _ = loop := !loop - 1 in
			ForLoop (pos, s1', e', s2', ss')

	and weed_printstmt (ps : printstmt) : printstmt = match ps with
		| PrintStmt (pos, es) ->
			let es' =
				begin
				 	match es with
				 	| None -> None
				 	| Some es'' -> Some (List.map weed_expr es'')
				 end
			in PrintStmt (pos, es')
		| PrintlnStmt (pos, es) ->
			let es' =
				begin
				 	match es with
				 	| None -> None
				 	| Some es'' -> Some (List.map weed_expr es'')
				 end
			in PrintlnStmt (pos, es')

	and weed_expr (e : expr) : expr = match e with
		| Unary (pos, ue, r) -> Unary (pos, weed_unaryexpr ue, r)
		| Binary (pos, op, e1, e2, r) -> Binary (pos, op, weed_expr e1, weed_expr e2, r)

	and weed_unaryexpr (ue : unaryexpr) : unaryexpr = match ue with
		| Primary (pos, pe, r) -> Primary (pos, weed_primaryexpr pe, r)
		| UnaryOp (pos, op, ue', r) -> UnaryOp (pos, op, weed_unaryexpr ue', r)

	and weed_primaryexpr (pe : primaryexpr) : primaryexpr = match pe with
		| Operand (pos, op, r) -> Operand (pos, weed_operand op, r)
		| Sel (pos, pe', i, r) -> Sel (pos, weed_primaryexpr pe', i, r)
		| ArrAccess (pos, pe', e, r) -> ArrAccess (pos, weed_primaryexpr pe', weed_expr e, r)
		| Slice (pos, pe', e1, e2, r) ->
			let e1' =
				begin
					match e1 with
					| None -> None
					| Some e1'' -> Some (weed_expr e1'')
				end
			in
			let e2' =
				begin
					match e2 with
					| None -> None
					| Some e2'' -> Some (weed_expr e2'')
				end
			in
			Slice (pos, weed_primaryexpr pe', e1', e2', r)
		| SliceCap (pos, pe', e1, e2, e3, r) ->
			let e1' =
				begin
					match e1 with
					| None -> None
					| Some e1'' -> Some (weed_expr e1'')
				end
			in
			SliceCap (pos, weed_primaryexpr pe', e1', weed_expr e2, weed_expr e3, r)
		| FunApp (pos, pe', es, r) -> FunApp (pos, weed_primaryexpr pe', List.map weed_expr es, r)
		| Append (pos, i, e, r) -> Append (pos, i, weed_expr e, r)
		| Cast (pos, typ, e, r) -> Cast (pos, weed_tp typ, weed_expr e, r)

	and weed_operand (o : operand) : operand = match o with
		| Parens (pos, x, r) -> Parens (pos, weed_expr x, r)
		| Var (pos, x, r) -> Var (pos, x, r)
		| IntLit (pos, x, r) -> IntLit (pos, x, r)
		| FloatLit (pos, x, r) -> FloatLit (pos, x, r)
		| RuneLit (pos, x, r) -> RuneLit (pos, x, r)
		| StrLit (pos, x, r) -> StrLit (pos, x, r)

end

module ReturnWeeding = struct

	let tp_of_expr e = match e with
		| Unary (_, _, tor) -> Util.omap (fun x -> x) Void !tor
	  	| Binary (_, _, _, _, tor) -> Util.omap (fun x -> x) Void !tor

	let rec weed (t : prog) : unit =
		let Prog (pos, pkg, tlist) = t in
		        List.iter weed_topleveldecl tlist

	and weed_topleveldecl (t : topleveldecl) : unit = match t with
	| FuncDecl (pos, id, vspec, typ, ss) ->
		let rtype = Util.omap (fun x -> x) Void typ in
		(* Check if this is valid, void function must not have a return statement *)
		let ret_list = List.map (fun s -> weed_stmt rtype s) ss in
		if List.mem true ret_list then ()
		else if rtype = Void then ()    	 
		else raise (Check.TypeError (id ^ " contains a branch missing a return statement"))			
	| Stmt (pos, s) -> ()

	and weed_stmt  (t : tp) (s : stmt) : bool = match s with
		| Decl (pos, ds) -> false
		| Simple (pos, ss) -> false
		| Return (pos, e) ->			
			let (e', t') =			
		  	begin
		  		match e with
		  		| None -> (None, Void)
		  		| Some e'' -> (Some e'', tp_of_expr e'') 
		  	end
		in
		if t = Void 
		then 				
			begin
				match e' with
				| None -> true
				| Some _ -> raise (WeedError "Return wtih value in function with no return type")
			end				
		else
			if (Check.compare_tps t t') 
			then true		
			else raise (WeedError "Type mismatch in return statement.")
		| Break pos -> false
		| Continue pos -> false
		| Block (pos, sl) -> List.mem true (List.map (weed_stmt t) sl)
		| If (pos, is) -> weed_ifstmt t is
		| Switch (pos, ss) -> weed_switchstmt t ss
		| For (pos, fs) -> weed_forstmt t fs
		| Print (pos, ps) -> false

	and weed_ifstmt (t :tp) (is : ifstmt) : bool = match is with
	  | IfOnly (pos, ic, sl) -> List.iter (fun x -> ignore (weed_stmt t x)) sl; false (* Trivially false? *)
	  | IfElse (pos, ic, sl1, sl2) -> (List.mem true (List.map (weed_stmt t) sl1)) && (List.mem true (List.map (weed_stmt t) sl2))
	  | IfElseIf (pos, ic, sl, is') -> List.mem true (List.map (weed_stmt t) sl) && (weed_ifstmt t is')

	  (* If there is no default, trivially false *)
	and weed_switchstmt (t :tp) (ss : switchstmt) : bool =
		let SwitchStmt (_, _, eccl) = ss in
		if (List.mem 1 
			(List.map 
				(fun x -> let ExprCaseClause (_, esc,_) = x in 
					begin 
						match esc with 
						| Case _ -> 0 
						| Default _ -> 1 
					end
				) 
			eccl)
		   ) then
	 	  List.fold_left (fun a b -> a && b) true (List.map (weed_exprcaseclause t) eccl)
	 	else false

	 and weed_exprcaseclause (t :tp) (ecc : exprcaseclause) : bool =
	   let ExprCaseClause (_, _, sl) = ecc in
	        List.mem true (List.map (weed_stmt t) sl)

	and weed_forstmt (t :tp) (fs : forstmt) : bool = match fs with
		| InfLoop (pos, sl) ->
			List.mem true (List.map (weed_stmt t) sl)
		| WhileLoop (pos, e, sl) ->
		   List.mem true (List.map (weed_stmt t) sl)
		| ForLoop (pos, s1, e, s2, sl) ->
		   List.mem true (List.map (weed_stmt t) sl)

end
