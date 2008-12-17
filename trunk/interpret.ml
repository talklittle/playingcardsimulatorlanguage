open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

exception ReturnException of Ast.expr * Ast.expr NameMap.t

(* Main entry point: run a program *)

let run (vars, funcs) =
  let func_decls = List.fold_left
	  (fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
	  NameMap.empty funcs
  in

  (* Invoke a function and return an updated global symbol table *)
  let rec call fdecl actuals globals =

  (* Evaluate an expression and return (value, updated environment) *)
  (* TODO put the rest of our expressions stuff here *)
  (* FIXME the types are really screwed up *)
	  let rec eval env = function
		  Null -> Null, env  (* XXX ok to return "null" here? *)
		| Noexpr -> Noexpr, env (* must be non-zero for the loop predicate *)

		| IntLiteral(i) -> IntLiteral(i), env
		| StringLiteral(i) -> StringLiteral(i), env
		| BoolLiteral(i) -> BoolLiteral(i), env
		| CardLiteral(i) -> CardLiteral(i), env

		(* Return (list of evaluated expressions), env *)
		(* Applicative order: evaluate each argument, updating env each time *)
		| ListLiteral(ls) ->
			(match ls with
			  []       -> ListLiteral([]), env
			| hd :: tl ->
			  let evalhd, env = eval env hd in
			  let evaltl, env = eval env (ListLiteral(tl)) in
			  (match evaltl with
				ListLiteral(lstl) -> ListLiteral(evalhd :: lstl), env
			  | _ -> raise (Failure ("invalid ListLiteral construction"))))

		| Binop(e1, op, e2) ->
			let v1, env = eval env e1 in
			let v2, env = eval env e2 in
			let boolean i = if i then BoolLiteral(true) else BoolLiteral(false) in
			(match v1, op, v2 with
			  IntLiteral(i1), Add, IntLiteral(i2)  -> IntLiteral(i1 + i2)
			| IntLiteral(i1), Sub, IntLiteral(i2)  -> IntLiteral(i1 - i2)
			| IntLiteral(i1), Mult, IntLiteral(i2) -> IntLiteral(i1 * i2)
			| IntLiteral(i1), Div, IntLiteral(i2)  -> IntLiteral(i1 / i2)
			| IntLiteral(i1),    Equal, IntLiteral(i2)    -> boolean (i1 = i2)
			| StringLiteral(i1), Equal, StringLiteral(i2) -> boolean (i1 = i2)
			| CardLiteral(i1),   Equal, CardLiteral(i2)   -> boolean (i1 = i2)
			| BoolLiteral(i1),   Equal, BoolLiteral(i2)   -> boolean (string_of_bool i1 = string_of_bool i2)
			| IntLiteral(i1),    Neq, IntLiteral(i2)    -> boolean (i1 <> i2)
			| StringLiteral(i1), Neq, StringLiteral(i2) -> boolean (i1 <> i2)
			| CardLiteral(i1),   Neq, CardLiteral(i2)   -> boolean (i1 <> i2)
			| BoolLiteral(i1),   Neq, BoolLiteral(i2)   -> boolean (string_of_bool i1 <> string_of_bool i2)
			| IntLiteral(i1),    Less, IntLiteral(i2)    -> boolean (i1 < i2)
			| StringLiteral(i1), Less, StringLiteral(i2) -> boolean (i1 < i2)
			| CardLiteral(i1),   Less, CardLiteral(i2)   -> boolean (i1 < i2) (* XXX cmp cards as string? *)
			| IntLiteral(i1),    Leq, IntLiteral(i2)    -> boolean (i1 <= i2)
			| StringLiteral(i1), Leq, StringLiteral(i2) -> boolean (i1 <= i2)
			| CardLiteral(i1),   Leq, CardLiteral(i2)   -> boolean (i1 <= i2) (* XXX cmp cards as string? *)
			| IntLiteral(i1),    Greater, IntLiteral(i2)    -> boolean (i1 > i2)
			| StringLiteral(i1), Greater, StringLiteral(i2) -> boolean (i1 > i2)
			| CardLiteral(i1),   Greater, CardLiteral(i2)   -> boolean (i1 > i2) (* XXX cmp cards as string? *)
			| IntLiteral(i1),    Geq, IntLiteral(i2)    -> boolean (i1 >= i2)
			| StringLiteral(i1), Geq, StringLiteral(i2) -> boolean (i1 >= i2)
			| CardLiteral(i1),   Geq, CardLiteral(i2)   -> boolean (i1 >= i2) (* XXX cmp cards as string? *)
			| BoolLiteral(i1), And, BoolLiteral(i2) -> boolean (i1 && i2)
			| BoolLiteral(i1), Or, BoolLiteral(i2)  -> boolean (i1 || i2)

			| StringLiteral(i1), Concat, StringLiteral(i2) -> StringLiteral(i1 ^ i2)  (* XXX we want String concat, right? *)

			| _, _, _ ->
				raise (Failure ("invalid binary operation"))
			), env

		| Rand(e) ->
			(match e with
			  IntLiteral(i) -> IntLiteral(Random.int i), env
			| _ -> raise (Failure ("invalid argument for random operator ~. Must supply an int."))
			)

		| Variable(var) ->
			let locals, globals, entities = env in
			(match var with
			  VarExp(id, scope) ->
				(match scope with
				  Local ->
					(* NameMap maps var names to (literalvalue) *)
					if NameMap.mem id locals then
					  NameMap.find id locals, env
					else raise (Failure ("undeclared local variable " ^ id))
				| Global ->
					if NameMap.mem id globals then
					  NameMap.find id globals, env
					else raise (Failure ("undeclared global variable " ^ id))
				(* XXX are CardEntities retrieved this way? What expression do they evaluate to? *)
				| Entity ->
					if NameMap.mem id entities then
					  NameMap.find id entities, env
					else raise (Failure ("undeclared CardEntity " ^ id))
				)
			| GetIndex(id, scope, index) ->
				let evalidx, env = eval env index in
				(match scope, evalidx with
				  Local, IntLiteral(i) ->
					if NameMap.mem id locals then
					  (match NameMap.find id locals with
						ListLiteral(ls) -> List.nth ls i
					  | _ -> raise (Failure ("trying to dereference a non-list"))
					  ), env
					else raise (Failure ("undeclared local variable " ^ id))
				| Global, IntLiteral(i) ->
					if NameMap.mem id globals then
					  (match NameMap.find id globals with
						ListLiteral(ls) -> List.nth ls i
					  | _ -> raise (Failure ("trying to dereference a non-list"))
					  ), env
					else raise (Failure ("undeclared global variable " ^ id))
				| _, _ ->
					raise (Failure ("invalid list dereference, probably using non-integer index"))
				))

		| Assign(var, e) ->
			let v, (locals, globals, entities) = eval env e in
			(match var with
			  VarExp(id, scope) ->
				(match scope with
				  Local ->
					if NameMap.mem id locals then
					  v, (NameMap.add id v locals, globals, entities)
					else raise (Failure ("undeclared local variable " ^ id))
				| Global ->
					if NameMap.mem id globals then
					  v, (locals, NameMap.add id v globals, entities)
					else raise (Failure ("undeclared global variable " ^ id))
				(* XXX are entities assigned this way too? *)
				| Entity ->
					if NameMap.mem id entities then
					  v, (locals, globals, NameMap.add id v entities)
					else raise (Failure ("undeclared CardEntity " ^ id))
				)
			| GetIndex(id, scope, index) ->
				let evalidx, env = eval env index in
				(match scope, evalidx with
				  Local, IntLiteral(i) ->
					if NameMap.mem id locals then
					  let rec inserthelper ls targetindex value curr =
						(match ls, curr with
						  _ :: tl, targetindex  -> value :: tl
						| [], _                 -> raise (Failure ("index out of bounds"))
						| hd :: tl, _           -> hd :: (inserthelper tl targetindex value (curr+1)))
					  in
					  (match NameMap.find id locals with
						ListLiteral(ls) ->
						  v, (NameMap.add id (ListLiteral(inserthelper ls i v 0)) locals, globals, entities)
					  | _ -> raise (Failure ("trying to dereference a non-list")))
					else raise (Failure ("undeclared local variable " ^ id))
				| Global, IntLiteral(i) ->
					if NameMap.mem id globals then
					  let rec inserthelper ls targetindex value curr =
						(match ls, curr with
						  _ :: tl, targetindex  -> value :: tl
						| [], _                 -> raise (Failure ("index out of bounds"))
						| hd :: tl, _           -> hd :: (inserthelper tl targetindex value (curr+1)))
					  in
					  (match NameMap.find id globals with
						ListLiteral(ls) ->
						  v, (locals, NameMap.add id (ListLiteral(inserthelper ls i v 0)) globals, entities)
					  | _ -> raise (Failure ("trying to dereference a non-list")))
					else raise (Failure ("undeclared global variable " ^ id))
				(* XXX Entities are treated the same way for assigning to a GetIndex?
				| Entity, IntLiteral(i) ->
					if NameMap.mem id entities then
					  let rec inserthelper ls targetindex value curr =
						(match ls, curr with
						  _ :: tl, targetindex  -> value :: tl
						| [], _                 -> raise (Failure ("index out of bounds"))
						| hd :: tl, _           -> hd :: (inserthelper tl targetindex value (curr+1))
					  in
					  (match v, NameMap.find id entities with
						IntLiteral(_), (ListLiteral(ls), Int) ->
						  v, (locals, globals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Int) entities)
					  | StringLiteral(_), (ListLiteral(ls), StringType) ->
						  v, (locals, globals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), StringType) entities)
					  | BoolLiteral(_), (ListLiteral(ls), Bool) ->
						  v, (locals, globals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Bool) entities)
					  | CardLiteral(_), (ListLiteral(ls), Card) ->
						  v, (locals, globals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Card) entities)
						(* FIXME what expression do CardEntities have?
					  | CardEntityLiteral(_), (ListLiteral(ls), CardEntity) ->
						  v, (locals, globals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), CardEntity) entities)
						*)
					  | ListLiteral(_), (ListLiteral(ls), Card) ->
						  v, (locals, globals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), List) entities)
					  | _, _ -> raise (Failure ("trying to dereference a non-list")))
					else raise (Failure ("undeclared CardEntity " ^ id))
				*)
				| _, _ ->
					raise (Failure ("invalid list dereference, probably using non-integer index"))
				))

		| ListLength(vlist) ->
			let evlist, env = eval env vlist in
			(match evlist with
			  ListLiteral(ls) -> IntLiteral(List.length ls), env
			| _ -> raise (Failure ("argument to list length operator must be a list")))

		| Append(vlist, e) ->
			let v, env = eval env e in
			let evlist, env = eval env vlist in
			(match evlist with
			  ListLiteral(ls) -> ListLiteral(ls @ [v]), env
			| _ -> raise (Failure ("trying to append an element to a non-list")))

		| Transfer(cevar, card) ->
			(* Make sure that cevar is a CardEntity, and e is Card *)
			let evalc, env = eval env card in
			let evalce, env = eval env cevar in
			(match evalce, evalc with
			  (*CardEntity(ce), Card(c) -> XXX CardEntity expression??? *)
			| _, _ -> raise (Failure ("Transfer: arguments must be cardentity <- card")))
		| Call(f, actuals) ->
			let fdecl =
			  try NameMap.find f func_decls
			  with Not_found -> raise (Failure ("undefined function " ^ f))
			in
			let actuals, env = List.fold_left
				(fun (actuals, values) actual ->
				  let v, env = eval env actual in
				  v :: actuals, values) ([], env) actuals
			in
			let (locals, globals, entities) = env in
			try
			  let globals = call fdecl actuals globals
			  in  BoolLiteral(false), (locals, globals, entities)
			with Not_found -> raise (Failure ("Unknown Fail 1 "))
		(* FIXME can't assume everything is int
		| Call("print", [e]) ->
			let v, env = eval env e in
			print_endline (string_of_int v);
			0, env
		*)

		(* FIXME locals, globals, entities *)
	  in
	  (* end of eval section *)

	  (* Execute a statement and return an updated environment *)
	  (* TODO add the rest of our statements *)
	  let rec exec env = function 
		 Expr(e) -> let _, env = eval env e in env
		| If(e, s1, s2) ->
			let v, env = eval env e in
			let b = (match v with
				BoolLiteral(b) -> b
				| _ -> raise (Failure ("Invalid conditional expression.")))
			in
			if b then begin
				ignore (List.iter (fun n -> ignore(exec env n)) s1);  env end
			else begin
				ignore (List.iter (fun n -> ignore(exec env n)) s2);  env end    
		| While (e, s) ->
			let rec loop env =
			  let v, env = eval env e in
			  let b = (match v with
				BoolLiteral(b) -> b
				| _ -> raise (Failure ("Invalid conditional expression.")))
			  in
			  if b then begin
				ignore (List.iter (fun n -> ignore(exec env n)) s); loop env end 
			  else env
			in loop env
		| Return(e) ->
			let v, (locals, globals, entities) = eval env e in
		   raise(Failure("Return Success"))
  
  (* end of statement execution *)

  (* call: enter the function: bind actual values to formal args *)
  (* XXX make sure globals are bound correctly when entering a function. probably this section *)
  
  (*let evaluatedActuals = List.map eval env actuals
  
  let locals =
	try List.fold_left2
	  (fun locals (formal,_) actual -> NameMap.add formal actual locals)
	  NameMap.empty fdecl.formals actuals
	with Invalid_argument(_) ->
	  raise (Failure ("wrong number of arguments to " ^ fdecl.fname))
  in
  let locals = List.fold_left   (* Set local variables to 0 *)
	  (fun locals local -> NameMap.add local 0 locals)
	  locals fdecl.locals
  in   (* Execute each statement; return updated global symbol table *)
  snd (List.fold_left exec (locals, globals) fdecl.body)
*)
(* run: set global variables to 0; find and run "start" *)
(* TODO instead of setting global vars to 0, read them from the globals block *)
in let globals = List.fold_left
	(fun globals vdecl -> NameMap.add vdecl 0 globals)
	NameMap.empty vars
in
try
  call (NameMap.find "start" func_decls) [] globals
with Not_found ->
  raise (Failure ("did not find the start() function"))