open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

exception ReturnException of int * int NameMap.t

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
    (* IntLiteral, StringLiteral, BoolLiteral, CardLiteral *)
    | Literal(i) -> i, env
    | Noexpr -> 1, env (* must be non-zero for the loop predicate *)

    | Variable(var) ->
        (* XXX make sure this is correct. should locals and globals be separated?
               Do globals supersede locals for our language?? *)
        
        let locals, globals = env in
        if NameMap.mem var locals then
          (NameMap.find var locals), env
        else if NameMap.mem var globals then
          (NameMap.find var globals), env
        else raise (Failure ("undeclared identifier " ^ var))

    | Binop(e1, op, e2) ->
        let v1, env = eval env e1 in
        let v2, env = eval env e2 in
        let boolean i = if i then 1 else 0 in
        (match op with
          Add -> v1 + v2
        | Sub -> v1 - v2
        | Mult -> v1 * v2
        | Div -> v1 / v2
        | Equal -> boolean (v1 = v2)
        | Neq -> boolean (v1 != v2)
        | Less -> boolean (v1 < v2)
        | Leq -> boolean (v1 <= v2)
        | Greater -> boolean (v1 > v2)
        | Geq -> boolean (v1 >= v2)
        | And -> boolean (v1 && v2)
        | Or -> boolean (v1 || v2)
        | Concat -> v1 ^ v2  (* XXX this is String concat, right? *)
        ), env
    | Assign(var, e) ->
        (* FIXME our program separates locals and globals, right? *)
        let v, (locals, globals) = eval env e in
        if NameMap.mem var locals then
          v, (NameMap.add var v locals , globals)
        else if NameMap.mem var globals then
          v, (locals, NameMap.var v globals)
        else raise (Failure ("undeclared identifier " ^ var))
    | Call("print", [e]) ->
        let v, env = eval env e in
        print_endline (string_of_int v);
        0, env
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
        let (locals, globals) = env in
        try
          let globals = call fdecl actuals globals
          in  0, (locals, globals)
        with ReturnException(v, globals) -> v, (locals, globals)
  in
  (* end of eval section *)

  (* Execute a statement and return an updated environment *)
  (* TODO add the rest of our statements *)
  let rec exec env = function
      Block(stmts) -> List.fold_left exec env stmts
    | Expr(e) -> let _, env = eval env e in env
    | If(e, s1, s2) ->
        let v, env = eval env e in
        exec env (if v != 0 then s1 else s2)
    | While (e, s) ->
        let rec loop env =
          let v, env = eval env e in
          if v != 0 then loop (exec env s) else env
        in loop env
    | For(e1, e2, e3, s) ->
        let _, env = eval env e1 in
        let rec loop env =
          let v, env = eval env e2 in
          if v!= 0 then
            let _, env = eval (exec env s) e3 in
            loop env
          else
            env
        in loop env
    | Return(e) ->
        let v, (locals, globals) = eval env e in
        raise (ReturnException(v, globals))
  in
  (* end of statement execution *)

  (* call: enter the function: bind actual values to formal args *)
  (* XXX make sure globals are bound correctly when entering a function. probably this section *)
  let locals =
    try List.fold_left2
      (fun locals formal actual -> NameMap.add formal actual locals)
      NameMap.empty fdecl.formals actuals
    with Invalid_argument(_) ->
      raise (Failure ("wrong number of arguments to " ^ fdecl.fname))
  in
  let locals = List.fold_left   (* Set local variables to 0 *)
      (fun locals local -> NameMap.add local 0 locals)
      locals fdecl.locals
  in   (* Execute each statement; return updated global symbol table *)
  snd (List.fold_left exec (locals, globals) fdecl.body)

(* run: set global variables to 0; find and run "start" *)
(* TODO instead of setting global vars to 0, read them from the globals block *)
in let globals = List.fold_left
    (fun globals vdecl -> NameMap.add vdecl 0 globals)
    NameMap.empty vars
in try
  call (NameMap.find "start" func_decls) [] globals
with Not_found ->
  raise (Failure ("did not find the start() function"))

