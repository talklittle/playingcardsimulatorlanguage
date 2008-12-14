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

    | Variable(var) ->
        (match var with
          VarExp(id, scope) ->
            (* TODO *)
        | GetIndex(vexpr, idx) ->
            (* TODO *)

        let locals, globals = env in
        if NameMap.mem var locals then
          (NameMap.find var locals), env
        else if NameMap.mem var globals then
          (NameMap.find var globals), env
        else raise (Failure ("undeclared identifier " ^ var))

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
        IntLiteral(1), env (* TODO *)

    | Assign(var, e) ->
        (* TODO *)
        (*
        let v, (locals, globals) = eval env e in
        if NameMap.mem var locals then
          v, (NameMap.add var v locals, globals)
        else if NameMap.mem var globals then
          v, (locals, NameMap.var v globals)
        else raise (Failure ("undeclared identifier " ^ var))
        *)

    | Transfer(var, e) ->
        IntLiteral(1), env (* TODO *)

    (* FIXME can't assume everything is int
    | Call("print", [e]) ->
        let v, env = eval env e in
        print_endline (string_of_int v);
        0, env
    *)

    (* FIXME locals, globals, entities *)
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


  1
in
1
