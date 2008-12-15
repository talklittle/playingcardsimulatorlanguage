open Ast

module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

exception ReturnException of int * int NameMap.t

(* seed random number generator with current time *)
let _ = Random.init (truncate (Unix.time ()))

(* Main entry point: run a program *)

let run (vars, funcs) =
  (* Put function declarations in a symbol table *)
  let func_decls = List.fold_left
      (fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)
      NameMap.empty funcs
  in

  (* Invoke a function and return an updated global symbol table *)
  let rec call fdecl actuals globals =

  (* Evaluate an expression and return (value, updated environment) *)
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
                if NameMap.mem id locals then
                  (NameMap.find id locals), env
                else raise (Failure ("undeclared local variable " ^ id))
            | Global ->
                if NameMap.mem id globals then
                  (NameMap.find id globals), env
                else raise (Failure ("undeclared global variable " ^ id))
            (* XXX are CardEntities retrieved this way? What expression do they evaluate to? *)
            (*
            | Entity ->
                if NameMap.mem id entities then
                  (NameMap.find id entities), env
                else raise (Failure ("undeclared CardEntity variable " ^ id))
            )
            *)
        | GetIndex(id, scope, index) ->
            let evalidx, env = eval env idx in
            (match scope, evalidx with
              Local, IntLiteral(i) ->
                if NameMap.mem id locals then
                  (match NameMap.find id locals with
                    ListLiteral(ls), Int -> IntLiteral(List.nth ls i)
                  | ListLiteral(ls), StringType -> StringLiteral(List.nth ls i)
                  | ListLiteral(ls), Bool -> BoolLiteral(List.nth ls i)
                  | ListLiteral(ls), Card -> CardLiteral(List.nth ls i)
                  (* | ListLiteral(ls), CardEntity -> XXX what is CardEntity expression?? *)
                  | ListLiteral(ls), List(t) -> ListLiteral(List.nth ls i)
                  | _, _ -> raise (Failure ("trying to dereference a non-list"))
                  ), env
                else raise (Failure ("undeclared local variable " ^ id))
            | Global, IntLiteral(i) ->
                if NameMap.mem id globals then
                  (match NameMap.find id globals with
                    ListLiteral(ls), Int -> IntLiteral(List.nth ls i)
                  | ListLiteral(ls), StringType -> StringLiteral(List.nth ls i)
                  | ListLiteral(ls), Bool -> BoolLiteral(List.nth ls i)
                  | ListLiteral(ls), Card -> CardLiteral(List.nth ls i)
                  (* | ListLiteral(ls), CardEntity -> XXX what is CardEntity expression?? *)
                  | ListLiteral(ls), List(t) -> ListLiteral(List.nth ls i)
                  | _, _ -> raise (Failure ("trying to dereference a non-list"))
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
                  let _, t = NameMap.find id locals in
                  v, (NameMap.add id (v, t) locals, globals, entities)
                else raise (Failure ("undeclared local variable " ^ id))
            | Global ->
                if NameMap.mem id globals then
                  let _, t = NameMap.find id globals in
                  v, (locals, NameMap.add id (v, t) globals, entities)
                else raise (Failure ("undeclared global variable " ^ id))
            (* XXX are entities assigned this way too? *)
            (*
            | Entity ->
                if NameMap.mem id entities then
                  let _, t = NameMap.find id locals in
                  v, (locals, globals, NameMap.add id v entities)
                else raise (Failure ("undeclared CardEntity " ^ id))
            )
            *)
        (* TODO still to fix after we changed GetIndex in AST *)
        | GetIndex(vexp, idx) ->
            let evalvexp, env = eval env (Variable(vexp)) in
            let evalidx, env = eval env idx in
            (match evalvexp, evalidx with
              (* The key is the evaluated Variable expression *)
              Variable(VarExp(id, Local)), IntLiteral(i) ->
                let key = (Variable(GetIndex(VarExp(id, Local), evalidx))) in
                if NameMap.mem key locals then
                  (NameMap.find key locals), env
                else raise (Failure ("local list dereference of " ^ id^"["^string_of_int i^"] without initializing that index"))
            | Variable(VarExp(id, Global)), IntLiteral(i) ->
                let key = (Variable(GetIndex(VarExp(id, Global), evalidx))) in
                if NameMap.mem key globals then
                  (NameMap.find key globals), env
                else raise (Failure ("global list dereference of " ^ id^"["^string_of_int i^"] without initializing that index"))
            | _, _ ->
                raise (Failure ("invalid list dereference, probably using non-integer index"))
            ))

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