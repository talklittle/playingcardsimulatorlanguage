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
                (* NameMap maps var names to (value, type) *)
                if NameMap.mem id locals then
                  fst (NameMap.find id locals), env
                else raise (Failure ("undeclared local variable " ^ id))
            | Global ->
                if NameMap.mem id globals then
                  fst (NameMap.find id globals), env
                else raise (Failure ("undeclared global variable " ^ id))
            (* XXX are CardEntities retrieved this way? What expression do they evaluate to? *)
            | Entity ->
                if NameMap.mem id entities then
                  fst (NameMap.find id entities), env
                else raise (Failure ("undeclared CardEntity " ^ id))
            )
        | GetIndex(id, scope, index) ->
            let evalidx, env = eval env index in
            (match scope, evalidx with
              Local, IntLiteral(i) ->
                if NameMap.mem id locals then
                  (match NameMap.find id locals with
                    ListLiteral(ls), _ -> List.nth ls i
                  | _, _ -> raise (Failure ("trying to dereference a non-list"))
                  ), env
                else raise (Failure ("undeclared local variable " ^ id))
            | Global, IntLiteral(i) ->
                if NameMap.mem id globals then
                  (match NameMap.find id globals with
                    ListLiteral(ls), _ -> List.nth ls i
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
            | Entity ->
                if NameMap.mem id entities then
                  let _, t = NameMap.find id locals in
                  v, (locals, globals, NameMap.add id (v, t) entities)
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
                  (match v, NameMap.find id locals with
                    IntLiteral(_), (ListLiteral(ls), Int) ->
                      v, (NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Int) locals, globals, entities)
                  | StringLiteral(_), (ListLiteral(ls), StringType) ->
                      v, (NameMap.add id ((ListLiteral(inserthelper ls i v 0)), StringType) locals, globals, entities)
                  | BoolLiteral(_), (ListLiteral(ls), Bool) ->
                      v, (NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Bool) locals, globals, entities)
                  | CardLiteral(_), (ListLiteral(ls), Card) ->
                      v, (NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Card) locals, globals, entities)
                    (* FIXME what expression do CardEntities have?
                  | CardEntityLiteral(_), (ListLiteral(ls), CardEntity) ->
                      v, (NameMap.add id ((ListLiteral(inserthelper ls i v 0)), CardEntity) locals, globals, entities)
                    *)
                  | ListLiteral(_), (ListLiteral(ls), ListType(lt)) ->
                      v, (NameMap.add id ((ListLiteral(inserthelper ls i v 0)), ListType(lt)) locals, globals, entities)
                  | _, _ -> raise (Failure ("trying to dereference a non-list")))
                else raise (Failure ("undeclared local variable " ^ id))
            | Global, IntLiteral(i) ->
                if NameMap.mem id globals then
                  let rec inserthelper ls targetindex value curr =
                    (match ls, curr with
                      _ :: tl, targetindex  -> value :: tl
                    | [], _                 -> raise (Failure ("index out of bounds"))
                    | hd :: tl, _           -> hd :: (inserthelper tl targetindex value (curr+1)))
                  in
                  (match v, NameMap.find id globals with
                    IntLiteral(_), (ListLiteral(ls), Int) ->
                      v, (locals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Int) globals, entities)
                  | StringLiteral(_), (ListLiteral(ls), StringType) ->
                      v, (locals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), StringType) globals, entities)
                  | BoolLiteral(_), (ListLiteral(ls), Bool) ->
                      v, (locals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Bool) globals, entities)
                  | CardLiteral(_), (ListLiteral(ls), Card) ->
                      v, (locals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), Card) globals, entities)
                    (* FIXME what expression do CardEntities have?
                  | CardEntityLiteral(_), (ListLiteral(ls), CardEntity) ->
                      v, (locals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), CardEntity) globals, entities)
                    *)
                  | ListLiteral(_), (ListLiteral(ls), ListType(lt)) ->
                      v, (locals, NameMap.add id ((ListLiteral(inserthelper ls i v 0)), ListType(lt)) globals, entities)
                  | _, _ -> raise (Failure ("trying to dereference a non-list")))
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
        IntLiteral(1), env (* TODO *)

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
