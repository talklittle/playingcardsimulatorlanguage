open Ast

let string_of_op op = match op with
    Add     -> "+" 
  | Sub     -> "-" 
  | Mult    -> "*" 
  | Div     -> "/"
  | Equal   -> "==" 
  | Neq     -> "!="
  | Less    -> "<" 
  | Leq     -> "<=" 
  | Greater -> ">" 
  | Geq     -> ">="
  | And     -> "&&"
  | Or      -> "||"

let string_of_bool bool = match bool with
    True  -> "true"
  | False -> "false"

let string_of_t t = match t with
    Unknown    -> ""
  | Int        -> "int "
  | StringType -> "string "
  | Bool       -> "bool "
  | CardEntity -> "$"
  | Card       -> "Card "

let string_of_scope scope = match scope with
    None    -> ""
  | Global  -> "#"
  | Local   -> ""
  | Some(s) -> "$" ^ s ^ "."
			   
let string_of_var v = match v with
  Var(id, scope, t) -> string_of_t t ^ string_of_scope scope ^ id

let rec string_of_expr = function
    Null -> "null"
  | IntLiteral(l) -> string_of_int l
  | StringLiteral(l) -> l
  | BoolLiteral(b) -> string_of_bool b
  | Id(v) -> string_of_var v
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(e1, e2) -> string_of_expr e1 ^ " <- " ^ string_of_expr e2
  | Transfer(e1, e2) -> string_of_expr e1 ^ " -> " ^ string_of_expr e2
  | Call(f, el) ->
      string_of_var f ^ "(" ^ 
      String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Break -> "break;\n"
  | Print(expr) -> string_of_expr expr ^ " >>\n"
  | Read(expr) -> string_of_expr expr ^ " <<\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ") {\n" ^
      String.concat "" (List.map string_of_stmt s1) ^ "} else {\n" ^ 
      String.concat "" (List.map string_of_stmt s2) ^ "}\n"
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") {\n" ^ 
      String.concat "" (List.map string_of_stmt s) ^ "}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") {\n" ^ 
      String.concat "" (List.map string_of_stmt s) ^ "}\n"
  | Nostmt -> ""

let string_of_strdecl id = id ^ ";\n"
				  
let string_of_vdecl v = 
  string_of_var v ^ ";\n"

let string_of_fdecl fdecl =
  string_of_var fdecl.fname ^ "(" ^ 
  String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_sdecl_1 sname strings =
  sname ^ "\n{\n" ^
  String.concat "" (List.map string_of_strdecl strings) ^
  "}\n"

let string_of_sdecl_2 sname vars =
  sname ^ "\n{\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^
  "}\n"

let string_of_sdecl_3 sname vars body =
  sname ^ "\n{\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^
  String.concat "" (List.map string_of_stmt body) ^
  "}\n"

let string_of_program (spec, funcs) =
  string_of_sdecl_1 "Include" spec.incl.includes ^ "\n" ^
  string_of_sdecl_1 "CardEntities" spec.cent.entities ^ "\n" ^
  string_of_sdecl_2 "Globals" spec.glob.globals ^ "\n" ^
  string_of_sdecl_3 "Start" spec.strt.slocals spec.strt.sbody ^ "\n" ^
  string_of_sdecl_3 "PlayOrder" spec.play.plocals spec.play.pbody ^ "\n" ^
  string_of_sdecl_3 "WinningCondition" spec.wcon.wlocals spec.wcon.wbody 
  ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
