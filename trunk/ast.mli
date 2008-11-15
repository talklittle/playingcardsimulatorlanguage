type bool = True | False

type op = 
  Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type scope =
    None
  | Global
  | Local
  | Some of string

type t =
    Unknown
  | Int
  | StringType
  | Bool
  | CardEntity
  | Card

type var = Var of string * scope * t

type expr =
    Null
  | IntLiteral of int
  | StringLiteral of string
  | BoolLiteral of bool
  | Id of var
  | Binop of expr * op * expr
  | Assign of expr * expr
  | Transfer of expr * expr
  | Call of var * expr list
  | Noexpr

type stmt =
    Break
  | Print of expr
  | Read of expr
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of expr * expr * expr * stmt list
  | While of expr * stmt list
  | Nostmt

type func_decl = {
    fname : var;
    formals : string list;
    locals : var list;
    body : stmt list;
  }

type incl_decl = {
    includes : string list;
  }

type cent_decl = {
    entities : string list;
  }

type glob_decl = {
    globals : var list;
  }

type strt_decl = {
    slocals : var list;
    sbody : stmt list;
  }

type play_decl = {
    plocals : var list;
    pbody : stmt list;
  }

type wcon_decl = {
    wlocals : var list;
    wbody : stmt list;
  }

type spec_decl = {
    incl : incl_decl;
    cent : cent_decl;
    glob : glob_decl;
    strt : strt_decl;
    play : play_decl;
    wcon : wcon_decl;
  }

type program = spec_decl * func_decl list
