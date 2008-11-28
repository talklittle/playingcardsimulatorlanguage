type bool = True | False

type op = 
  Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type scope =
    Global
  | Local
  | Entity

type t =
    Int
  | StringType
  | Bool
  | Card
  | CardEntity
  | List of t

type literal =
  | IntLiteral of int
  | StringLiteral of string
  | BoolLiteral of bool
  | CardLiteral of string

type vardec = VarDec of string * t

type varexp = 
    VarExp of string * scope
  | GetIndex of varexp * expr
and expr =
    Null
  | Variable of varexp
  | Literal of literal
  | ListLiteral of expr list
  | Binop of expr * op * expr
  | Rand of expr
  | Assign of varexp * expr
  | Transfer of varexp * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Break
  | Print of expr
  | Read of varexp
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of expr * expr * expr * stmt list
  | While of expr * stmt list
  | Nostmt

type func_decl = {
    fname : string;
    formals : vardec list;
    locals : vardec list;
    body : stmt list;
  }

type incl_decl = {
    includes : string list;
  }

type cent_decl = {
    entities : string list;
  }

type glob_decl = {
    globals : vardec list;
  }

type strt_decl = {
    slocals : vardec list;
    sbody : stmt list;
  }

type play_decl = {
    plocals : vardec list;
    pbody : stmt list;
  }

type wcon_decl = {
    wlocals : vardec list;
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
