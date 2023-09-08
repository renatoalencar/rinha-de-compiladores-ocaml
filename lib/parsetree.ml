
type loc =
  { start : int
  ; _end : int
  ; filename : string }

let pp_loc fmt { start; _end; _ } =
  Format.fprintf fmt "<loc %d:%d>" start _end

type error =
  { message : string
  ; full_text :  string
  ; loc : loc }

type var = string * loc

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
[@@deriving show]

type parsed_tree =
  | E_Error of error
  | E_Int of Int64.t * loc
  | E_Str of string * loc
  | E_Call of call
  | E_Binary of binary
  | E_Function of func
  | E_Let of string * parsed_tree * parsed_tree * loc
  | E_If of _if
  | E_Print of parsed_tree * loc
  | E_First of parsed_tree * loc
  | E_Second of parsed_tree * loc
  | E_Bool of bool * loc
  | E_Tuple of parsed_tree * parsed_tree * loc
  | E_Var of var

and binary =
  { lhs : parsed_tree
  ; op : binary_op
  ; rhs : parsed_tree
  ; loc : loc }

and _if =
  { predicate : parsed_tree
  ; consequent : parsed_tree
  ; alternative : parsed_tree
  ; loc : loc }

and call =
  { callee : parsed_tree
  ; arguments: parsed_tree list
  ; loc : loc }

and func =
  { parameters : var list
  ; body : parsed_tree
  ; loc : loc }

