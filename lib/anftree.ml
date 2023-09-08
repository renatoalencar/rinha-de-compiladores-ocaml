(* open Parsetree

type anftree =
  | A_Int of Int64.t
  | A_Str of string
  | A_Call of anftree * anftree list
  | A_Binary of binary_op * anftree * anftree
  | A_Function of int list * anftree
  | A_Let of int * anftree * anftree
  | A_If of anftree * anftree * anftree
  | A_Print of anftree
  | A_First of anftree
  | A_Second of anftree
  | A_Bool of anftree
  | A_Tuple of anftree * anftree
  | A_Var of int

open Typedtree

let rec of_typed_expr expr =
  match expr with
  | T_Int (value, _) -> A_Int value
  | T_Str (value, _) -> A_Str value
  | T_Call { callee; arguments; _ } ->
    assert false
  | T_Binary { lhs; op; rhs; _ } ->
    let lhs' = of_typed_expr lhs in
    let rhs' = of_typed_expr rhs in
    A_Binary (op, lhs', rhs') *)