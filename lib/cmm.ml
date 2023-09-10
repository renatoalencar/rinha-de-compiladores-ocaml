
type expr =
  | CInt64 of Int64.t
  | CInt32 of Int32.t
  | CString of string
  | CCall of string * expr list
  | CBinary of Parsetree.binary_op * expr * expr
  | CVar of int
  | CTuple of expr * expr
  | CCar of expr
  | CCdr of expr
[@@deriving show]

type statement =
  | CAssign of int * expr
  | CIf of expr * statement * statement
  | CExpr of expr
  | CReturn of expr
[@@deriving show]

type func =
  { name : string
  ; arguments: int list
  ; body : statement list }
[@@deriving show]

open Typedtree

let functions = ref []
let push_function fn = functions := fn :: !functions

let counter = ref 0
let create_var () =
  let id = !counter in
  incr counter;
  id

let rec compile_expr env tree =
  match tree with
  | T_Int (value, _) -> CInt64 value
  | T_Str (value, _) -> CString value
  | T_Bool (true, _) -> CInt32 0xffffffffl
  | T_Bool (false, _) -> CInt32 0x00000001l
  | T_Binary { lhs; op; rhs; _ } ->
    let lhs' = compile_expr env lhs in
    let bhs' = compile_expr env rhs in
    CBinary (op, lhs', bhs')
  | T_Var (name, _, _) ->
    CVar (Env.find name env)

  | T_Call { callee = T_Var (name, _, _); arguments; _ } ->
    CCall (name, List.map (compile_expr env) arguments)

  | expr -> Format.eprintf "Unsupported on statement %a\n" pp_typed_tree expr;
    exit 1

let rec compile env tree =
  match tree with
  | T_Call { callee = T_Var (name, _, _); arguments; _ } ->
    [ CExpr (CCall (name, List.map (compile_expr env) arguments)) ]

  | T_Let (name, T_Function { parameters; body; _ }, next, _, _) ->
    let rec build_args env compiled arguments =
      match arguments with
      | (name, _) :: args ->
        let id = create_var () in
        build_args
          (Env.add name id env)
          (id :: compiled)
          args
      | [] -> env, List.rev compiled
    in
    let body, args =
      let env, args = build_args env [] parameters in
      compile env body, args
    in
    push_function
      { name
      ; arguments = args
      ; body = body };
    compile env next

  | T_Let (name, expr, next, _, _) ->
    let expr' = compile_expr env expr in
    let var = create_var () in
    let env' = Env.add name var env in
    CAssign (var, expr') :: compile env' next

  | T_If { predicate; consequent; alternative; _ } ->
    let var = create_var () in
    [ CIf
        (compile_expr env predicate
        , CAssign (var, compile_expr env consequent)
        , CAssign (var, compile_expr env alternative)) ]

  | T_Print (expr, _) ->
    [ CExpr (CCall ("print", [ compile_expr env expr ])) ]

  | expr ->
    Format.eprintf "Unsupported on statement %a\n" pp_typed_tree expr;
    exit 1

let compile tree =
  let main =
    { name = "main"
    ; arguments = []
    ; body = compile Env.empty tree }
  in
  main :: !functions