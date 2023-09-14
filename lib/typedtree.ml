
open Parsetree

module Env = struct
  include Map.Make(String)
  let pp _ _ _ = ()
end


type typ =
  | Int
  | Str
  | Bool
  | Var of int ref
  | Tuple of typ * typ
  | Arrow of typ list * typ

let rec type_to_string t =
  match t with
  | Int -> "Int"
  | Str -> "Str"
  | Bool -> "Bool"
  | Var v -> "'v" ^ string_of_int !v
  | Tuple (fst, snd) -> "(" ^ type_to_string fst ^ ", " ^ type_to_string snd ^ ")"
  | Arrow (args, ret) ->
    "fn (" ^ String.concat ", " (List.map type_to_string args) ^ ") -> " ^ type_to_string ret

let pp_typ fmt t = Format.pp_print_string fmt (type_to_string t)

type typed_tree =
  | T_Int of Int64.t * loc
  | T_Str of string * loc
  | T_Call of call
  | T_Binary of binary
  | T_Function of func
  | T_Let of string * typed_tree * typed_tree * typ * loc
  | T_If of _if
  | T_Print of typed_tree * loc
  | T_First of typed_tree * typ * loc
  | T_Second of typed_tree * typ * loc
  | T_Bool of bool * loc
  | T_Tuple of typed_tree * typed_tree * typ * loc
  | T_Var of string * typ * loc

and binary =
  { lhs : typed_tree
  ; op : binary_op
  ; rhs : typed_tree
  ; typ : typ
  ; loc : loc }

and _if =
  { predicate : typed_tree
  ; consequent : typed_tree
  ; alternative : typed_tree
  ; typ : typ
  ; loc : loc }

and call =
  { callee : typed_tree
  ; arguments : typed_tree list
  ; typ : typ
  ; loc : loc }

and func =
  { parameters : (string * typ) list
  ; body : typed_tree
  ; env : typ Env.t
  ; typ : typ
  ; loc : loc }
[@@deriving show]

let type_env = Hashtbl.create 1024
let counter = ref 0
let create_var () =
  let c = !counter in
  incr counter;
  Var (ref c)

let report_error ~loc message =
  Printf.eprintf "%d:%d - %s"
    (loc.start)
    (loc._end)
    message;
  exit 1

let find_type tree =
  match tree with
  | T_Int _ -> Int
  | T_Bool _ -> Bool
  | T_Str _ -> Str
  | T_Call { typ; _ } -> typ
  | T_Binary { typ; _ } -> typ
  | T_Function { typ; _ } ->
    (match typ with
    | Arrow (_, ret) -> ret
    | _ -> assert false)
  | T_Let (_name, _value, _next, typ, _loc) -> typ
  | T_If { typ; _ } -> typ
  | T_Print _ -> Int
  | T_First (_tuple, typ, _loc) -> typ
  | T_Second (_tuple, typ, _loc) -> typ
  | T_Tuple (_fst, _snd, typ, _loc) -> typ
  | T_Var (_name, typ, _loc) -> typ

let rec build_parameters types env params =
  match params with
  | (name, _loc) :: params ->
    let typ = create_var () in
    build_parameters
      ((name, typ) :: types)
      (Env.add name typ env)
      params
  | [] -> List.rev types, env

let rec of_parsed_tree ~env tree =
  match tree with
  | E_Int (value, loc) -> T_Int (value, loc)
  | E_Bool (value, loc) -> T_Bool (value, loc)
  | E_Str (value, loc) -> T_Str (value, loc)

  | E_Call { callee; arguments; loc } ->
    let callee = of_parsed_tree ~env callee in
    let arguments = List.map (of_parsed_tree ~env) arguments in
    let return_type =
      match find_type callee with
      | Arrow (_, typ) as callee_type ->
        unify callee_type (Arrow (List.map find_type arguments, typ));
        typ
      | _ -> report_error ~loc "Callee is not a function"
    in
    T_Call { callee; arguments; loc; typ = return_type  }

  | E_Binary { lhs; op = Add; rhs; loc } ->
    let lhs = of_parsed_tree ~env lhs in
    let rhs = of_parsed_tree ~env rhs in
    let typ =
      (* TODO: Probably too naive? *)
      match find_type lhs, find_type rhs with
      | _, Str | Str, _ -> Str
      | _ -> Int
    in
    T_Binary { lhs; op = Add; rhs; loc; typ }

  | E_Binary { lhs; op; rhs; loc } ->
    (* TODO: Could do better for some cases *)
    let lhs = of_parsed_tree ~env lhs in
    let rhs = of_parsed_tree ~env rhs in
    let expected_type = type_binary op lhs rhs in
    T_Binary { lhs; op; rhs; loc; typ = expected_type }

  | E_Let (name, E_Function { parameters; body; loc = fn_loc }, next, loc) ->
    let types, body_env = build_parameters [] env parameters in
    let return_type = create_var () in
    let fn_type = Arrow (List.map (fun (_, typ) -> typ) types, return_type) in
    let body_env =
      Env.add
        name
        fn_type
        body_env
    in
    let env = Env.add name fn_type env in
    let next = of_parsed_tree ~env next in
    let body = of_parsed_tree ~env:body_env body in
    unify (find_type body) return_type;
    let value =
      T_Function
        { parameters = types
        ; body
        ; env
        ; loc = fn_loc
        ; typ = fn_type }
    in
    T_Let (name, value, next, find_type next, loc)

  | E_Function { parameters; body; loc } ->
    let types, body_env = build_parameters [] env parameters in
    let body = of_parsed_tree ~env:body_env body in
    T_Function
      { parameters = types
      ; body
      ; env
      ; loc
      ; typ = Arrow (List.map (fun (_, typ) -> typ) types, find_type body) }

  | E_Let (name, value, next, loc) ->
    let value = of_parsed_tree ~env value in
    let env = Env.add name (find_type value) env in
    let next = of_parsed_tree ~env next in
    T_Let (name, value, next, find_type next, loc)

  | E_If { predicate; consequent; alternative; loc } ->
    let predicate = of_parsed_tree ~env predicate in
    let consequent = of_parsed_tree ~env consequent in
    let alternative = of_parsed_tree ~env alternative in
    unify Bool (find_type predicate);
    let expected_type = find_type consequent in
    (* TODO: If there's a type mistach replace both arms with
       a dynamic dispatchable value. *)
    unify expected_type (find_type alternative);
    T_If { predicate
         ; consequent
         ; alternative
         ; typ = expected_type
         ; loc }

  | E_Print (arg, loc) ->
    T_Print (of_parsed_tree ~env arg, loc)

  | E_First (tuple, loc) ->
    let tuple = of_parsed_tree ~env tuple in
    let typ = match (find_type tuple) with
      | Tuple (fst, _) -> fst
      | t -> report_error ~loc ("Expected a tuple, found a " ^ type_to_string t)
    in
    T_First (tuple, typ, loc)

  | E_Second (tuple, loc) ->
    let tuple = of_parsed_tree ~env tuple in
    let typ = match find_type tuple with
      | Tuple (_, snd) -> snd
      | t -> report_error ~loc ("Expected a tuple, found a " ^ type_to_string t)
    in
    T_Second (tuple, typ, loc)

  | E_Tuple (fst, snd, loc) ->
    let fst= of_parsed_tree ~env fst in
    let snd = of_parsed_tree ~env snd in
    T_Tuple (fst, snd, Tuple (find_type fst, find_type snd), loc)

  | E_Var (name, loc) ->
    let typ =
      match Env.find_opt name env with
      | Some typ -> typ
      | None -> report_error ~loc ("Cant find name " ^ name)
    in
    T_Var (name, typ, loc)

  | E_Error { message; loc; _ } -> report_error ~loc message

and unify t1 t2 =
  match t1, t2 with
  | (Bool, Bool) | (Str, Str) | (Int, Int) -> ()
  | (Var v1, Var v2) when !v1 <> !v2 ->
    (match Hashtbl.find_opt type_env !v1, Hashtbl.find_opt type_env !v2 with
    | Some t1, Some t2 -> unify t1 t2
    | Some _, None -> v2 := !v1
    | None, None | None, Some _ -> v1 := !v2)
  | (Var _, Var _) -> ()
  | (Var v, t) | (t, Var v) ->
    (match Hashtbl.find_opt type_env !v with
    | Some t' -> unify t t'
    | None -> Hashtbl.add type_env !v t)
  | (Tuple (t1, t2), Tuple (t1', t2')) ->
    unify t1 t1';
    unify t2 t2'
  | (Arrow (a1, r1), Arrow (a2, r2)) ->
    (try
      List.iter2 unify a1 a2
    with Invalid_argument _ ->
      Printf.eprintf "%d arguments expected, %d found."
        (List.length a1)
        (List.length a2);
      exit 1);
    unify r1 r2

  | t1, t2 ->
    Printf.eprintf "Type %s cant match %s"
      (type_to_string t1)
      (type_to_string t2);
    exit 1

and type_binary op lhs rhs =
    unify (find_type lhs) (find_type rhs);
    match op with
    | Add | Sub | Mul | Div | Rem -> Int
    | Eq | Neq | Lt | Gt | Lte | Gte | And | Or -> Bool

let rec resolve_variable = function
  | Var v -> (match Hashtbl.find_opt type_env !v with Some t -> t | None -> Var v)
  | Tuple (fst, snd) -> Tuple (resolve_variable fst, resolve_variable snd)
  | Arrow (arguments, return_type) -> Arrow (List.map resolve_variable arguments, resolve_variable return_type)
  | t -> t

let resolve_type_variables typed_tree =
  let rec resolve typed_tree =
    match typed_tree with
    | T_Int _ | T_Str _ | T_Bool _ -> typed_tree
    | T_Call { callee; arguments; typ; loc } ->
      T_Call
        { callee = resolve callee
        ; arguments = List.map resolve arguments
        ; typ = resolve_variable typ
        ; loc }
    | T_Binary { lhs; op; rhs; typ; loc } ->
      T_Binary
        { lhs = resolve lhs
        ; op = op
        ; rhs = resolve rhs
        ; typ = resolve_variable typ
        ; loc }
    | T_Function { parameters; body; typ; loc; env } ->
      T_Function
        { parameters = List.map (fun (name, typ) -> name, resolve_variable typ) parameters
        ; body = resolve body
        ; env = env
        ; typ = resolve_variable typ
        ; loc }
    | T_Let (name, value, next, typ, loc) ->
      T_Let (name, resolve value, resolve next, resolve_variable typ, loc)
    | T_If { predicate; consequent; alternative; typ; loc } ->
      T_If
        { predicate = resolve predicate
        ; consequent = resolve consequent
        ; alternative = resolve alternative
        ; typ = resolve_variable typ
        ; loc = loc }
    | T_Print (value, loc) -> T_Print (resolve value, loc)
    | T_First (tuple, typ, loc) -> T_First (resolve tuple, resolve_variable typ, loc)
    | T_Second (tuple, typ, loc) -> T_Second (resolve tuple, resolve_variable typ, loc)
    | T_Tuple (fst, snd, typ, loc) ->
      T_Tuple (resolve fst, resolve snd, resolve_variable typ, loc)
    | T_Var (name, typ, loc) ->
      T_Var (name, resolve_variable typ, loc)
  in
  resolve typed_tree

let of_parsed_tree parsed_tree =
  let env = Env.empty in
  resolve_type_variables @@ of_parsed_tree ~env parsed_tree