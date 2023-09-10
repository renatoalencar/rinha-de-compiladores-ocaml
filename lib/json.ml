open Parsetree

type loc = Parsetree.loc =
  { start : int
  ; _end : int [@key "end"]
  ; filename : string }
[@@deriving yojson]

type var = { text: string; location: loc }
[@@deriving of_yojson]

let operation_of_yojson = function
  | `String "Add" -> Add
  | `String "Sub" -> Sub
  | `String "Lt" -> Lt
  | _ -> assert false

let var_of_yojson yojson =
  Result.get_ok @@ var_of_yojson yojson
let loc_of_yojson yojson =
  Result.get_ok @@ loc_of_yojson yojson

let parse_list f = function
  | `List lst -> List.map f lst
  | _ -> assert false

let parse_string = function
  | `String s -> s
  | _ -> assert false

let parse_int = function
  | `Int s -> s
  | _ -> assert false


let rec parse_expression expr =
  let expr =
    match expr with
    | `Assoc expr -> expr
    | _ -> assert false
  in
  let kind = List.assoc "kind" expr in
  match kind with
  | `String "Let" ->
    let name = var_of_yojson @@ List.assoc "name" expr in
    let value = parse_expression @@ List.assoc "value" expr in
    let next = parse_expression @@ List.assoc "next" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Let
      (name.text, value, next, loc)
  | `String "Function" ->
    let parameters =
      parse_list var_of_yojson @@ List.assoc "parameters" expr
    in
    let value = parse_expression @@ List.assoc "value" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Function
      { parameters = List.map (fun p -> (p.text, p.location)) parameters
      ; body = value
      ; loc }
  | `String "If" ->
    let condition = parse_expression @@ List.assoc "condition" expr in
    let then_ = parse_expression @@ List.assoc "then" expr in
    let otherwise = parse_expression @@ List.assoc "otherwise" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_If
      { predicate = condition
      ; consequent = then_
      ; alternative = otherwise
      ; loc }
  | `String "Binary" ->
    let lhs = parse_expression @@ List.assoc "lhs" expr in
    let op = operation_of_yojson @@ List.assoc "op" expr in
    let rhs = parse_expression @@ List.assoc "rhs" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Binary
      { lhs; op; rhs; loc }
  | `String "Call" ->
    let callee = parse_expression @@ List.assoc "callee" expr in
    let arguments = parse_list parse_expression @@ List.assoc "arguments" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Call
      { callee; arguments; loc }
  | `String "Var" ->
    let name = parse_string @@ List.assoc "text" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Var (name, loc)
  | `String "Print" ->
    let value = parse_expression @@ List.assoc "value" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Print (value, loc)
  | `String "Int" ->
    let value = parse_int @@ List.assoc "value" expr in
    let loc = loc_of_yojson @@ List.assoc "location" expr in
    E_Int (Int64.of_int value, loc)
  | _ -> assert false

let of_yojson yojson =
  match yojson with
  | `Assoc assoc -> parse_expression @@ List.assoc "expression" assoc
  | _ -> failwith "Invalid input"

let of_channel channel =
  let yojson = Yojson.Safe.from_channel channel in
  of_yojson yojson
