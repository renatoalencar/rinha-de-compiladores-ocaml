
open Typedtree

let remaining_functions = Queue.create ()

open Llvm

let type_to_llvm typ =
  match typ with
  | Int -> I64
  | Bool -> I1
  | _ -> assert false

let compile_binary func op typ l' r' =
  let open Parsetree in
  match op with
  | Lt ->
    let reg = create_register func "v" in
    push_instruction func
      (Assign (reg, I_icmp (Slt, type_to_llvm typ, l', r')));
    reg

  | Add ->
    let reg = create_register func "v"in
    push_instruction func
      (Assign (reg, I_add (type_to_llvm typ, l', r')));
    reg

  | Sub ->
    let reg = create_register func "v" in
    push_instruction func
      (Assign (reg, I_sub (type_to_llvm typ, l', r')));
    reg

  | _ -> assert false

let rec compile func env tree =
  match tree with
  | T_Let (name, T_Function fn, next, _, _) ->
    Queue.push
      (name, fn)
      remaining_functions;
    compile func env next

  | T_Print (expr, _) ->
    let expr' = compile func env expr in
    push_instruction func
      (Call (Void, "print", [ (I64, expr') ]));
    "0"

  | T_Call { callee = T_Var (name, _, _); arguments; typ; _ } ->
    let args =
      arguments
      |> List.map (compile func env)
      |> List.map (fun arg -> (I64, arg))
    in
    let retval = create_register func "v" in
    push_instruction func
      (Assign (retval, I_call (type_to_llvm typ, name, args)));
    retval

  | T_Int (value, _) ->
    Int64.to_string value

  | T_Binary { lhs; op; rhs; _ } ->
    let l' = compile func env lhs in
    let r' = compile func env rhs in
    compile_binary func op Int l' r'

  | T_Var (name, _, _) ->
    Env.find name env

  | T_If { predicate; consequent; alternative; typ; _ } ->
    let pred' = compile func env predicate in
    let consequent_label = Llvm.create_label func "if_then" in
    let alternative_label = Llvm.create_label func "if_else" in
    let end_label = Llvm.create_label func "if_end" in
    push_instruction func
      (Br (pred', consequent_label, alternative_label));
    push_instruction func
      (Label alternative_label);
    let a' = compile func env alternative in
    push_instruction func
      (Br_Label end_label);
    push_instruction func
      (Label consequent_label);
    let b' = compile func env consequent in
    push_instruction func
      (Br_Label end_label);
    push_instruction func
      (Label end_label);
    let r' = create_register func "v" in
    push_instruction func
      (Assign (r', I_phi (type_to_llvm typ, (b', consequent_label), (a', alternative_label))));
    r'

  | t ->
    Format.eprintf "%a\n" pp_typed_tree t;
    exit 1

let compile_remaining_functions output functions =
  while not @@ Queue.is_empty functions do
    let (name, func) = Queue.pop functions in
    let rec aux i llvm_args env args =
      match args with
      | (arg, typ) :: args ->
        let arg_name = Printf.sprintf "%%%d" i in
        aux
          (i + 1)
          (type_to_llvm typ :: llvm_args)
          (Env.add arg arg_name env)
          args
      | [] -> List.rev llvm_args, env
    in
    let args, env = aux 0 [] Env.empty func.parameters in
    let return_type =
      match func.typ with
      | Arrow (_, return_type) -> return_type
      | _ -> assert false
    in
    let llvm_function = Llvm.create_function name (type_to_llvm return_type) args in
    let return_value = compile llvm_function env func.body in
    push_instruction llvm_function
      (Ret (type_to_llvm return_type, return_value));
    Llvm.write_to_file output llvm_function
  done

let compile_main output tree =
  Llvm.write_declare output "print" Void [ I64 ];
  let main = Llvm.create_function "main" I32 [] in
  let _ = compile main Env.empty tree in
  push_instruction main
    (Ret (I32, "0"));
  Llvm.write_to_file output main;
  compile_remaining_functions output remaining_functions
