
open Typedtree

let remaining_functions = Queue.create ()
let remaining_strings = Queue.create ()
let pushed_strings = Hashtbl.create 64

open Llvm

let type_to_llvm typ =
  match typ with
  | Int -> I32
  | Bool -> I1
  | Str -> Ptr
  | Tuple _ -> Array (2, Ptr)
  | _ -> assert false

let compile_binary func op typ lhs lhs_type rhs _rhs_type =
  let open Parsetree in
  let reg = create_register func "v" in
  let result_type = type_to_llvm typ in
  let instr =
    match op with
    | Lt -> I_icmp (Sle, type_to_llvm lhs_type, lhs, rhs)
    | Lte -> I_icmp (Slt, type_to_llvm lhs_type, lhs, rhs)
    | Eq -> I_icmp (Eq, type_to_llvm lhs_type, lhs, rhs)
    | Neq -> I_icmp (Ne, type_to_llvm lhs_type, lhs, rhs)
    | Gt -> I_icmp (Sgt, type_to_llvm lhs_type, lhs, rhs)
    | Gte -> I_icmp (Sge, type_to_llvm lhs_type, lhs, rhs)
    | Add -> I_add (result_type, lhs, rhs)
    | Sub -> I_sub (result_type, lhs, rhs)
    | Mul -> I_mul (result_type, lhs, rhs)
    | Div -> I_sdiv (result_type, lhs, rhs)
    | Rem -> I_srem (result_type, lhs, rhs)
    | Or -> I_or (result_type, lhs, rhs)
    | And -> I_and (result_type, lhs, rhs)

  in
  push_instruction func (Assign (reg, instr));
  Reg (result_type, reg)

let compile_string func value =
  match Hashtbl.find_opt pushed_strings value with
  | Some value -> value
  | None ->
    let label = create_label func ".str" in
    Queue.push (label, value) remaining_strings;
    let llvm_value: Llvm.value = Label (Array (String.length value - 1, I8) , label) in
    Hashtbl.add pushed_strings value llvm_value;
    llvm_value

let compile_string_convertion func typ compiled' =
  match typ with
  | Str -> compiled'
  | Int ->
    let str' = create_register func "v" in
    push_instruction func
      (Assign (str', I_call (Ptr, "int_to_string", [ compiled' ])));
    Reg (Ptr, str')
  | Bool ->
    let true' = compile_string func "true" in
    let false' = compile_string func "false" in
    let str = create_register func "v" in
    push_instruction func
      (Assign (str, I_select (compiled', true', false')));
    Reg (Ptr, str)
  | _ -> assert false

let compile_string_concatenation func lhs lhs_type rhs rhs_type =
  let reg = create_register func "v" in
  let instr =
  match lhs_type, rhs_type with
  | Str, Str ->
    [ Assign (reg, I_call (Ptr, "rinha_strcat", [ lhs; rhs  ])) ]
  | typ, Str ->
    let term' = compile_string_convertion func typ lhs in
    [ Assign (reg, I_call (Ptr, "rinha_strcat", [ term'; force_ptr rhs  ])) ]
  | Str, typ ->
    let term' = compile_string_convertion func typ rhs in
    [ Assign (reg, I_call (Ptr, "rinha_strcat", [ force_ptr lhs; term'  ])) ]
  | _ -> assert false
  in
  push_instructions func instr;
  Reg (Ptr, reg)

let rec compile func env tree =
  match tree with
  | T_Let (name, T_Function fn, next, _, _) ->
    Queue.push
      (name, fn)
      remaining_functions;
    compile func env next

  | T_Print (expr, _) ->
    let expr' = compile func env expr in
    let str' = compile_string_convertion func (find_type expr) expr' in
    push_instruction func
      (Call (Void, "print", [ str' ]));
    Int1 0

  | T_Call { callee = T_Var (name, _, _); arguments; typ; _ } ->
    let args =
      arguments
      |> List.map
        (fun expr ->
          compile func env expr)
    in
    let retval = create_register func "v" in
    let return_type = type_to_llvm typ in
    push_instruction func
      (Assign (retval, I_call (return_type, name, args)));
    Reg (return_type, retval)

  | T_Int (value, _) ->
    Int32 (Int64.to_int32 value)

  | T_Bool (value, _) -> if value then Int1 1 else Int1 0

  | T_Str (value, _) ->
    compile_string func value

  | T_Binary { lhs; op = Add; rhs; typ = Str; _ } ->
    let l' = compile func env lhs in
    let r' = compile func env rhs in
    compile_string_concatenation func
      l' (find_type lhs)
      r' (find_type rhs)

  | T_Binary { lhs; op; rhs; typ; _ } ->
    let l' = compile func env lhs in
    let r' = compile func env rhs in
    compile_binary func op typ
      l' (find_type lhs)
      r' (find_type rhs)

  | T_Var (name, _, _) ->
    Env.find name env

  | T_If { predicate; consequent; alternative; typ; _ } ->
    let pred' = compile func env predicate in
    let consequent_label = Llvm.create_label func "if_then" in
    let alternative_label = Llvm.create_label func "if_else" in
    let end_label = Llvm.create_label func "if_end" in

    push_instructions func
      [ Br (pred', consequent_label, alternative_label)
      ; Label alternative_label ];

    let alternative_value = compile func env alternative in
    push_instructions func
      [ Br_Label end_label
      ; Label consequent_label ];

    let consequent_value = compile func env consequent in
    push_instructions func
      [ Br_Label end_label
      ; Label end_label ];

    let if_value = create_register func "v" in
    push_instruction func
      (Assign (if_value
        , I_phi (type_to_llvm typ
          , (consequent_value, consequent_label)
          , (alternative_value, alternative_label))));
    Reg (type_to_llvm typ, if_value)

  | T_Let (name, value, next, _typ, _loc) ->
    let value' = compile func env value in
    compile func (Env.add name value' env) next

  | T_Tuple (t1, t2, typ, _) ->
    let t1 = compile func env t1 in
    let t2 = compile func env t2 in
    let v = create_register func "v" in
    let ptr = Reg (Ptr, v) in
    let el1 = create_register func "v" in 
    let el2 = create_register func "v" in 
    push_instructions func
      [ Assign (v, I_call (Ptr, "malloc", [ Int32 16l ]))
      ; Assign (el1, I_getelementptr (type_to_llvm typ, ptr, Int32 0l))
      ; Store (t1, Reg (Ptr, el1), Int32 8l)
      ; Assign (el2, I_getelementptr (type_to_llvm typ, ptr, Int32 1l))
      ; Store (t2, Reg (Ptr, el2), Int32 8l) ];
    Ptr (type_to_llvm typ, Reg (Ptr, v))

  | T_First (tuple, typ, _) ->
    let ptr = create_register func "v" in
    let value = create_register func "v" in
    push_instructions func
      [ Assign (ptr, I_getelementptr (type_to_llvm typ, compile func env tuple, Int32 0l))
      ; Assign (value, I_load (Ptr, Reg (Ptr, ptr), Int32 8l))];
    Reg (Ptr, value)

  | T_Second (tuple, _typ, _) ->
    let ptr = create_register func "v" in
    let value = create_register func "v" in
    (* TODO: Fix getelementptr type. *)
    push_instructions func
      [ Assign (ptr, I_getelementptr (Ptr, compile func env tuple, Int32 1l))
      ; Assign (value, I_load (Ptr, Reg (Ptr, ptr), Int32 8l))];
    Reg (Ptr, value)

  (* TODO:
    - GC
    - Anonymous functions and lambda lifting
    - Closures (variables in scope) *)

  | t ->
    Format.eprintf "ERROR: %a\n" pp_typed_tree t;
    exit 1

let compile_strings output =
  while not @@ Queue.is_empty remaining_strings do
    let label, str = Queue.pop remaining_strings in
    Llvm.write_string output label str
  done

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
          (Env.add arg (Reg (type_to_llvm typ, arg_name)) env)
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
      (Ret return_value);
    compile_strings output;
    Llvm.write_to_file output llvm_function
  done

(* External functions *)
let header: (string * typ * typ list) list =
  [ "print"        , Void, [ Ptr ]
  ; "rinha_strcat", Void, [ Ptr; Ptr ]
  ; "int_to_string", Ptr , [ Ptr; Ptr ]
  ; "malloc"       , Ptr , [ I32 ] ]

let compile_main output tree =
  List.iter
    (fun (name, return_type, arg_types) ->
      Llvm.write_declare output name return_type arg_types)
    header;

  let main = Llvm.create_function "main" I32 [] in
  let _ = compile main Env.empty tree in
  push_instruction main
    (Ret (Int32 0l));
  compile_strings output;
  Llvm.write_to_file output main;
  compile_remaining_functions output remaining_functions