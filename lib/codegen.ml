
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
    func.allocations <- Int32.add func.allocations 1l;
    push_instruction func
      (Assign (str', I_call (Ptr, "rinha_int_to_string", [ compiled' ])));
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
  func.allocations <- Int32.add func.allocations 1l;
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

let compile_alloc func words tag =
  let ptr = create_register func "v" in
  func.allocations <- Int32.add func.allocations 1l;
  push_instruction func
    ( Assign (ptr, I_call (Ptr, "rinha_alloc", [ Int32 words; Int32 tag ])) );
  Reg (Ptr, ptr)

let gc_add_global func value =
  push_instruction func
    (Call (Void, "rinha_gc_add_root", [ value ]))

let compile_array_load func typ arr pos =
  let ptr = create_register func "v" in
  let value = create_register func "v" in
  push_instructions func
    [ Assign (ptr, I_getelementptr (Ptr, arr, pos))
    ; Assign (value, I_load (type_to_llvm typ, Reg (Ptr, ptr), Int32 8l))];
  Reg (type_to_llvm typ, value)

let compile_array_store func typ arr pos value =
  let el_ptr = create_register func "v" in 
  push_instructions func
    [ Assign (el_ptr, I_getelementptr (type_to_llvm typ, arr, pos))
    ; Store (value, Reg (Ptr, el_ptr), Int32 8l) ]

module StringSet = Set.Make(String)

(* TODO: Still not working!
   It should rely on environment only for non global functions,
   we should differentiate for that. *)
let find_free_variables env parameters body =
  let rec aux vars defined expr =
  match expr with
  | T_Int _ | T_Str _ | T_Bool _ -> vars

  | T_Let (name, value, next, _, _) ->
    let vars = aux vars defined value in
    aux vars (StringSet.add name defined) next

  | T_Var (name, _, _) ->
    if StringSet.mem name defined || Env.mem name env then
      vars
    else
      (StringSet.add name vars)

  | T_Call { callee; arguments; _ } ->
    let vars = aux vars defined callee in
    List.fold_left
      (fun vars arg -> aux vars defined arg)
      vars
      arguments

  | T_Binary { lhs; rhs; _ } ->
    StringSet.union
      (aux vars defined lhs)
      (aux vars defined rhs)

  | T_Function _ ->
    prerr_endline "Warning: TODO find free variables in nested closures.";
    vars

  | T_If { predicate; consequent; alternative; _ } ->
    StringSet.(union
      (aux vars defined predicate)
      (union (aux vars defined consequent) (aux vars defined alternative)))

  | T_Tuple (first, second, _, _) ->
    StringSet.union (aux vars defined first) (aux vars defined second)

  | T_Print (value, _)
  | T_First (value, _, _)
  | T_Second (value, _, _) -> aux vars defined value
  in
  let parameters = List.fold_left
    (fun env (parameter, _) -> StringSet.add parameter env)
    StringSet.empty
    parameters
  in
  aux StringSet.empty parameters body

let rec compile global func env tree =
  match tree with
  | T_Let (name, T_Function fn, next, _, _) ->
    (* TODO: Pass actual refence to function.
       Either a closure or a global function. *)
    let env = Env.add name (Int32 0l) env in

    let free_variables = find_free_variables env fn.parameters fn.body in
    let rec aux vars =
      match vars () with
      | Seq.Cons (var, next) -> (var, Env.find var fn.env) :: aux next
      | Seq.Nil -> fn.parameters
    in
    let fn =
      { fn with
        parameters = aux (StringSet.to_seq free_variables) }
    in
    Queue.push
      (name, fn)
      remaining_functions;
    compile global func env next

  | T_Print (expr, _) ->
    let expr' = compile global func env expr in
    let str' = compile_string_convertion func (find_type expr) expr' in
    push_instruction func
      (Call (Void, "rinha_print", [ str' ]));
    expr'

  | T_Call { callee = T_Var (name, _, _); arguments; typ; _ } ->
    let args =
      arguments
      |> List.map
        (fun expr ->
          compile false func env expr)
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
    let l' = compile global func env lhs in
    let r' = compile global func env rhs in
    compile_string_concatenation func
      l' (find_type lhs)
      r' (find_type rhs)

  | T_Binary { lhs; op; rhs; typ; _ } ->
    let l' = compile global func env lhs in
    let r' = compile global func env rhs in
    compile_binary func op typ
      l' (find_type lhs)
      r' (find_type rhs)

  | T_Var (name, _, _) ->
    Env.find name env

  | T_If { predicate; consequent; alternative; typ; _ } ->
    let pred' = compile global func env predicate in
    let consequent_label = Llvm.create_label func "if_then" in
    let alternative_label = Llvm.create_label func "if_else" in
    let end_label = Llvm.create_label func "if_end" in

    push_instructions func
      [ Br (pred', consequent_label, alternative_label)
      ; Label alternative_label ];

    let alternative_value = compile global func env alternative in
    push_instructions func
      [ Br_Label end_label
      ; Label consequent_label ];

    let consequent_value = compile global func env consequent in
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
    let value' = compile false func env value in
    if global then (match (find_type value) with Int | Bool -> () | _ -> gc_add_global func value');
    compile global func (Env.add name value' env) next

  | T_Tuple (t0, t1, typ, _) ->
    let t0 = compile global func env t0 in
    let t1 = compile global func env t1 in
    let ptr = compile_alloc func 2l 2l in
    compile_array_store func typ ptr (Int32 0l) t0;
    compile_array_store func typ ptr (Int32 1l) t1;
    Ptr (type_to_llvm typ, ptr)

  | T_First (tuple, typ, _) ->
    compile_array_load func typ (compile global func env tuple) (Int32 0l)

  | T_Second (tuple, typ, _) ->
    compile_array_load func typ (compile global func env tuple) (Int32 1l)

  (* TODO:
    - Anonymous functions and lambda lifting
    - Closures (variables in scope) 
    - Dynamic dispatching *)

  | t ->
    Format.eprintf "ERROR: %a\n" pp_typed_tree t;
    exit 1

let compile_strings output =
  while not @@ Queue.is_empty remaining_strings do
    let label, str = Queue.pop remaining_strings in
    (* TODO: Escape strings *)
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
    let return_value = compile false llvm_function env func.body in
    push_instruction llvm_function
      (Ret return_value);
    compile_strings output;
    Llvm.write_to_file output llvm_function
  done

(* External functions *)
let header: (string * typ * typ list) list =
  [ "rinha_print"         , Void, [ Ptr ]
  ; "rinha_strcat"        , Void, [ Ptr; Ptr ]
  ; "rinha_int_to_string" , Ptr , [ Ptr; Ptr ]
  ; "rinha_alloc"         , Ptr , [ I32; I32 ]
  ; "rinha_init_memory"   , Void, []
  ; "rinha_gc_add_root"   , Void, [ Ptr ]
  ; "rinha_require_allocations", Void, [ I32 ]]

let compile_main output tree =
  List.iter
    (fun (name, return_type, arg_types) ->
      Llvm.write_declare output name return_type arg_types)
    header;

  let main = Llvm.create_function "main" I32 [] in
  push_instruction main
      (Call (Void, "rinha_init_memory", []));
  let _ = compile true main Env.empty tree in
  push_instruction main
    (Ret (Int32 0l));

  compile_strings output;
  Llvm.write_to_file output main;
  compile_remaining_functions output remaining_functions
