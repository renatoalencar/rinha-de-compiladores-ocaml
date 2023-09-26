
type typ =
  | I1
  | I8
  | I32
  | I64
  | Void
  | Ptr
  | Array of int * typ
  | Fn of typ * typ list
[@@deriving show]

type value =
  | Ptr of typ * value
  | Label of typ * string
  | Closure of (typ * string) list * value
  | Reg of typ * string
  | Int32 of int32
  | Int1 of int
[@@deriving show]

type cmp =
  | Sle
  | Slt
  | Eq
  | Ne
  | Sge
  | Sgt

type instr =
  | I_icmp of cmp * typ * value * value
  | I_add of typ * value * value
  | I_sub of typ * value * value
  | I_mul of typ * value * value
  | I_sdiv of typ * value * value
  | I_srem of typ * value * value
  | I_or of typ * value * value
  | I_and of typ * value * value
  | I_call of typ * value * value list
  | I_phi of typ * ( value * string ) * ( value * string )
  | I_bitcast of value * typ
  | I_select of value * value * value
  | I_load of typ * value * value
  | I_getelementptr of typ * value * value

type function_toplevel =
  | Assign of string * instr
  | Label of string
  | Call of typ * string * value list
  | Br of value * string * string
  | Br_Label of string
  | Ret of value
  | Store of value * value * value

type func =
  { name : string
  ; return_type : typ
  ; arguments : typ list
  ; body : function_toplevel Queue.t
  ; mutable counter : int
  ; mutable allocations : int32 }

let create_function name return_type arguments =
  { name; return_type; arguments; body = Queue.create (); allocations = 0l; counter = 0 }

let create_register func prefix =
  let register = Printf.sprintf "%%%s%d" prefix func.counter in
  func.counter <- func.counter + 1;
  register

let create_label func prefix =
  let label = Printf.sprintf "%s%d" prefix func.counter in
  func.counter <- func.counter + 1;
  label

let push_instruction func instr =
  Queue.push instr func.body

let push_instructions func instrs =
  List.iter (push_instruction func) instrs

let rec type_to_string = function
  | I1 -> "i1"
  | I8 -> "i8"
  | I32 -> "i32"
  | I64 -> "i64"
  | Void -> "void"
  | Ptr -> "ptr"
  | Array (size, typ) ->
    Printf.sprintf "[%d x %s]*" size (type_to_string typ)
  | Fn (ret, args) ->
    Printf.sprintf "%s (%s)"
       (type_to_string ret)
       (String.concat ", " @@ List.map type_to_string args)
       
let cmp_to_string = function
  | Sle -> "sle"
  | Slt -> "slt"
  | Eq -> "eq"
  | Ne -> "ne"
  | Sge -> "sge"
  | Sgt -> "sgt"

let rec value_to_string ?(ignore_type = false) typ =
  let typ, str =
  match typ with
    | Reg (typ, reg) -> typ, reg
    | Int32 int32 -> I32, Int32.to_string int32
    | Int1 value -> I1, string_of_int value
    | Label (typ, label) -> typ, "@" ^ label
    | Ptr (typ, value) -> typ, value_to_string ~ignore_type:true value
    | _ -> assert false
    (* | value -> Format.eprintf "ERROR %a\n" pp_value value; exit 1 *)
  in
  if ignore_type then str
  else type_to_string typ ^ " " ^ str

let find_llvm_type = function
  | Reg (typ, _) -> typ
  | Int32 _ -> I32
  | Label (typ, _) -> typ
  | _ -> assert false

let force_ptr = function
  | Reg (_, reg) -> Reg (Ptr, reg)
  | Label (_, name) -> Label (Ptr, name)
  | _ -> assert false

let write_call fmt typ fn arguments =
  (* TODO: Looks like LLVM can forgive this `tail` but we shouldn't count on it. *)
  Format.fprintf fmt "tail call %s %s(%s)"
    (type_to_string typ)
    (value_to_string ~ignore_type:true fn)
    (String.concat ", "
      @@ List.map value_to_string arguments)

let write_instr fmt = function
  | I_icmp (cmp, typ, x, y) ->
    Format.fprintf fmt "icmp %s %s %s, %s"
      (cmp_to_string cmp)
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_add (typ, x, y) ->
    Format.fprintf fmt "add %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_sub (typ, x, y) ->
    Format.fprintf fmt "sub %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_mul (typ, x, y) ->
    Format.fprintf fmt "mul %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_sdiv (typ, x, y) ->
    Format.fprintf fmt "sdiv %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_srem (typ, x, y) ->
    Format.fprintf fmt "srem %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_or (typ, x, y) ->
    Format.fprintf fmt "or %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_and (typ, x, y) ->
    Format.fprintf fmt "and %s %s, %s"
      (type_to_string typ)
      (value_to_string ~ignore_type:true x) (value_to_string ~ignore_type:true y)
  | I_call (typ, fn, arguments) ->
    write_call fmt typ fn arguments
  | I_phi (typ, (r1, l1), (r2, l2)) ->
    Format.fprintf fmt "phi %s [ %s, %%%s ], [ %s, %%%s ]"
      (type_to_string typ)
      (value_to_string ~ignore_type:true r1) l1
      (value_to_string ~ignore_type:true r2) l2
  | I_bitcast (value, typ) ->
    Format.fprintf fmt "bitcast %s to %s"
      (value_to_string value)
      (type_to_string typ)
  | I_select (v, t', f') ->
    Format.fprintf fmt "select %s, %s, %s"
      (value_to_string v)
      (value_to_string t')
      (value_to_string f')
  | I_load (typ, ptr, alignment) ->
    Format.fprintf fmt "load %s, %s ;;, align %s"
      (type_to_string typ)
      (value_to_string ptr)
      (value_to_string ~ignore_type:true alignment)
  | I_getelementptr (typ, ptr, pos) ->
    Format.fprintf fmt "getelementptr %s, %s, %s"
      (type_to_string typ)
      (value_to_string ptr)
      (value_to_string pos)

let write_toplevel fmt = function
  | Assign (dest_reg, instr) -> Format.fprintf fmt "\t%s = %a\n" dest_reg write_instr instr
  | Label name -> Format.fprintf fmt "%s:\n" name
  | Call (typ, name, arguments) ->
    Format.pp_print_char fmt '\t';
    write_call fmt typ (Label (Ptr, name)) arguments;
    Format.pp_print_char fmt '\n'
  | Br (cmp, if_true, if_false) ->
    Format.fprintf fmt "\tbr %s, label %%%s, label %%%s\n"
      (value_to_string cmp) if_true if_false
  | Br_Label name ->
    Format.fprintf fmt "\tbr label %%%s\n" name
  | Ret value ->
    Format.fprintf fmt "\tret %s\n" (value_to_string value)
  | Store (value, ptr, index) ->
    Format.fprintf fmt "\tstore %s, %s ;;, align %s\n"
      (value_to_string value) (value_to_string ptr) (value_to_string ~ignore_type:true index)

let write_to_file output { name; return_type; body; arguments; allocations; _ } =
  let fmt = Format.formatter_of_out_channel output in
  Format.fprintf fmt "define %s @%s(%s) {\n" (type_to_string return_type) name
    (arguments
      |> List.mapi (fun i typ -> Format.sprintf "%s %%%d" (type_to_string typ) i)
      |> String.concat ", ");

  if allocations > 0l then
    write_toplevel fmt (Call (Void, "rinha_require_allocations", [ Int32 allocations  ]));

  while not @@ Queue.is_empty body do
    let instr = Queue.pop body in
    write_toplevel fmt instr
  done;
  Format.fprintf fmt "}\n";
  Format.pp_print_flush fmt ()

let write_declare output name return_type arguments =
  let fmt = Format.formatter_of_out_channel output in
  Format.fprintf fmt "declare %s @%s(%s)\n"
    (type_to_string return_type)
    name
    (String.concat ", " @@ List.map type_to_string arguments);
  Format.pp_print_flush fmt ()

let write_string output label str =
  let fmt = Format.formatter_of_out_channel output in
  let len = String.length str in 
  Format.fprintf fmt "@%s = private constant <{ i64, [%d x i8] }> <{ i64 %d, [%d x i8] c\"%s\" }>, align 8 \n"
    label len len len str;
  Format.pp_print_flush fmt ()