
type typ =
  | I1
  | I32
  | I64
  | Void

type cmp =
  | Slt

type register = string

type instr =
  | I_icmp of cmp * typ * register * register
  | I_add of typ * register * register
  | I_sub of typ * register * register
  | I_call of typ * string * (typ * register) list
  | I_phi of typ * ( register * string ) * ( register * string )

type function_toplevel =
  | Assign of register * instr
  | Label of string
  | Call of typ * string * (typ * register) list
  | Br of register * string * string
  | Br_Label of string
  | Ret of typ * register

type func =
  { name : string
  ; return_type : typ
  ; arguments : typ list
  ; body : function_toplevel Queue.t
  ; mutable counter : int }

let create_function name return_type arguments =
  { name; return_type; arguments; body = Queue.create (); counter = 0 }

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

let type_to_string = function
  | I1 -> "i1"
  | I32 -> "i32"
  | I64 -> "i64"
  | Void -> "void"

let cmp_to_string = function
  | Slt -> "slt"

let write_call fmt typ name arguments =
  Format.fprintf fmt "call %s @%s(%s)"
    (type_to_string typ)
    name
    (String.concat ", "
      @@ List.map (fun (typ, reg) -> Format.sprintf "%s %s" (type_to_string typ) reg)
        arguments)

let write_instr fmt = function
  | I_icmp (cmp, typ, x, y) ->
    Format.fprintf fmt "icmp %s %s %s, %s"
      (cmp_to_string cmp)
      (type_to_string typ)
      x y
  | I_add (typ, x, y) ->
    Format.fprintf fmt "add %s %s, %s"
      (type_to_string typ)
      x y
  | I_sub (typ, x, y) ->
    Format.fprintf fmt "sub %s %s, %s"
      (type_to_string typ)
      x y
  | I_call (typ, name, arguments) ->
    write_call fmt typ name arguments
  | I_phi (typ, (r1, l1), (r2, l2)) ->
    Format.fprintf fmt "phi %s [ %s, %%%s ], [ %s, %%%s ]"
      (type_to_string typ)
      r1 l1
      r2 l2


let write_toplevel fmt = function
  | Assign (dest_reg, instr) -> Format.fprintf fmt "\t%s = %a\n" dest_reg write_instr instr
  | Label name -> Format.fprintf fmt "%s:\n" name
  | Call (typ, name, arguments) ->
    Format.pp_print_char fmt '\t';
    write_call fmt typ name arguments;
    Format.pp_print_char fmt '\n'
  | Br (cmp, if_true, if_false) ->
    Format.fprintf fmt "\tbr i1 %s, label %%%s, label %%%s\n"
      cmp if_true if_false
  | Br_Label name ->
    Format.fprintf fmt "\tbr label %%%s\n" name
  | Ret (typ, value) ->
    Format.fprintf fmt "\tret %s %s\n" (type_to_string typ) value

let write_to_file output { name; return_type; body; arguments; _ } =
  let fmt = Format.formatter_of_out_channel output in
  Format.fprintf fmt "define %s @%s(%s) {\n" (type_to_string return_type) name
    (arguments
      |> List.mapi (fun i typ -> Format.sprintf "%s %%%d" (type_to_string typ) i)
      |> String.concat ", ");
  while not @@ Queue.is_empty body do
    let instr = Queue.pop body in
    write_toplevel fmt instr
  done;
  Format.fprintf fmt "}\n"

let write_declare output name return_type arguments =
  let fmt = Format.formatter_of_out_channel output in
  Format.fprintf fmt "declare %s @%s(%s)\n"
    (type_to_string return_type)
    name
    (String.concat ", " @@ List.map type_to_string arguments);
  Format.pp_print_flush fmt ()