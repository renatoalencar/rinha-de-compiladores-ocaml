open Rinha

let compile_module modname =
  let llc = Option.value ~default:"llc" @@ Sys.getenv_opt "LLC" in
  let output_filename = Filename.temp_file "rinha" ".o" in
  let command =
    String.concat " "
    [ llc; "-o"; output_filename; "--filetype=obj"; modname ]
  in
  prerr_endline command;
  ignore @@ Sys.command command;
  output_filename

let link_program modules outfile =
  let ld = Option.value ~default:"gold" @@ Sys.getenv_opt "LD" in
  let ld_flags = Option.value ~default:"" @@ Sys.getenv_opt "LD_FLAGS" in
  let command =
    String.concat " "
        ([ ld; ld_flags; "-o"; outfile; "-static" ] @ modules)
  in
  prerr_endline command;
  ignore @@ Sys.command command
      
let main () =
  (* TODO: Use Cmdliner or something else *)
  let input_filename = Sys.argv.(1) in

  let parsetree =
    let in_channel =
      if input_filename = "-" then stdin
      else open_in input_filename
    in
    let parsetree = Json.of_channel in_channel in
    close_in in_channel;
    parsetree
  in

  let typedtree = Typedtree.of_parsed_tree parsetree in
  (* Format.printf "%a\n" Typedtree.pp_typed_tree typedtree; *)

  (* TODO: Use the file base name *)
  let output_filename = Filename.temp_file "rinha" ".ll" in
  let output = open_out output_filename in
  Codegen.compile_main output typedtree;
  close_out output;

  let objfile = compile_module output_filename in
  link_program [objfile; "runtime.o" ] Sys.argv.(2)

let () = main ()
