open Rinha

let main () =
  let input_filename = Sys.argv.(1) in

  let parsetree =
    let in_channel = open_in input_filename in
    let parsetree = Json.of_channel in_channel in
    close_in in_channel;
    parsetree
  in

  let typedtree = Typedtree.of_parsed_tree parsetree in

  let output_filename = Filename.temp_file "rinha" ".ll" in
  let output = open_out output_filename in
  Codegen.compile_main output typedtree;
  close_out output;

  ignore @@
    Sys.command
      (String.concat " "
        [ "clang -o"; Sys.argv.(2); "runtime.o"; output_filename ])

let () = main ()