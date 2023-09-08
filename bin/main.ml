open Rinha
open Parsetree

let dummy_loc = { start = 0; _end = 0; filename = "<stdin>" }

let fib =
  E_Let
    ("fib"
    , E_Function
        { parameters = [ ("n", dummy_loc) ]
        ; body = E_If
            { predicate = E_Binary { lhs = E_Var ("n", dummy_loc)
                                   ; op = Lt
                                   ; rhs = E_Int (2L, dummy_loc)
                                   ; loc = dummy_loc}
            ; consequent = E_Var ("n", dummy_loc)
            ; alternative =
                E_Binary
                  { lhs =
                      E_Call
                        { callee = E_Var ("fib", dummy_loc)
                        ; arguments =
                            [ E_Binary { lhs = E_Var ("n", dummy_loc)
                                       ; op = Sub
                                       ; rhs = E_Int (1L, dummy_loc)
                                       ; loc = dummy_loc} ]
                        ; loc = dummy_loc }
                  ; op = Add
                  ; rhs =
                      E_Call
                        { callee = E_Var ("fib", dummy_loc)
                        ; arguments =
                            [ E_Binary { lhs = E_Var ("n", dummy_loc)
                                      ; op = Sub
                                      ; rhs = E_Int (2L, dummy_loc)
                                      ; loc = dummy_loc} ]
                        ; loc = dummy_loc }
                  ; loc = dummy_loc }
            ; loc = dummy_loc }
        ; loc = dummy_loc }
    , E_Print
        (E_Call
          { callee = E_Var ("fib", dummy_loc)
          ; arguments = [ E_Int (10L, dummy_loc) ]
          ; loc = dummy_loc}
        , dummy_loc)
    , dummy_loc)

let tree = Typedtree.of_parsed_tree fib

let () = Format.printf "%a\n" Typedtree.pp_typed_tree tree

let _ = print_endline "SUCESS"

let _ =
  Hashtbl.iter
    (fun key value -> Format.printf "%d -> %a\n" key Typedtree.pp_typ value)
    Typedtree.type_env