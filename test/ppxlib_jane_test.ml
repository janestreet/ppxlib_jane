open! Core

let loc = Astlib.Location.none

module Jane_ast_builder = (val Ppxlib_jane.Ast_builder.make loc)

let print expression = Astlib.Pprintast.expression Format.std_formatter expression
let pat i = { ([%pat? _]) with ppat_desc = Ppat_var { txt = sprintf "x%d" i; loc } }

let cases ~n =
  List.init n ~f:(fun i : Ppxlib_ast.Ast.case ->
    { pc_lhs = pat i; pc_rhs = [%expr ()]; pc_guard = None })
;;

let param i = Jane_ast_builder.fun_param Nolabel (pat i)
let newtype_param : Jane_ast_builder.function_param = Pparam_newtype { txt = "a"; loc }

let%expect_test "add_params" =
  let add_params = Jane_ast_builder.add_fun_params in
  add_params [ param 0 ] [%expr ()] |> print;
  [%expect {| fun x0 -> () |}];
  add_params [ param 0; param 1 ] [%expr ()] |> print;
  [%expect {| fun x0 -> fun x1 -> () |}];
  add_params [ newtype_param; param 0 ] [%expr ()] |> print;
  [%expect {| fun (type a) -> fun x0 -> () |}];
  (* [make] just returns the body if there are no params. This makes it a drop-in
     replacement for code that used to call [List.fold_right] over a parameter list. *)
  add_params [] [%expr ()] |> print;
  [%expect {| () |}]
;;

let%expect_test "arity: eabstract vs. add_params" =
  let eabstract = Jane_ast_builder.eabstract ~coalesce_arity:false in
  let add_params = Jane_ast_builder.add_fun_params in
  let print_both params body ~expect_same =
    let for_add_params =
      add_params (List.map ~f:(Jane_ast_builder.fun_param Nolabel) params) body
    in
    let for_eabstract = eabstract params body in
    let for_add_params =
      print for_add_params;
      [%expect.output]
    in
    let for_eabstract =
      print for_eabstract;
      [%expect.output]
    in
    printf "add_params: %s\neabstract: %s\n" for_add_params for_eabstract;
    if expect_same
    then
      Expect_test_helpers_core.require_equal
        [%here]
        (module String)
        for_add_params
        for_eabstract
    else
      Expect_test_helpers_core.require_not_equal
        [%here]
        (module String)
        for_add_params
        for_eabstract
  in
  print_both [ pat 0 ] [%expr ()] ~expect_same:true;
  [%expect {|
    add_params: fun x0 -> ()
    eabstract: fun x0 -> () |}];
  print_both [ pat 0 ] (eabstract [ pat 1 ] [%expr ()]) ~expect_same:true;
  [%expect
    {|
    add_params: fun x0 -> fun x1 -> ()
    eabstract: fun x0 -> fun x1 -> () |}];
  print_both [ pat 0; pat 1 ] (eabstract [ pat 2; pat 3 ] [%expr ()]) ~expect_same:true;
  [%expect
    {|
    add_params: fun x0 -> fun x1 -> fun x2 -> fun x3 -> ()
    eabstract: fun x0 -> fun x1 -> fun x2 -> fun x3 -> () |}]
;;

let%expect_test "add_param" =
  let add_param = Jane_ast_builder.add_fun_param in
  add_param Nolabel None (pat 0) [%expr ()] |> print;
  [%expect {| fun x0 -> () |}];
  add_param Nolabel None (pat 0) (add_param Nolabel None (pat 1) [%expr ()]) |> print;
  [%expect {| fun x0 -> fun x1 -> () |}]
;;

let%expect_test "unary_function" =
  let unary_function = Jane_ast_builder.unary_function in
  unary_function (cases ~n:1) |> print;
  [%expect {| function | x0 -> () |}];
  unary_function (cases ~n:2) |> print;
  [%expect {| function | x0 -> () | x1 -> () |}];
  unary_function (cases ~n:3) |> print;
  [%expect {| function | x0 -> () | x1 -> () | x2 -> () |}]
;;
