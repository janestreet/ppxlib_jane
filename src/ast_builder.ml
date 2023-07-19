open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
open Stdppx
include Ast_builder_intf

let expression pexp_desc ~loc:pexp_loc =
  { pexp_desc; pexp_loc; pexp_attributes = []; pexp_loc_stack = [] }
;;

module Default = struct
  type function_param =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc

  let n_ary_function ~(loc : Location.t) params body =
    List.fold_right params ~init:body ~f:(fun param acc ->
      match param with
      | Pparam_val (arg_label, optional_default, pattern) ->
        expression (Pexp_fun (arg_label, optional_default, pattern, acc)) ~loc
      | Pparam_newtype newtype -> expression (Pexp_newtype (newtype, acc)) ~loc)
  ;;

  let unary_function ~loc cases = expression (Pexp_function cases) ~loc
  let fun_param arg_label pattern = Pparam_val (arg_label, None, pattern)

  let add_fun_param_internal ~loc param body = n_ary_function ~loc [ param ] body

  let add_fun_param ~loc lbl def pat body =
    add_fun_param_internal ~loc (Pparam_val (lbl, def, pat)) body
  ;;

  let add_fun_params ~loc params body =
    List.fold_right params ~init:body ~f:(fun param body ->
      add_fun_param_internal ~loc param body)
  ;;

  let eabstract ~loc ?(coalesce_arity = true) pats body =
    let params = List.map pats ~f:(fun pat -> fun_param Nolabel pat) in
    if coalesce_arity
    then add_fun_params ~loc params body
    else n_ary_function ~loc params body
  ;;
end

module Make (Loc : sig
    val loc : Location.t
  end) =
struct
  include Default

  let loc = Loc.loc
  let eabstract ?coalesce_arity a b : expression = eabstract ~loc ?coalesce_arity a b
  let unary_function a : expression = unary_function ~loc a
  let add_fun_param a b c d : expression = add_fun_param ~loc a b c d
  let add_fun_params a b : expression = add_fun_params ~loc a b
end

let make loc : (module S_with_implicit_loc) =
  (module Make (struct
       let loc = loc
     end))
;;
