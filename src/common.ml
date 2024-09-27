open Shadow_compiler_distribution
open Parsetree
open Asttypes

let not_a_type_parameter ~loc =
  Astlib.Location.Error.make { loc; txt = "not a named type parameter" } ~sub:[]
;;

let get_type_param_name_and_jkind_res (ty, _) =
  let loc = ty.ptyp_loc in
  match Jane_syntax.Core_type.of_ast ty with
  | Some (Jtyp_layout (Ltyp_var { name = Some name; jkind }), _) ->
    Ok ({ Location.loc; txt = name }, Some jkind)
  | Some _ -> Error (not_a_type_parameter ~loc)
  | None ->
    (match ty.ptyp_desc with
     | Ptyp_var name -> Ok ({ loc; txt = name }, None)
     | _ -> Error (not_a_type_parameter ~loc))
;;

let get_type_param_name_and_jkind t =
  match get_type_param_name_and_jkind_res t with
  | Ok e -> e
  | Error err -> Ppxlib_ast.Location_error.raise err
;;
