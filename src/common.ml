open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

let not_a_type_parameter ~loc =
  Astlib.Location.Error.make { loc; txt = "not a named type parameter" } ~sub:[]
;;

let get_type_param_name_and_jkind_res (ty, _) =
  let loc = ty.ptyp_loc in
  match Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
  | Ptyp_var (name, jkind) -> Ok ({ loc; txt = name }, jkind)
  | _ -> Error (not_a_type_parameter ~loc)
;;

let get_type_param_name_and_jkind t =
  match get_type_param_name_and_jkind_res t with
  | Ok e -> e
  | Error err -> Ppxlib_ast.Location_error.raise err
;;

let as_unlabeled_tuple components =
  if List.for_all (fun (label, _) -> Option.is_none label) components
  then Some (List.map snd components)
  else None
;;
