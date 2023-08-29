open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
open Stdppx
include Ast_builder_intf
include Types

let expression pexp_desc ~loc:pexp_loc =
  { pexp_desc; pexp_loc; pexp_attributes = []; pexp_loc_stack = [] }
;;

let core_type ptyp_desc ~loc:ptyp_loc =
  { ptyp_desc; ptyp_loc; ptyp_attributes = []; ptyp_loc_stack = [] }
;;

module Default = struct
  include Types


  let add_extension_attribute ~loc attrs name =
    attrs
    @ [ { attr_name = { txt = "extension." ^ name; loc }
        ; attr_payload = PStr []
        ; attr_loc = loc
        }
      ]
  ;;

  let mark_type_with_extension ~loc name ty =
    { ty with ptyp_attributes = add_extension_attribute ~loc ty.ptyp_attributes name }
  ;;

  let mark_type_with_mode ~loc mode ty =
    match mode with
    | None -> ty
    | Some Local -> mark_type_with_extension ~loc "local" ty
  ;;

  let ptyp_arrow ~loc { arg_label; arg_mode; arg_type } { result_mode; result_type } =
    core_type
      ~loc
      (Ptyp_arrow
         ( arg_label
         , mark_type_with_mode ~loc arg_mode arg_type
         , mark_type_with_mode ~loc result_mode result_type ))
  ;;

  let tarrow ~loc args result =
    match args with
    | [] ->
      raise
        (Invalid_argument
           "tarrow: Can't construct a 0-ary arrow, argument list must be nonempty")
    | _ :: _ ->
      let result_mode_and_type =
        let { result_mode; result_type } = result in
        mark_type_with_mode ~loc result_mode result_type
      in
      List.fold_right
        args
        ~init:result_mode_and_type
        ~f:(fun { arg_label; arg_mode; arg_type } arrow_type ->
          let arg_type = mark_type_with_mode ~loc arg_mode arg_type in
          core_type ~loc (Ptyp_arrow (arg_label, arg_type, arrow_type)))
  ;;

  let get_mode ty =
    match
      List.partition ty.ptyp_attributes ~f:(function
        | { attr_name = { txt = "extension.local" | "ocaml.local" | "local"; loc = _ }
          ; attr_payload = PStr []
          ; attr_loc = _
          } -> true
        | _ -> false)
    with
    | [], _ -> None, ty
    | _ :: _, ptyp_attributes -> Some Local, { ty with ptyp_attributes }
  ;;

  let mark_type_with_modality ~loc cmo ty =
    match cmo with
    | None -> ty
    | Some Global -> mark_type_with_extension ~loc "global" ty
  ;;

  let mark_label_with_mode ~loc cmo ld =
    match cmo, ld with
    | None, _ -> ld
    | Some Global, { pld_mutable = Immutable; _ } ->
      { ld with pld_attributes = add_extension_attribute ~loc ld.pld_attributes "global" }
    | Some Global, { pld_mutable = Mutable; _ } ->
      raise (Invalid_argument "record fields cannot be marked as both global and mutable")
  ;;

  let pcstr_tuple ~loc modes_tys =
    Pcstr_tuple
      (List.map modes_tys ~f:(fun (mode, ty) -> mark_type_with_modality ~loc mode ty))
  ;;

  let add_modes_to_label_declarations ~for_ ~loc modes_lds =
    match modes_lds with
    | [] -> raise (Invalid_argument (for_ ^ ": records must have at least one field"))
    | _ :: _ ->
      List.map modes_lds ~f:(fun (mode, ld) -> mark_label_with_mode ~loc mode ld)
  ;;

  let pcstr_record ~loc modes_lds =
    Pcstr_record (add_modes_to_label_declarations ~for_:"pcstr_record" ~loc modes_lds)
  ;;

  let ptype_record ~loc modes_lds =
    Ptype_record (add_modes_to_label_declarations ~for_:"ptyp_record" ~loc modes_lds)
  ;;

  let get_tuple_field_modality carg =
    match
      List.partition carg.ptyp_attributes ~f:(function
        | { attr_name = { txt = "extension.global" | "ocaml.global" | "global"; loc = _ }
          ; attr_payload = PStr []
          ; attr_loc = _
          } -> true
        | _ -> false)
    with
    | [], _ -> None, carg
    | _ :: _, ptyp_attributes -> Some Global, { carg with ptyp_attributes }
  ;;

  let get_label_declaration_modality ld =
    match
      List.partition ld.pld_attributes ~f:(function
        | { attr_name = { txt = "extension.global" | "ocaml.global" | "global"; loc = _ }
          ; attr_payload = PStr []
          ; attr_loc = _
          } -> true
        | _ -> false)
    with
    | [], _ -> None, ld
    | _ :: _, pld_attributes -> Some Global, { ld with pld_attributes }
  ;;

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
  let ptyp_arrow arg res : core_type = ptyp_arrow ~loc arg res
  let tarrow args res : core_type = tarrow ~loc args res
  let pcstr_tuple fields : constructor_arguments = pcstr_tuple ~loc fields
  let pcstr_record labels : constructor_arguments = pcstr_record ~loc labels
  let ptype_record labels : type_kind = ptype_record ~loc labels
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
