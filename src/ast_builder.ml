open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
open Stdppx
include Ast_builder_intf
include Types

let core_type ptyp_desc ~loc:ptyp_loc =
  { ptyp_desc; ptyp_loc; ptyp_attributes = []; ptyp_loc_stack = [] }
;;

module Default = struct
  include Types

  let mark_type_with_mode_expr modes ty =
    let attr = Jane_syntax.Mode_expr.attr_of modes in
    match attr with
    | None -> ty
    | Some attr -> { ty with ptyp_attributes = attr :: ty.ptyp_attributes }
  ;;

  let mode_expr_of_mode ~loc mode =
    match mode with
    | None -> Jane_syntax.Mode_expr.empty
    | Some Local ->
      let mode = Jane_syntax.Mode_expr.Const.mk "local" loc in
      { txt = [ mode ]; loc }
  ;;

  let mark_type_with_mode ~loc mode ty =
    mark_type_with_mode_expr (mode_expr_of_mode ~loc mode) ty
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

  let tarrow_maybe ~loc args result_type =
    match args with
    | [] -> result_type
    | _ :: _ -> tarrow ~loc args { result_mode = None; result_type }
  ;;

  let get_mode ty =
    let modes, ptyp_attributes = Jane_syntax.Mode_expr.of_attrs ty.ptyp_attributes in
    let mode =
      match (modes.txt : Jane_syntax.Mode_expr.Const.t list :> _ Location.loc list) with
      | [] -> None
      | [ { txt = "local"; _ } ] -> Some Local
      | _ -> raise (Invalid_argument "Unrecognized modes")
    in
    mode, { ty with ptyp_attributes }
  ;;

  let mode_expr_of_modality ~loc cmo ld =
    match cmo, ld with
    | None, _ -> Jane_syntax.Mode_expr.empty
    | Some Global, (None | Some { pld_mutable = Immutable; _ }) ->
      let mode = Jane_syntax.Mode_expr.Const.mk "global" loc in
      { txt = [ mode ]; loc }
    | Some Global, Some { pld_mutable = Mutable; _ } ->
      raise (Invalid_argument "record fields cannot be marked as both global and mutable")
  ;;

  let mark_type_with_modality ~loc cmo ty =
    mark_type_with_mode_expr (mode_expr_of_modality ~loc cmo None) ty
  ;;

  let mark_label_with_mode_expr modes ld =
    let pld_type = mark_type_with_mode_expr modes ld.pld_type in
    { ld with pld_type }
  ;;

  let mark_label_with_modality ~loc cmo ld =
    mark_label_with_mode_expr (mode_expr_of_modality ~loc cmo (Some ld)) ld
  ;;

  let pcstr_tuple ~loc modes_tys =
    Pcstr_tuple
      (List.map modes_tys ~f:(fun (mode, ty) -> mark_type_with_modality ~loc mode ty))
  ;;

  let add_modes_to_label_declarations ~for_ ~loc modes_lds =
    match modes_lds with
    | [] -> raise (Invalid_argument (for_ ^ ": records must have at least one field"))
    | _ :: _ ->
      List.map modes_lds ~f:(fun (mode, ld) -> mark_label_with_modality ~loc mode ld)
  ;;

  let pcstr_record ~loc modes_lds =
    Pcstr_record (add_modes_to_label_declarations ~for_:"pcstr_record" ~loc modes_lds)
  ;;

  let ptype_record ~loc modes_lds =
    Ptype_record (add_modes_to_label_declarations ~for_:"ptyp_record" ~loc modes_lds)
  ;;

  let get_attributes_modality attrs =
    let modalities, rest = Jane_syntax.Mode_expr.of_attrs attrs in
    let modality =
      let modalities =
        (modalities.txt : Jane_syntax.Mode_expr.Const.t list :> _ Location.loc list)
      in
      match modalities with
      | [] -> None
      | [ { txt = "global"; _ } ] -> Some Global
      | _ -> raise (Invalid_argument "Unrecognized modalities")
    in
    modality, rest
  ;;

  let get_tuple_field_modality carg =
    let modality, ptyp_attributes = get_attributes_modality carg.ptyp_attributes in
    modality, { carg with ptyp_attributes }
  ;;

  let get_label_declaration_modality ld =
    let modality, pld_type = get_tuple_field_modality ld.pld_type in
    modality, { ld with pld_type }
  ;;

  let n_ary_function ~loc ~attrs ~params ~ty_constraint ~body =
    let expr = Jane_syntax.N_ary_functions.expr_of (params, ty_constraint, body) ~loc in
    match attrs with
    | [] -> expr
    | _ :: _ as attrs -> { expr with pexp_attributes = expr.pexp_attributes @ attrs }
  ;;

  let match_n_ary_function ast =
    match Jane_syntax.Expression.of_ast ast with
    | Some (Jexp_n_ary_function (params, ty_constraint, body), attrs) ->
      Some (params, ty_constraint, body, attrs)
    | _ -> None
  ;;

  let unary_function ~loc ?(attrs = []) cases =
    n_ary_function
      ~attrs
      ~params:[]
      ~ty_constraint:None
      ~body:(Pfunction_cases (cases, loc, []))
      ~loc
  ;;

  let fun_param ~loc arg_label pattern =
    { pparam_desc = Pparam_val (arg_label, None, pattern); pparam_loc = loc }
  ;;

  let add_fun_params ~loc ?(attrs = []) new_params body =
    match new_params with
    | [] -> body
    | _ :: _ ->
      (* If the body is already a function, extend its arity rather than creating a new
         function.
      *)
      (match match_n_ary_function body with
       | Some (params, ty_constraint, body, existing_attrs) ->
         let existing_attrs =
           List.filter existing_attrs ~f:(fun attr ->
             (* We drop "merlin.loc" attributes inserted by merlin's parser.

                These attributes are always fine to drop -- they're a best-effort attempt
                to encode extra location information -- and usually not fine to move.
                That's because merlin expects certain invariants to hold between
                the location encoded by the "merlin.loc" and locations of sub-ASTs.
             *)
             String.( <> ) attr.attr_name.txt "merlin.loc")
         in
         n_ary_function
           ~params:(new_params @ params)
           ~ty_constraint
           ~body
           ~loc
           ~attrs:(existing_attrs @ attrs)
       | None ->
         n_ary_function
           ~params:new_params
           ~ty_constraint:None
           ~body:(Pfunction_body body)
           ~loc
           ~attrs)
  ;;

  let add_fun_param ~loc ?attrs lbl def pat body =
    add_fun_params
      ?attrs
      ~loc
      [ { pparam_desc = Pparam_val (lbl, def, pat); pparam_loc = pat.ppat_loc } ]
      body
  ;;

  let coalesce_fun_arity ast =
    match match_n_ary_function ast with
    | None | Some (_, Some _, _, _) | Some (_, _, Pfunction_cases _, _) -> ast
    | Some (params1, None, Pfunction_body outer_body, outer_attrs) ->
      (match match_n_ary_function outer_body with
       | Some (params2, ty_constraint, inner_body, []) ->
         n_ary_function
           ~params:(params1 @ params2)
           ~ty_constraint
           ~body:inner_body
           ~loc:ast.pexp_loc
           ~attrs:outer_attrs
       | Some (_, _, _, _ :: _) | None -> ast)
  ;;

  let eabstract ~loc ?(coalesce_fun_arity = true) pats body =
    let params = List.map pats ~f:(fun pat -> fun_param ~loc:pat.ppat_loc Nolabel pat) in
    if coalesce_fun_arity
    then add_fun_params ~loc params body
    else
      n_ary_function
        ~loc
        ~params
        ~ty_constraint:None
        ~body:(Pfunction_body body)
        ~attrs:[]
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
  let tarrow_maybe args res : core_type = tarrow_maybe ~loc args res
  let pcstr_tuple fields : constructor_arguments = pcstr_tuple ~loc fields
  let pcstr_record labels : constructor_arguments = pcstr_record ~loc labels
  let ptype_record labels : type_kind = ptype_record ~loc labels

  let eabstract ?coalesce_fun_arity a b : expression =
    eabstract ~loc ?coalesce_fun_arity a b
  ;;

  let fun_param a b : function_param = fun_param ~loc a b
  let unary_function ?attrs a : expression = unary_function ~loc ?attrs a
  let add_fun_param ?attrs a b c d : expression = add_fun_param ~loc ?attrs a b c d
  let add_fun_params ?attrs a b : expression = add_fun_params ~loc ?attrs a b
end

let make loc : (module S_with_implicit_loc) =
  (module Make (struct
    let loc = loc
  end))
;;
