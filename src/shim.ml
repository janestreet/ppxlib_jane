open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
module Ast_helper = Ppxlib_ast.Ast_helper

type mode = Local (** [local_ ty] *)

type arrow_argument =
  { arg_label : arg_label
  ; arg_mode : mode option
  ; arg_type : core_type
  }

type arrow_result =
  { result_mode : mode option
  ; result_type : core_type
  }

type modality =
  | Global (** [C of (..., global_ ty, ...)] or [{ ...; global_ l : ty; ... }]. *)

module Pcstr_tuple_arg = struct
  type t = core_type

  let extract_modality t = None, t
  let to_core_type t = t
  let of_core_type core_type = core_type
  let map_core_type t ~f = f t
  let map_core_type_extra t ~f = f t
  let create ~loc:_ ~modality:_ ~type_ = type_
end

module Label_declaration = struct
  let extract_modality ld = None, ld

  let create ~loc ~name ~mutable_ ~modality:_ ~type_ =
    { pld_loc = loc
    ; pld_name = name
    ; pld_type = type_
    ; pld_mutable = mutable_
    ; pld_attributes = []
    }
  ;;
end

module Value_description = struct
  let extract_modality vd = None, vd

  let create ~loc ~name ~type_ ~modality:_ ~prim =
    { pval_loc = loc
    ; pval_name = name
    ; pval_type = type_
    ; pval_prim = prim
    ; pval_attributes = []
    }
  ;;
end

type mode_const_expression = string Location.loc
type mode_expression = mode_const_expression list Location.loc
type jkind_const_annotation = string Location.loc

type jkind_annotation =
  | Default
  | Abbreviation of jkind_const_annotation
  | Mod of jkind_annotation * mode_expression
  | With of jkind_annotation * core_type
  | Kind_of of core_type

module Pexp_function = struct
  type function_param_desc =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc * jkind_annotation loc option

  type function_param =
    { pparam_loc : Location.t
    ; pparam_desc : function_param_desc
    }

  type type_constraint =
    | Pconstraint of core_type
    | Pcoerce of core_type option * core_type

  type function_constraint =
    { mode_annotations : mode_expression
    ; type_constraint : type_constraint
    }

  type function_body =
    | Pfunction_body of expression
    | Pfunction_cases of case list * Location.t * attributes

  let to_parsetree ~params ~constraint_ ~body =
    let body =
      match body with
      | Pfunction_body body -> body
      | Pfunction_cases (cases, loc, pexp_attributes) ->
        Ast_helper.Exp.function_ cases ~loc ~attrs:pexp_attributes
    in
    let body =
      match constraint_ with
      | None -> body
      | Some { type_constraint = Pconstraint ty; _ } ->
        let body_loc = { body.pexp_loc with loc_ghost = true } in
        Ast_helper.Exp.constraint_ body ty ~loc:body_loc
      | Some { type_constraint = Pcoerce (ty1, ty2); _ } ->
        let body_loc = { body.pexp_loc with loc_ghost = true } in
        Ast_helper.Exp.coerce body ty1 ty2 ~loc:body_loc
    in
    let fun_ =
      ListLabels.fold_right params ~init:body ~f:(fun param body ->
        let loc : Location.t =
          { loc_start = param.pparam_loc.loc_start
          ; loc_end = body.pexp_loc.loc_end
          ; loc_ghost = true
          }
        in
        match param.pparam_desc with
        | Pparam_val (lbl, eo, pat) -> Ast_helper.Exp.fun_ lbl eo pat body ~loc
        | Pparam_newtype (newtype, _) -> Ast_helper.Exp.newtype newtype body ~loc)
    in
    fun_.pexp_desc
  ;;

  let of_parsetree =
    let body_of_parsetree body =
      match body.pexp_desc with
      | Pexp_function cases -> Pfunction_cases (cases, body.pexp_loc, body.pexp_attributes)
      | _ -> Pfunction_body body
    in
    let finish ~rev_params ~constraint_ ~body =
      let body = body_of_parsetree body in
      match rev_params, constraint_, body with
      | [], _, Pfunction_body _ -> None
      | [], Some _, Pfunction_cases _ -> None
      | rev_params, constraint_, body ->
        let constraint_ : function_constraint option =
          match constraint_ with
          | None -> None
          | Some type_constraint ->
            Some { mode_annotations = { txt = []; loc = Location.none }; type_constraint }
        in
        Some (List.rev rev_params, constraint_, body)
    in
    let rec loop_expr_desc expr_desc ~rev_params ~containing_expr =
      match expr_desc with
      | Pexp_newtype (ty, body) ->
        loop_expr
          body
          ~rev_params:
            ({ pparam_loc = ty.loc; pparam_desc = Pparam_newtype (ty, None) }
             :: rev_params)
      | Pexp_fun (lbl, eo, pat, body) ->
        loop_expr
          body
          ~rev_params:
            ({ pparam_loc = pat.ppat_loc; pparam_desc = Pparam_val (lbl, eo, pat) }
             :: rev_params)
      | Pexp_constraint (body, ty) ->
        finish ~rev_params ~constraint_:(Some (Pconstraint ty)) ~body
      | _ ->
        (match containing_expr with
         | None -> None
         | Some body -> finish ~rev_params ~constraint_:None ~body)
    and loop_expr expr ~rev_params =
      match expr.pexp_attributes with
      | _ :: _ -> finish ~rev_params ~constraint_:None ~body:expr
      | [] -> loop_expr_desc expr.pexp_desc ~rev_params ~containing_expr:(Some expr)
    in
    fun expr_desc ~loc ->
      match expr_desc with
      | Pexp_function cases -> Some ([], None, Pfunction_cases (cases, loc, []))
      | _ -> loop_expr_desc expr_desc ~rev_params:[] ~containing_expr:None
  ;;
end

module Expression_desc = struct
  type t =
    | Pexp_ident of Longident.t loc
    | Pexp_constant of constant
    | Pexp_let of rec_flag * value_binding list * expression
    | Pexp_function of
        Pexp_function.function_param list
        * Pexp_function.function_constraint option
        * Pexp_function.function_body
    | Pexp_apply of expression * (arg_label * expression) list
    | Pexp_match of expression * case list
    | Pexp_try of expression * case list
    | Pexp_tuple of expression list
    | Pexp_construct of Longident.t loc * expression option
    | Pexp_variant of label * expression option
    | Pexp_record of (Longident.t loc * expression) list * expression option
    | Pexp_field of expression * Longident.t loc
    | Pexp_setfield of expression * Longident.t loc * expression
    | Pexp_array of expression list
    | Pexp_ifthenelse of expression * expression * expression option
    | Pexp_sequence of expression * expression
    | Pexp_while of expression * expression
    | Pexp_for of pattern * expression * expression * direction_flag * expression
    | Pexp_constraint of expression * core_type
    | Pexp_coerce of expression * core_type option * core_type
    | Pexp_send of expression * label loc
    | Pexp_new of Longident.t loc
    | Pexp_setinstvar of label loc * expression
    | Pexp_override of (label loc * expression) list
    | Pexp_letmodule of string option loc * module_expr * expression
    | Pexp_letexception of extension_constructor * expression
    | Pexp_assert of expression
    | Pexp_lazy of expression
    | Pexp_poly of expression * core_type option
    | Pexp_object of class_structure
    | Pexp_newtype of string loc * expression
    | Pexp_pack of module_expr
    | Pexp_open of open_declaration * expression
    | Pexp_letop of letop
    | Pexp_extension of extension
    | Pexp_unreachable

  let to_parsetree : t -> expression_desc = function
    | Pexp_function (x1, x2, x3) ->
      Pexp_function.to_parsetree ~params:x1 ~constraint_:x2 ~body:x3
    | Pexp_ident x -> Pexp_ident x
    | Pexp_constant x -> Pexp_constant x
    | Pexp_let (x1, x2, x3) -> Pexp_let (x1, x2, x3)
    | Pexp_apply (x1, x2) -> Pexp_apply (x1, x2)
    | Pexp_match (x1, x2) -> Pexp_match (x1, x2)
    | Pexp_try (x1, x2) -> Pexp_try (x1, x2)
    | Pexp_tuple x -> Pexp_tuple x
    | Pexp_construct (x1, x2) -> Pexp_construct (x1, x2)
    | Pexp_variant (x1, x2) -> Pexp_variant (x1, x2)
    | Pexp_record (x1, x2) -> Pexp_record (x1, x2)
    | Pexp_field (x1, x2) -> Pexp_field (x1, x2)
    | Pexp_setfield (x1, x2, x3) -> Pexp_setfield (x1, x2, x3)
    | Pexp_array x -> Pexp_array x
    | Pexp_ifthenelse (x1, x2, x3) -> Pexp_ifthenelse (x1, x2, x3)
    | Pexp_sequence (x1, x2) -> Pexp_sequence (x1, x2)
    | Pexp_while (x1, x2) -> Pexp_while (x1, x2)
    | Pexp_for (x1, x2, x3, x4, x5) -> Pexp_for (x1, x2, x3, x4, x5)
    | Pexp_constraint (x1, x2) -> Pexp_constraint (x1, x2)
    | Pexp_coerce (x1, x2, x3) -> Pexp_coerce (x1, x2, x3)
    | Pexp_send (x1, x2) -> Pexp_send (x1, x2)
    | Pexp_new x -> Pexp_new x
    | Pexp_setinstvar (x1, x2) -> Pexp_setinstvar (x1, x2)
    | Pexp_override x -> Pexp_override x
    | Pexp_letmodule (x1, x2, x3) -> Pexp_letmodule (x1, x2, x3)
    | Pexp_letexception (x1, x2) -> Pexp_letexception (x1, x2)
    | Pexp_assert x -> Pexp_assert x
    | Pexp_lazy x -> Pexp_lazy x
    | Pexp_poly (x1, x2) -> Pexp_poly (x1, x2)
    | Pexp_object x -> Pexp_object x
    | Pexp_newtype (x1, x2) -> Pexp_newtype (x1, x2)
    | Pexp_pack x -> Pexp_pack x
    | Pexp_open (x1, x2) -> Pexp_open (x1, x2)
    | Pexp_letop x -> Pexp_letop x
    | Pexp_extension x -> Pexp_extension x
    | Pexp_unreachable -> Pexp_unreachable
  ;;

  let of_parsetree (expr_desc : expression_desc) ~loc : t =
    match Pexp_function.of_parsetree expr_desc ~loc with
    | Some (x1, x2, x3) -> Pexp_function (x1, x2, x3)
    | None ->
      (match expr_desc with
       | Pexp_function _ | Pexp_fun _ ->
         (* matched by above call to [of_parsetree] *)
         assert false
       | Pexp_ident x -> Pexp_ident x
       | Pexp_constant x -> Pexp_constant x
       | Pexp_let (x1, x2, x3) -> Pexp_let (x1, x2, x3)
       | Pexp_apply (x1, x2) -> Pexp_apply (x1, x2)
       | Pexp_match (x1, x2) -> Pexp_match (x1, x2)
       | Pexp_try (x1, x2) -> Pexp_try (x1, x2)
       | Pexp_tuple x -> Pexp_tuple x
       | Pexp_construct (x1, x2) -> Pexp_construct (x1, x2)
       | Pexp_variant (x1, x2) -> Pexp_variant (x1, x2)
       | Pexp_record (x1, x2) -> Pexp_record (x1, x2)
       | Pexp_field (x1, x2) -> Pexp_field (x1, x2)
       | Pexp_setfield (x1, x2, x3) -> Pexp_setfield (x1, x2, x3)
       | Pexp_array x -> Pexp_array x
       | Pexp_ifthenelse (x1, x2, x3) -> Pexp_ifthenelse (x1, x2, x3)
       | Pexp_sequence (x1, x2) -> Pexp_sequence (x1, x2)
       | Pexp_while (x1, x2) -> Pexp_while (x1, x2)
       | Pexp_for (x1, x2, x3, x4, x5) -> Pexp_for (x1, x2, x3, x4, x5)
       | Pexp_constraint (x1, x2) -> Pexp_constraint (x1, x2)
       | Pexp_coerce (x1, x2, x3) -> Pexp_coerce (x1, x2, x3)
       | Pexp_send (x1, x2) -> Pexp_send (x1, x2)
       | Pexp_new x -> Pexp_new x
       | Pexp_setinstvar (x1, x2) -> Pexp_setinstvar (x1, x2)
       | Pexp_override x -> Pexp_override x
       | Pexp_letmodule (x1, x2, x3) -> Pexp_letmodule (x1, x2, x3)
       | Pexp_letexception (x1, x2) -> Pexp_letexception (x1, x2)
       | Pexp_assert x -> Pexp_assert x
       | Pexp_lazy x -> Pexp_lazy x
       | Pexp_poly (x1, x2) -> Pexp_poly (x1, x2)
       | Pexp_object x -> Pexp_object x
       | Pexp_newtype (x1, x2) -> Pexp_newtype (x1, x2)
       | Pexp_pack x -> Pexp_pack x
       | Pexp_open (x1, x2) -> Pexp_open (x1, x2)
       | Pexp_letop x -> Pexp_letop x
       | Pexp_extension x -> Pexp_extension x
       | Pexp_unreachable -> Pexp_unreachable)
  ;;
end
