open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
module Ast_helper = Ppxlib_ast.Ast_helper

module Modality = struct
  type nonrec t = Modality of string [@@unboxed]
end

module Modalities = struct
  type t = Modality.t loc list
end

module Mode = struct
  type t = Mode of string [@@unboxed]
end

module Modes = struct
  type t = Mode.t loc list

  let local = [ { txt = Mode.Mode "local"; loc = Location.none } ]
  let none = []
end

module Include_kind = struct
  type t =
    | Structure
    | Functor
end

type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : Modes.t
  ; arg_type : core_type
  }

type arrow_result =
  { result_modes : Modes.t
  ; result_type : core_type
  }

module Pcstr_tuple_arg = struct
  type t = core_type

  let extract_modalities t = [], t
  let to_core_type t = t
  let of_core_type core_type = core_type
  let map_core_type t ~f = f t
  let map_core_type_extra t ~f = f t
  let create ~loc:_ ~modalities:_ ~type_ = type_
end

module Label_declaration = struct
  let extract_modalities ld = [], ld

  let create ~loc ~name ~mutable_ ~modalities:_ ~type_ =
    { pld_loc = loc
    ; pld_name = name
    ; pld_type = type_
    ; pld_mutable = mutable_
    ; pld_attributes = []
    }
  ;;
end

module Value_description = struct
  let extract_modalities vd = [], vd

  let create ~loc ~name ~type_ ~modalities:_ ~prim =
    { pval_loc = loc
    ; pval_name = name
    ; pval_type = type_
    ; pval_prim = prim
    ; pval_attributes = []
    }
  ;;
end

module Value_binding = struct
  let extract_modes vb = [], vb

  let create ~loc ~pat ~expr ~modes:_ =
    { pvb_pat = pat; pvb_expr = expr; pvb_attributes = []; pvb_loc = loc }
  ;;
end

type jkind_const_annotation = string Location.loc

type jkind_annotation =
  | Default
  | Abbreviation of jkind_const_annotation
  | Mod of jkind_annotation * Modes.t
  | With of jkind_annotation * core_type
  | Kind_of of core_type
  | Product of jkind_annotation list

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
    { mode_annotations : Modes.t
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
          | Some type_constraint -> Some { mode_annotations = []; type_constraint }
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

let ptyp_any =
  { ptyp_desc = Ptyp_any
  ; ptyp_loc = Location.none
  ; ptyp_loc_stack = []
  ; ptyp_attributes = []
  }
;;

module Core_type_desc = struct
  type t =
    | Ptyp_any
    | Ptyp_var of string
    | Ptyp_arrow of arg_label * core_type * core_type * Modes.t * Modes.t
    | Ptyp_tuple of core_type list
    | Ptyp_unboxed_tuple of (string option * core_type) list
    | Ptyp_constr of Longident.t loc * core_type list
    | Ptyp_object of object_field list * closed_flag
    | Ptyp_class of Longident.t loc * core_type list
    | Ptyp_alias of core_type * string
    | Ptyp_variant of row_field list * closed_flag * label list option
    | Ptyp_poly of string loc list * core_type
    | Ptyp_package of package_type
    | Ptyp_extension of extension

  let of_parsetree : core_type_desc -> t = function
    (* changed constructors *)
    | Ptyp_arrow (a, b, c) -> Ptyp_arrow (a, b, c, [], [])
    (* unchanged constructors *)
    | Ptyp_any -> Ptyp_any
    | Ptyp_var s -> Ptyp_var s
    | Ptyp_tuple a -> Ptyp_tuple a
    | Ptyp_constr (a, b) -> Ptyp_constr (a, b)
    | Ptyp_object (a, b) -> Ptyp_object (a, b)
    | Ptyp_class (a, b) -> Ptyp_class (a, b)
    | Ptyp_alias (a, b) -> Ptyp_alias (a, b)
    | Ptyp_variant (a, b, c) -> Ptyp_variant (a, b, c)
    | Ptyp_poly (a, b) -> Ptyp_poly (a, b)
    | Ptyp_package a -> Ptyp_package a
    | Ptyp_extension a -> Ptyp_extension a
  ;;

  let to_parsetree : t -> core_type_desc = function
    (* changed constructors *)
    | Ptyp_arrow (a, b, c, _, _) -> Ptyp_arrow (a, b, c)
    (* new constructors *)
    | Ptyp_unboxed_tuple typs ->
      let typs =
        List.map
          (fun (lbl, typ) ->
            if Option.is_some lbl
            then
              failwith
                "[Ptyp_unboxed_tuple], when labels are present, is not a legal \
                 [core_type_desc] in your parsetree";
            typ)
          typs
      in
      Ptyp_tuple typs
    (* unchanged constructors *)
    | Ptyp_any -> Ptyp_any
    | Ptyp_var s -> Ptyp_var s
    | Ptyp_tuple a -> Ptyp_tuple a
    | Ptyp_constr (a, b) -> Ptyp_constr (a, b)
    | Ptyp_object (a, b) -> Ptyp_object (a, b)
    | Ptyp_class (a, b) -> Ptyp_class (a, b)
    | Ptyp_alias (a, b) -> Ptyp_alias (a, b)
    | Ptyp_variant (a, b, c) -> Ptyp_variant (a, b, c)
    | Ptyp_poly (a, b) -> Ptyp_poly (a, b)
    | Ptyp_package a -> Ptyp_package a
    | Ptyp_extension a -> Ptyp_extension a
  ;;
end

module Core_type = struct
  type t =
    { ptyp_desc : Core_type_desc.t
    ; ptyp_loc : Location.t
    ; ptyp_loc_stack : Location.t list
    ; ptyp_attributes : attributes
    }

  let of_parsetree
    { Ppxlib_ast.Parsetree.ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
    =
    let ptyp_desc = Core_type_desc.of_parsetree ptyp_desc in
    { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
  ;;

  let to_parsetree { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } =
    let ptyp_desc = Core_type_desc.to_parsetree ptyp_desc in
    { Ppxlib_ast.Parsetree.ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
  ;;
end

module Pattern_desc = struct
  type t =
    | Ppat_any
    | Ppat_var of string loc
    | Ppat_alias of pattern * string loc
    | Ppat_constant of constant
    | Ppat_interval of constant * constant
    | Ppat_tuple of pattern list
    | Ppat_unboxed_tuple of (string option * pattern) list * closed_flag
    | Ppat_construct of Longident.t loc * (string loc list * pattern) option
    | Ppat_variant of label * pattern option
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
    | Ppat_array of pattern list
    | Ppat_or of pattern * pattern
    | Ppat_constraint of pattern * core_type option * Modes.t
    | Ppat_type of Longident.t loc
    | Ppat_lazy of pattern
    | Ppat_unpack of string option loc
    | Ppat_exception of pattern
    | Ppat_extension of extension
    | Ppat_open of Longident.t loc * pattern

  let of_parsetree : pattern_desc -> t = function
    (* changed constructors *)
    | Ppat_constraint (a, b) -> Ppat_constraint (a, Some b, [])
    (* unchanged constructors *)
    | Ppat_any -> Ppat_any
    | Ppat_var a -> Ppat_var a
    | Ppat_alias (a, b) -> Ppat_alias (a, b)
    | Ppat_constant a -> Ppat_constant a
    | Ppat_interval (a, b) -> Ppat_interval (a, b)
    | Ppat_tuple a -> Ppat_tuple a
    | Ppat_construct (a, b) -> Ppat_construct (a, b)
    | Ppat_variant (a, b) -> Ppat_variant (a, b)
    | Ppat_record (a, b) -> Ppat_record (a, b)
    | Ppat_array a -> Ppat_array a
    | Ppat_or (a, b) -> Ppat_or (a, b)
    | Ppat_type a -> Ppat_type a
    | Ppat_lazy a -> Ppat_lazy a
    | Ppat_unpack a -> Ppat_unpack a
    | Ppat_exception a -> Ppat_exception a
    | Ppat_extension a -> Ppat_extension a
    | Ppat_open (a, b) -> Ppat_open (a, b)
  ;;

  let to_parsetree : t -> pattern_desc = function
    (* changed constructors *)
    | Ppat_constraint (a, Some b, _) -> Ppat_constraint (a, b)
    | Ppat_constraint (a, None, _) -> Ppat_constraint (a, ptyp_any)
    (* new constructors *)
    | Ppat_unboxed_tuple (_, Open) ->
      failwith
        "[Ppat_unboxed_tuple] with an \"open\" pattern is not a legal [pattern_desc] in \
         your parsetree"
    | Ppat_unboxed_tuple (pats, Closed) ->
      let pats =
        List.map
          (fun (lbl, pat) ->
            if Option.is_some lbl
            then
              failwith
                "[Ppat_unboxed_tuple], when labels are present, is not a legal \
                 [pattern_desc] in your parsetree";
            pat)
          pats
      in
      Ppat_tuple pats
    (* unchanged constructors *)
    | Ppat_any -> Ppat_any
    | Ppat_var a -> Ppat_var a
    | Ppat_alias (a, b) -> Ppat_alias (a, b)
    | Ppat_constant a -> Ppat_constant a
    | Ppat_interval (a, b) -> Ppat_interval (a, b)
    | Ppat_tuple a -> Ppat_tuple a
    | Ppat_construct (a, b) -> Ppat_construct (a, b)
    | Ppat_variant (a, b) -> Ppat_variant (a, b)
    | Ppat_record (a, b) -> Ppat_record (a, b)
    | Ppat_array a -> Ppat_array a
    | Ppat_or (a, b) -> Ppat_or (a, b)
    | Ppat_type a -> Ppat_type a
    | Ppat_lazy a -> Ppat_lazy a
    | Ppat_unpack a -> Ppat_unpack a
    | Ppat_exception a -> Ppat_exception a
    | Ppat_extension a -> Ppat_extension a
    | Ppat_open (a, b) -> Ppat_open (a, b)
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
    | Pexp_unboxed_tuple of (string option * expression) list
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
    | Pexp_constraint of expression * core_type option * Modes.t
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
    | Pexp_stack of expression

  let to_parsetree : t -> expression_desc = function
    (* changed constructors *)
    | Pexp_function (x1, x2, x3) ->
      Pexp_function.to_parsetree ~params:x1 ~constraint_:x2 ~body:x3
    | Pexp_constraint (x1, Some x2, _) -> Pexp_constraint (x1, x2)
    | Pexp_constraint (x1, None, _) -> Pexp_constraint (x1, ptyp_any)
    (* new constructors *)
    | Pexp_unboxed_tuple exps ->
      let exps =
        List.map
          (fun (lbl, exp) ->
            if Option.is_some lbl
            then
              failwith
                "[Ppat_unboxed_tuple], when labels are present, is not a legal \
                 [pattern_desc] in your parsetree";
            exp)
          exps
      in
      Pexp_tuple exps
    (* unchanged constructors *)
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
    | Pexp_stack _ ->
      failwith "[Pexp_stack] is not a legal [expression_desc] in your parsetree"
  ;;

  let of_parsetree (expr_desc : expression_desc) ~loc : t =
    (* changed constructors *)
    match Pexp_function.of_parsetree expr_desc ~loc with
    | Some (x1, x2, x3) -> Pexp_function (x1, x2, x3)
    | None ->
      (match expr_desc with
       | Pexp_function _ | Pexp_fun _ ->
         (* matched by above call to [of_parsetree] *)
         assert false
       | Pexp_constraint (x1, x2) -> Pexp_constraint (x1, Some x2, [])
       (* unchanged constructors *)
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

module Include_infos = struct
  type 'a t =
    { pincl_kind : Include_kind.t
    ; pincl_mod : 'a
    ; pincl_loc : Location.t
    ; pincl_attributes : attributes
    }

  let of_parsetree x : 'a t =
    let ({ pincl_mod; pincl_loc; pincl_attributes } : 'a include_infos) = x in
    let pincl_kind : Include_kind.t = Structure in
    { pincl_kind; pincl_mod; pincl_loc; pincl_attributes }
  ;;

  let to_parsetree x : 'a include_infos =
    let ({ pincl_kind; pincl_mod; pincl_loc; pincl_attributes } : 'a t) = x in
    match pincl_kind with
    | Structure -> { pincl_mod; pincl_loc; pincl_attributes }
    | Functor -> failwith "[include functor] is not legal in your parsetree"
  ;;
end

module Signature_item_desc = struct
  type t =
    | Psig_value of value_description
    (** - [val x: T]
            - [external x: T = "s1" ... "sn"]
         *)
    | Psig_type of rec_flag * type_declaration list
    (** [type t1 = ... and ... and tn  = ...] *)
    | Psig_typesubst of type_declaration list
    (** [type t1 := ... and ... and tn := ...]  *)
    | Psig_typext of type_extension (** [type t1 += ...] *)
    | Psig_exception of type_exception (** [exception C of T] *)
    | Psig_module of module_declaration (** [module X = M] and [module X : MT] *)
    | Psig_modsubst of module_substitution (** [module X := M] *)
    | Psig_recmodule of module_declaration list
    (** [module rec X1 : MT1 and ... and Xn : MTn] *)
    | Psig_modtype of module_type_declaration
    (** [module type S = MT] and [module type S] *)
    | Psig_modtypesubst of module_type_declaration (** [module type S :=  ...]  *)
    | Psig_open of open_description (** [open X] *)
    | Psig_include of include_description * Modalities.t (** [include MT] *)
    | Psig_class of class_description list (** [class c1 : ... and ... and cn : ...] *)
    | Psig_class_type of class_type_declaration list
    (** [class type ct1 = ... and ... and ctn = ...] *)
    | Psig_attribute of attribute (** [[\@\@\@id]] *)
    | Psig_extension of extension * attributes (** [[%%id]] *)

  let of_parsetree (sig_desc : signature_item_desc) =
    match sig_desc with
    | Psig_value a -> Psig_value a
    | Psig_type (a, b) -> Psig_type (a, b)
    | Psig_typesubst a -> Psig_typesubst a
    | Psig_typext a -> Psig_typext a
    | Psig_exception a -> Psig_exception a
    | Psig_module a -> Psig_module a
    | Psig_modsubst a -> Psig_modsubst a
    | Psig_recmodule a -> Psig_recmodule a
    | Psig_modtype a -> Psig_modtype a
    | Psig_modtypesubst a -> Psig_modtypesubst a
    | Psig_open a -> Psig_open a
    | Psig_include a -> Psig_include (a, [])
    | Psig_class a -> Psig_class a
    | Psig_class_type a -> Psig_class_type a
    | Psig_attribute a -> Psig_attribute a
    | Psig_extension (a, b) -> Psig_extension (a, b)
  ;;

  let to_parsetree (t : t) : signature_item_desc =
    match t with
    | Psig_value a -> Psig_value a
    | Psig_type (a, b) -> Psig_type (a, b)
    | Psig_typesubst a -> Psig_typesubst a
    | Psig_typext a -> Psig_typext a
    | Psig_exception a -> Psig_exception a
    | Psig_module a -> Psig_module a
    | Psig_modsubst a -> Psig_modsubst a
    | Psig_recmodule a -> Psig_recmodule a
    | Psig_modtype a -> Psig_modtype a
    | Psig_modtypesubst a -> Psig_modtypesubst a
    | Psig_open a -> Psig_open a
    | Psig_include (a, b) ->
      if List.is_empty b
      then Psig_include a
      else
        failwith
          "[Psig_include] with modalities is not a legal [signature_item_desc] in your \
           parsetree"
    | Psig_class a -> Psig_class a
    | Psig_class_type a -> Psig_class_type a
    | Psig_attribute a -> Psig_attribute a
    | Psig_extension (a, b) -> Psig_extension (a, b)
  ;;
end
