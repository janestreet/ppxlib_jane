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

  let create ~loc ~constraint_ ~pat ~expr ~modes:_ =
    { pvb_pat = pat
    ; pvb_expr = expr
    ; pvb_attributes = []
    ; pvb_loc = loc
    ; pvb_constraint = constraint_
    }
  ;;
end

module T = struct
  type jkind_annotation_desc =
    | Default
    | Abbreviation of string
    | Mod of jkind_annotation * Modes.t
    | With of jkind_annotation * core_type
    | Kind_of of core_type
    | Product of jkind_annotation list

  and jkind_annotation =
    { pjkind_loc : Location.t
    ; pjkind_desc : jkind_annotation_desc
    }
end

include T

module Type_declaration = struct
  let extract_jkind_annotation _ = None
end

module Constant = struct
  type t =
    | Pconst_integer of string * char option
    | Pconst_unboxed_integer of string * char
    | Pconst_char of char
    | Pconst_string of string * Location.t * string option
    | Pconst_float of string * char option
    | Pconst_unboxed_float of string * char option

  let of_parsetree : constant -> t = function
    | Pconst_integer (a, b) -> Pconst_integer (a, b)
    | Pconst_char a -> Pconst_char a
    | Pconst_string (a, b, c) -> Pconst_string (a, b, c)
    | Pconst_float (a, b) -> Pconst_float (a, b)
  ;;

  let to_parsetree : t -> constant = function
    | Pconst_integer (a, b) -> Pconst_integer (a, b)
    | Pconst_char a -> Pconst_char a
    | Pconst_string (a, b, c) -> Pconst_string (a, b, c)
    | Pconst_float (a, b) -> Pconst_float (a, b)
    (* Unboxed literal constants erase to boxed literals. *)
    | Pconst_unboxed_integer (a, b) -> Pconst_integer (a, Some b)
    | Pconst_unboxed_float (a, b) -> Pconst_float (a, b)
  ;;
end

module Pexp_function = struct
  type jfunction_param_desc =
    | Pparam_val of arg_label * expression option * pattern
    | Pparam_newtype of string loc * jkind_annotation option

  type jfunction_param =
    { pparam_loc : Location.t
    ; pparam_desc : jfunction_param_desc
    }

  type function_constraint =
    { mode_annotations : Modes.t
    ; type_constraint : type_constraint
    }

  let to_parsetree ~params ~constraint_ ~body =
    let to_function_params : jfunction_param -> function_param =
      fun v ->
      match v.pparam_desc with
      | Pparam_newtype (ty, _) ->
        { pparam_desc = Pparam_newtype ty; pparam_loc = v.pparam_loc }
      | Pparam_val (lbl, e, p) ->
        { pparam_desc = Pparam_val (lbl, e, p); pparam_loc = v.pparam_loc }
    in
    let type_constraint = Option.map (fun v -> v.type_constraint) constraint_ in
    Pexp_function (List.map to_function_params params, type_constraint, body)
  ;;

  let of_parsetree =
    let to_jfunction_params : function_param -> jfunction_param =
      fun v ->
      match v.pparam_desc with
      | Pparam_newtype ty ->
        { pparam_desc = Pparam_newtype (ty, None); pparam_loc = v.pparam_loc }
      | Pparam_val (lbl, e, p) ->
        { pparam_desc = Pparam_val (lbl, e, p); pparam_loc = v.pparam_loc }
    in
    fun expr_desc ~loc ->
      match expr_desc with
      | Pexp_function (params, constraint_, body) ->
        let function_constraint =
          Option.map
            (fun type_constraint -> { type_constraint; mode_annotations = [] })
            constraint_
        in
        Some (List.map to_jfunction_params params, function_constraint, body)
      | _ -> None
  ;;
end

let ptyp_any =
  { ptyp_desc = Ptyp_any
  ; ptyp_loc = Location.none
  ; ptyp_loc_stack = []
  ; ptyp_attributes = []
  }
;;

let add_none_labels l = List.map (fun x -> None, x) l

(* Duplicated from [common.ml] to avoid dependency cycle *)
let as_unlabeled_tuple components =
  if List.for_all (fun (label, _) -> Option.is_none label) components
  then Some (List.map snd components)
  else None
;;

module Core_type_desc = struct
  type t =
    | Ptyp_any of jkind_annotation option
    | Ptyp_var of string * jkind_annotation option
    | Ptyp_arrow of arg_label * core_type * core_type * Modes.t * Modes.t
    | Ptyp_tuple of (string option * core_type) list
    | Ptyp_unboxed_tuple of (string option * core_type) list
    | Ptyp_constr of Longident.t loc * core_type list
    | Ptyp_object of object_field list * closed_flag
    | Ptyp_class of Longident.t loc * core_type list
    | Ptyp_alias of core_type * string loc * jkind_annotation option
    | Ptyp_variant of row_field list * closed_flag * label list option
    | Ptyp_poly of (string loc * jkind_annotation option) list * core_type
    | Ptyp_package of package_type
    | Ptyp_extension of extension
    | Ptyp_open of Longident.t loc * core_type

  let of_parsetree : core_type_desc -> t = function
    (* changed constructors *)
    | Ptyp_arrow (a, b, c) -> Ptyp_arrow (a, b, c, [], [])
    | Ptyp_tuple a -> Ptyp_tuple (add_none_labels a)
    | Ptyp_any -> Ptyp_any None
    | Ptyp_var s -> Ptyp_var (s, None)
    | Ptyp_alias (a, b) -> Ptyp_alias (a, b, None)
    | Ptyp_poly (a, b) -> Ptyp_poly (List.map (fun x -> x, None) a, b)
    (* unchanged constructors *)
    | Ptyp_constr (a, b) -> Ptyp_constr (a, b)
    | Ptyp_object (a, b) -> Ptyp_object (a, b)
    | Ptyp_class (a, b) -> Ptyp_class (a, b)
    | Ptyp_variant (a, b, c) -> Ptyp_variant (a, b, c)
    | Ptyp_package a -> Ptyp_package a
    | Ptyp_extension a -> Ptyp_extension a
    | Ptyp_open (a, b) -> Ptyp_open (a, b)
  ;;

  let to_parsetree : t -> core_type_desc = function
    (* changed constructors *)
    | Ptyp_arrow (a, b, c, _, _) -> Ptyp_arrow (a, b, c)
    | Ptyp_any (_ : jkind_annotation option) -> Ptyp_any
    | Ptyp_var (s, _) -> Ptyp_var s
    | Ptyp_poly (a, b) -> Ptyp_poly (List.map fst a, b)
    | Ptyp_alias (a, b, _) -> Ptyp_alias (a, b)
    | Ptyp_tuple labeled_typs ->
      (match as_unlabeled_tuple labeled_typs with
       | Some typs -> Ptyp_tuple typs
       | None ->
         failwith
           "[Ptyp_tuple], when labels are present, is not a legal [core_type_desc] in \
            your parsetree")
    (* new constructors *)
    | Ptyp_unboxed_tuple labeled_typs ->
      (match as_unlabeled_tuple labeled_typs with
       | Some typs -> Ptyp_tuple typs
       | None ->
         failwith
           "[Ptyp_unboxed_tuple], when labels are present, is not a legal \
            [core_type_desc] in your parsetree")
    (* unchanged constructors *)
    | Ptyp_constr (a, b) -> Ptyp_constr (a, b)
    | Ptyp_object (a, b) -> Ptyp_object (a, b)
    | Ptyp_class (a, b) -> Ptyp_class (a, b)
    | Ptyp_variant (a, b, c) -> Ptyp_variant (a, b, c)
    | Ptyp_package a -> Ptyp_package a
    | Ptyp_extension a -> Ptyp_extension a
    | Ptyp_open (a, b) -> Ptyp_open (a, b)
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
    | Ppat_tuple of (string option * pattern) list * closed_flag
    | Ppat_unboxed_tuple of (string option * pattern) list * closed_flag
    | Ppat_construct of Longident.t loc * (string loc list * pattern) option
    | Ppat_variant of label * pattern option
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
    | Ppat_array of mutable_flag * pattern list
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
    | Ppat_tuple a -> Ppat_tuple (add_none_labels a, Closed)
    (* unchanged constructors *)
    | Ppat_any -> Ppat_any
    | Ppat_var a -> Ppat_var a
    | Ppat_alias (a, b) -> Ppat_alias (a, b)
    | Ppat_constant a -> Ppat_constant a
    | Ppat_interval (a, b) -> Ppat_interval (a, b)
    | Ppat_construct (a, b) -> Ppat_construct (a, b)
    | Ppat_variant (a, b) -> Ppat_variant (a, b)
    | Ppat_record (a, b) -> Ppat_record (a, b)
    | Ppat_array a -> Ppat_array (Mutable, a)
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
    | Ppat_tuple (_, Open) ->
      failwith
        "[Ppat_tuple] with an \"open\" pattern is not a legal [pattern_desc] in your \
         parsetree"
    | Ppat_tuple (labeled_pats, Closed) ->
      (match as_unlabeled_tuple labeled_pats with
       | Some pats -> Ppat_tuple pats
       | None ->
         failwith
           "[Ppat_tuple], when labels are present, is not a legal [pattern_desc] in your \
            parsetree")
    | Ppat_array (Mutable, a) -> Ppat_array a
    | Ppat_array (Immutable, _) ->
      failwith "Immutable [Ppat_array] is not a legal [pattern_desc] in your parsetree"
    (* new constructors *)
    | Ppat_unboxed_tuple (_, Open) ->
      failwith
        "[Ppat_unboxed_tuple] with an \"open\" pattern is not a legal [pattern_desc] in \
         your parsetree"
    | Ppat_unboxed_tuple (labeled_pats, Closed) ->
      (match as_unlabeled_tuple labeled_pats with
       | Some pats -> Ppat_tuple pats
       | None ->
         failwith
           "[Ppat_unboxed_tuple], when labels are present, is not a legal [pattern_desc] \
            in your parsetree")
    (* unchanged constructors *)
    | Ppat_any -> Ppat_any
    | Ppat_var a -> Ppat_var a
    | Ppat_alias (a, b) -> Ppat_alias (a, b)
    | Ppat_constant a -> Ppat_constant a
    | Ppat_interval (a, b) -> Ppat_interval (a, b)
    | Ppat_construct (a, b) -> Ppat_construct (a, b)
    | Ppat_variant (a, b) -> Ppat_variant (a, b)
    | Ppat_record (a, b) -> Ppat_record (a, b)
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
  type comprehension_expression = private
    | Pcomp_list_comprehension of unit
    | Pcomp_array_comprehension of unit

  type t =
    | Pexp_ident of Longident.t loc
    | Pexp_constant of constant
    | Pexp_let of rec_flag * value_binding list * expression
    | Pexp_function of
        Pexp_function.jfunction_param list
        * Pexp_function.function_constraint option
        * function_body
    | Pexp_apply of expression * (arg_label * expression) list
    | Pexp_match of expression * case list
    | Pexp_try of expression * case list
    | Pexp_tuple of (string option * expression) list
    | Pexp_unboxed_tuple of (string option * expression) list
    | Pexp_construct of Longident.t loc * expression option
    | Pexp_variant of label * expression option
    | Pexp_record of (Longident.t loc * expression) list * expression option
    | Pexp_field of expression * Longident.t loc
    | Pexp_setfield of expression * Longident.t loc * expression
    | Pexp_array of mutable_flag * expression list
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
    | Pexp_newtype of string loc * jkind_annotation option * expression
    | Pexp_pack of module_expr
    | Pexp_open of open_declaration * expression
    | Pexp_letop of letop
    | Pexp_extension of extension
    | Pexp_unreachable
    | Pexp_stack of expression
    | Pexp_comprehension of comprehension_expression

  let to_parsetree : t -> expression_desc = function
    (* changed constructors *)
    | Pexp_function (x1, x2, x3) ->
      Pexp_function.to_parsetree ~params:x1 ~constraint_:x2 ~body:x3
    | Pexp_constraint (x1, Some x2, _) -> Pexp_constraint (x1, x2)
    | Pexp_constraint (x1, None, _) -> Pexp_constraint (x1, ptyp_any)
    | Pexp_tuple labeled_exps ->
      (match as_unlabeled_tuple labeled_exps with
       | Some exps -> Pexp_tuple exps
       | None ->
         failwith
           "[Pexp_tuple], when labels are present, is not a legal [expression_desc] in \
            your parsetree")
    | Pexp_newtype (x1, _, x2) -> Pexp_newtype (x1, x2)
    | Pexp_array (Mutable, x) -> Pexp_array x
    | Pexp_array (Immutable, _) ->
      failwith
        "Immutable [Pexp_array] is not a legal [expression_desc] in your parsetree."
    (* new constructors *)
    | Pexp_unboxed_tuple labeled_exps ->
      (match as_unlabeled_tuple labeled_exps with
       | Some exps -> Pexp_tuple exps
       | None ->
         failwith
           "[Pexp_unboxed_tuple], when labels are present, is not a legal \
            [expression_desc] in your parsetree")
    (* unchanged constructors *)
    | Pexp_ident x -> Pexp_ident x
    | Pexp_constant x -> Pexp_constant x
    | Pexp_let (x1, x2, x3) -> Pexp_let (x1, x2, x3)
    | Pexp_apply (x1, x2) -> Pexp_apply (x1, x2)
    | Pexp_match (x1, x2) -> Pexp_match (x1, x2)
    | Pexp_try (x1, x2) -> Pexp_try (x1, x2)
    | Pexp_construct (x1, x2) -> Pexp_construct (x1, x2)
    | Pexp_variant (x1, x2) -> Pexp_variant (x1, x2)
    | Pexp_record (x1, x2) -> Pexp_record (x1, x2)
    | Pexp_field (x1, x2) -> Pexp_field (x1, x2)
    | Pexp_setfield (x1, x2, x3) -> Pexp_setfield (x1, x2, x3)
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
    | Pexp_pack x -> Pexp_pack x
    | Pexp_open (x1, x2) -> Pexp_open (x1, x2)
    | Pexp_letop x -> Pexp_letop x
    | Pexp_extension x -> Pexp_extension x
    | Pexp_unreachable -> Pexp_unreachable
    | Pexp_stack _ ->
      failwith "[Pexp_stack] is not a legal [expression_desc] in your parsetree"
    | Pexp_comprehension _ ->
      failwith "[Pexp_comprehension] is not a legal [expression_desc] in your parsetree"
  ;;

  let of_parsetree (expr_desc : expression_desc) ~loc : t =
    (* changed constructors *)
    match Pexp_function.of_parsetree expr_desc ~loc with
    | Some (x1, x2, x3) -> Pexp_function (x1, x2, x3)
    | None ->
      (match expr_desc with
       | Pexp_function _ ->
         (* matched by above call to [of_parsetree] *)
         assert false
       | Pexp_constraint (x1, x2) -> Pexp_constraint (x1, Some x2, [])
       | Pexp_tuple x -> Pexp_tuple (add_none_labels x)
       | Pexp_newtype (x1, x2) -> Pexp_newtype (x1, None, x2)
       | Pexp_array x -> Pexp_array (Mutable, x)
       (* unchanged constructors *)
       | Pexp_ident x -> Pexp_ident x
       | Pexp_constant x -> Pexp_constant x
       | Pexp_let (x1, x2, x3) -> Pexp_let (x1, x2, x3)
       | Pexp_apply (x1, x2) -> Pexp_apply (x1, x2)
       | Pexp_match (x1, x2) -> Pexp_match (x1, x2)
       | Pexp_try (x1, x2) -> Pexp_try (x1, x2)
       | Pexp_construct (x1, x2) -> Pexp_construct (x1, x2)
       | Pexp_variant (x1, x2) -> Pexp_variant (x1, x2)
       | Pexp_record (x1, x2) -> Pexp_record (x1, x2)
       | Pexp_field (x1, x2) -> Pexp_field (x1, x2)
       | Pexp_setfield (x1, x2, x3) -> Pexp_setfield (x1, x2, x3)
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
    | Psig_type of rec_flag * type_declaration list
    | Psig_typesubst of type_declaration list
    | Psig_typext of type_extension
    | Psig_exception of type_exception
    | Psig_module of module_declaration
    | Psig_modsubst of module_substitution
    | Psig_recmodule of module_declaration list
    | Psig_modtype of module_type_declaration
    | Psig_modtypesubst of module_type_declaration
    | Psig_open of open_description
    | Psig_include of include_description * Modalities.t
    | Psig_class of class_description list
    | Psig_class_type of class_type_declaration list
    | Psig_attribute of attribute
    | Psig_extension of extension * attributes
    | Psig_kind_abbrev of string loc * jkind_annotation

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
    | Psig_kind_abbrev _ ->
      (* erase to [include sig end] *)
      Psig_include
        { pincl_loc = Location.none
        ; pincl_attributes = []
        ; pincl_mod =
            { pmty_desc = Pmty_signature []
            ; pmty_loc = Location.none
            ; pmty_attributes = []
            }
        }
  ;;
end

module Signature = struct
  type t = { psg_items : signature_item list }

  let of_parsetree psg_items = { psg_items }
  let to_parsetree { psg_items } = psg_items
end

module Structure_item_desc = struct
  type t =
    | Pstr_eval of expression * attributes
    | Pstr_value of rec_flag * value_binding list
    | Pstr_primitive of value_description
    | Pstr_type of rec_flag * type_declaration list
    | Pstr_typext of type_extension
    | Pstr_exception of type_exception
    | Pstr_module of module_binding
    | Pstr_recmodule of module_binding list
    | Pstr_modtype of module_type_declaration
    | Pstr_open of open_declaration
    | Pstr_class of class_declaration list
    | Pstr_class_type of class_type_declaration list
    | Pstr_include of include_declaration
    | Pstr_attribute of attribute
    | Pstr_extension of extension * attributes
    | Pstr_kind_abbrev of string loc * jkind_annotation

  let of_parsetree : structure_item_desc -> t = function
    | Pstr_eval (a, b) -> Pstr_eval (a, b)
    | Pstr_value (a, b) -> Pstr_value (a, b)
    | Pstr_primitive a -> Pstr_primitive a
    | Pstr_type (a, b) -> Pstr_type (a, b)
    | Pstr_typext a -> Pstr_typext a
    | Pstr_exception a -> Pstr_exception a
    | Pstr_module a -> Pstr_module a
    | Pstr_recmodule a -> Pstr_recmodule a
    | Pstr_modtype a -> Pstr_modtype a
    | Pstr_open a -> Pstr_open a
    | Pstr_class a -> Pstr_class a
    | Pstr_class_type a -> Pstr_class_type a
    | Pstr_include a -> Pstr_include a
    | Pstr_attribute a -> Pstr_attribute a
    | Pstr_extension (a, b) -> Pstr_extension (a, b)
  ;;

  let to_parsetree : t -> structure_item_desc = function
    | Pstr_eval (a, b) -> Pstr_eval (a, b)
    | Pstr_value (a, b) -> Pstr_value (a, b)
    | Pstr_primitive a -> Pstr_primitive a
    | Pstr_type (a, b) -> Pstr_type (a, b)
    | Pstr_typext a -> Pstr_typext a
    | Pstr_exception a -> Pstr_exception a
    | Pstr_module a -> Pstr_module a
    | Pstr_recmodule a -> Pstr_recmodule a
    | Pstr_modtype a -> Pstr_modtype a
    | Pstr_open a -> Pstr_open a
    | Pstr_class a -> Pstr_class a
    | Pstr_class_type a -> Pstr_class_type a
    | Pstr_include a -> Pstr_include a
    | Pstr_attribute a -> Pstr_attribute a
    | Pstr_extension (a, b) -> Pstr_extension (a, b)
    | Pstr_kind_abbrev _ ->
      (* erase to [include struct end] *)
      Pstr_include
        { pincl_loc = Location.none
        ; pincl_attributes = []
        ; pincl_mod =
            { pmod_desc = Pmod_structure []
            ; pmod_loc = Location.none
            ; pmod_attributes = []
            }
        }
  ;;
end

module Module_type_desc = struct
  type t =
    | Pmty_ident of Longident.t loc
    | Pmty_signature of signature
    | Pmty_functor of functor_parameter * module_type
    | Pmty_with of module_type * with_constraint list
    | Pmty_typeof of module_expr
    | Pmty_extension of extension
    | Pmty_alias of Longident.t loc
    | Pmty_strengthen of module_type * Longident.t loc

  let of_parsetree : module_type_desc -> t = function
    | Pmty_ident x -> Pmty_ident x
    | Pmty_signature x -> Pmty_signature x
    | Pmty_functor (x0, x1) -> Pmty_functor (x0, x1)
    | Pmty_with (x0, x1) -> Pmty_with (x0, x1)
    | Pmty_typeof x -> Pmty_typeof x
    | Pmty_extension x -> Pmty_extension x
    | Pmty_alias x -> Pmty_alias x
  ;;

  let to_parsetree : t -> module_type_desc = function
    (* new constructors *)
    | Pmty_strengthen _ ->
      failwith "[Pmty_strengthen] is not a legal [module_type_desc] in your parsetree"
    (* unchanged constructors *)
    | Pmty_ident x -> Pmty_ident x
    | Pmty_signature x -> Pmty_signature x
    | Pmty_functor (x0, x1) -> Pmty_functor (x0, x1)
    | Pmty_with (x0, x1) -> Pmty_with (x0, x1)
    | Pmty_typeof x -> Pmty_typeof x
    | Pmty_extension x -> Pmty_extension x
    | Pmty_alias x -> Pmty_alias x
  ;;
end

module Module_expr_desc = struct
  type module_instance = private Module_instance

  type t =
    | Pmod_ident of Longident.t loc
    | Pmod_structure of structure
    | Pmod_functor of functor_parameter * module_expr
    | Pmod_apply of module_expr * module_expr
    | Pmod_constraint of module_expr * module_type
    | Pmod_unpack of expression
    | Pmod_extension of extension
    | Pmod_instance of module_instance
    | Pmod_apply_unit of module_expr

  let of_parsetree : module_expr_desc -> t = function
    | Pmod_ident x -> Pmod_ident x
    | Pmod_structure x -> Pmod_structure x
    | Pmod_functor (x0, x1) -> Pmod_functor (x0, x1)
    | Pmod_apply (x0, x1) -> Pmod_apply (x0, x1)
    | Pmod_constraint (x0, x1) -> Pmod_constraint (x0, x1)
    | Pmod_unpack x -> Pmod_unpack x
    | Pmod_extension x -> Pmod_extension x
    | Pmod_apply_unit x -> Pmod_apply_unit x
  ;;

  let to_parsetree : t -> module_expr_desc = function
    (* new constructors *)
    | Pmod_instance _ ->
      failwith "[Pmod_instance] is not a legal [module_expr_desc] in your parsetree"
    (* unchanged constructors *)
    | Pmod_ident x -> Pmod_ident x
    | Pmod_structure x -> Pmod_structure x
    | Pmod_functor (x0, x1) -> Pmod_functor (x0, x1)
    | Pmod_apply (x0, x1) -> Pmod_apply (x0, x1)
    | Pmod_constraint (x0, x1) -> Pmod_constraint (x0, x1)
    | Pmod_unpack x -> Pmod_unpack x
    | Pmod_extension x -> Pmod_extension x
    | Pmod_apply_unit x -> Pmod_apply_unit x
  ;;
end

module Ast_traverse = struct
  module Deriving_inline = struct
    type location = Location.t

    type jkind_annotation_desc = T.jkind_annotation_desc =
      | Default
      | Abbreviation of string
      | Mod of jkind_annotation * modes
      | With of jkind_annotation * core_type
      | Kind_of of core_type
      | Product of jkind_annotation list

    and jkind_annotation = T.jkind_annotation =
      { pjkind_loc : location
      ; pjkind_desc : jkind_annotation_desc
      }

    and function_param_desc = Pexp_function.jfunction_param_desc =
      | Pparam_val of arg_label * expression option * pattern
      | Pparam_newtype of string loc * jkind_annotation option

    and function_param = Pexp_function.jfunction_param =
      { pparam_loc : location
      ; pparam_desc : function_param_desc
      }

    and function_constraint = Pexp_function.function_constraint =
      { mode_annotations : Modes.t
      ; type_constraint : type_constraint
      }

    and mode = Mode.t = Mode of string [@@unboxed]
    and modes = mode loc list
    and signature_items = signature_item list
    and signature = signature_items [@@deriving_inline traverse]

    class virtual map =
      object (self)
        method virtual arg_label : arg_label -> arg_label
        method virtual attributes : attributes -> attributes
        method virtual case : case -> case
        method virtual core_type : core_type -> core_type
        method virtual expression : expression -> expression
        method virtual list : 'a. ('a -> 'a) -> 'a list -> 'a list
        method virtual loc : 'a. ('a -> 'a) -> 'a loc -> 'a loc
        method virtual location : location -> location
        method virtual option : 'a. ('a -> 'a) -> 'a option -> 'a option
        method virtual pattern : pattern -> pattern
        method virtual signature_item : signature_item -> signature_item
        method virtual string : string -> string
        method virtual type_constraint : type_constraint -> type_constraint

        method jkind_annotation_desc : jkind_annotation_desc -> jkind_annotation_desc =
          fun x ->
            match x with
            | Default -> Default
            | Abbreviation a ->
              let a = self#string a in
              Abbreviation a
            | Mod (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#modes b in
              Mod (a, b)
            | With (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#core_type b in
              With (a, b)
            | Kind_of a ->
              let a = self#core_type a in
              Kind_of a
            | Product a ->
              let a = self#list self#jkind_annotation a in
              Product a

        method jkind_annotation : jkind_annotation -> jkind_annotation =
          fun { pjkind_loc; pjkind_desc } ->
            let pjkind_loc = self#location pjkind_loc in
            let pjkind_desc = self#jkind_annotation_desc pjkind_desc in
            { pjkind_loc; pjkind_desc }

        method jfunction_param_desc : function_param_desc -> function_param_desc =
          fun x ->
            match x with
            | Pparam_val (a, b, c) ->
              let a = self#arg_label a in
              let b = self#option self#expression b in
              let c = self#pattern c in
              Pparam_val (a, b, c)
            | Pparam_newtype (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              Pparam_newtype (a, b)

        method jfunction_param : function_param -> function_param =
          fun { pparam_loc; pparam_desc } ->
            let pparam_loc = self#location pparam_loc in
            let pparam_desc = self#jfunction_param_desc pparam_desc in
            { pparam_loc; pparam_desc }

        method function_constraint : function_constraint -> function_constraint =
          fun { mode_annotations; type_constraint } ->
            let mode_annotations = self#modes mode_annotations in
            let type_constraint = self#type_constraint type_constraint in
            { mode_annotations; type_constraint }

        method mode : mode -> mode =
          fun x ->
            match x with
            | Mode a ->
              let a = self#string a in
              Mode a

        method modes : modes -> modes = self#list (self#loc self#mode)

        method signature_items : signature_items -> signature_items =
          self#list self#signature_item

        method signature : signature -> signature = self#signature_items
      end

    class virtual iter =
      object (self)
        method virtual arg_label : arg_label -> unit
        method virtual attributes : attributes -> unit
        method virtual case : case -> unit
        method virtual core_type : core_type -> unit
        method virtual expression : expression -> unit
        method virtual list : 'a. ('a -> unit) -> 'a list -> unit
        method virtual loc : 'a. ('a -> unit) -> 'a loc -> unit
        method virtual location : location -> unit
        method virtual option : 'a. ('a -> unit) -> 'a option -> unit
        method virtual pattern : pattern -> unit
        method virtual signature_item : signature_item -> unit
        method virtual string : string -> unit
        method virtual type_constraint : type_constraint -> unit

        method jkind_annotation_desc : jkind_annotation_desc -> unit =
          fun x ->
            match x with
            | Default -> ()
            | Abbreviation a -> self#string a
            | Mod (a, b) ->
              self#jkind_annotation a;
              self#modes b
            | With (a, b) ->
              self#jkind_annotation a;
              self#core_type b
            | Kind_of a -> self#core_type a
            | Product a -> self#list self#jkind_annotation a

        method jkind_annotation : jkind_annotation -> unit =
          fun { pjkind_loc; pjkind_desc } ->
            self#location pjkind_loc;
            self#jkind_annotation_desc pjkind_desc

        method jfunction_param_desc : function_param_desc -> unit =
          fun x ->
            match x with
            | Pparam_val (a, b, c) ->
              self#arg_label a;
              self#option self#expression b;
              self#pattern c
            | Pparam_newtype (a, b) ->
              self#loc self#string a;
              self#option self#jkind_annotation b

        method jfunction_param : function_param -> unit =
          fun { pparam_loc; pparam_desc } ->
            self#location pparam_loc;
            self#jfunction_param_desc pparam_desc

        method function_constraint : function_constraint -> unit =
          fun { mode_annotations; type_constraint } ->
            self#modes mode_annotations;
            self#type_constraint type_constraint

        method mode : mode -> unit =
          fun x ->
            match x with
            | Mode a -> self#string a

        method modes : modes -> unit = self#list (self#loc self#mode)
        method signature_items : signature_items -> unit = self#list self#signature_item
        method signature : signature -> unit = self#signature_items
      end

    class virtual ['acc] fold =
      object (self)
        method virtual arg_label : arg_label -> 'acc -> 'acc
        method virtual attributes : attributes -> 'acc -> 'acc
        method virtual case : case -> 'acc -> 'acc
        method virtual core_type : core_type -> 'acc -> 'acc
        method virtual expression : expression -> 'acc -> 'acc
        method virtual list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
        method virtual loc : 'a. ('a -> 'acc -> 'acc) -> 'a loc -> 'acc -> 'acc
        method virtual location : location -> 'acc -> 'acc
        method virtual option : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
        method virtual pattern : pattern -> 'acc -> 'acc
        method virtual signature_item : signature_item -> 'acc -> 'acc
        method virtual string : string -> 'acc -> 'acc
        method virtual type_constraint : type_constraint -> 'acc -> 'acc

        method jkind_annotation_desc : jkind_annotation_desc -> 'acc -> 'acc =
          fun x acc ->
            match x with
            | Default -> acc
            | Abbreviation a -> self#string a acc
            | Mod (a, b) ->
              let acc = self#jkind_annotation a acc in
              let acc = self#modes b acc in
              acc
            | With (a, b) ->
              let acc = self#jkind_annotation a acc in
              let acc = self#core_type b acc in
              acc
            | Kind_of a -> self#core_type a acc
            | Product a -> self#list self#jkind_annotation a acc

        method jkind_annotation : jkind_annotation -> 'acc -> 'acc =
          fun { pjkind_loc; pjkind_desc } acc ->
            let acc = self#location pjkind_loc acc in
            let acc = self#jkind_annotation_desc pjkind_desc acc in
            acc

        method jfunction_param_desc : function_param_desc -> 'acc -> 'acc =
          fun x acc ->
            match x with
            | Pparam_val (a, b, c) ->
              let acc = self#arg_label a acc in
              let acc = self#option self#expression b acc in
              let acc = self#pattern c acc in
              acc
            | Pparam_newtype (a, b) ->
              let acc = self#loc self#string a acc in
              let acc = self#option self#jkind_annotation b acc in
              acc

        method jfunction_param : function_param -> 'acc -> 'acc =
          fun { pparam_loc; pparam_desc } acc ->
            let acc = self#location pparam_loc acc in
            let acc = self#jfunction_param_desc pparam_desc acc in
            acc

        method function_constraint : function_constraint -> 'acc -> 'acc =
          fun { mode_annotations; type_constraint } acc ->
            let acc = self#modes mode_annotations acc in
            let acc = self#type_constraint type_constraint acc in
            acc

        method mode : mode -> 'acc -> 'acc =
          fun x acc ->
            match x with
            | Mode a -> self#string a acc

        method modes : modes -> 'acc -> 'acc = self#list (self#loc self#mode)

        method signature_items : signature_items -> 'acc -> 'acc =
          self#list self#signature_item

        method signature : signature -> 'acc -> 'acc = self#signature_items
      end

    class virtual ['acc] fold_map =
      object (self)
        method virtual arg_label : arg_label -> 'acc -> arg_label * 'acc
        method virtual attributes : attributes -> 'acc -> attributes * 'acc
        method virtual case : case -> 'acc -> case * 'acc
        method virtual core_type : core_type -> 'acc -> core_type * 'acc
        method virtual expression : expression -> 'acc -> expression * 'acc

        method
          virtual list
          : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc

        method
          virtual loc
          : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a loc -> 'acc -> 'a loc * 'acc

        method virtual location : location -> 'acc -> location * 'acc

        method
          virtual option
          : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc

        method virtual pattern : pattern -> 'acc -> pattern * 'acc
        method virtual signature_item : signature_item -> 'acc -> signature_item * 'acc
        method virtual string : string -> 'acc -> string * 'acc
        method virtual type_constraint : type_constraint -> 'acc -> type_constraint * 'acc

        method jkind_annotation_desc
          : jkind_annotation_desc -> 'acc -> jkind_annotation_desc * 'acc =
          fun x acc ->
            match x with
            | Default -> Default, acc
            | Abbreviation a ->
              let a, acc = self#string a acc in
              Abbreviation a, acc
            | Mod (a, b) ->
              let a, acc = self#jkind_annotation a acc in
              let b, acc = self#modes b acc in
              Mod (a, b), acc
            | With (a, b) ->
              let a, acc = self#jkind_annotation a acc in
              let b, acc = self#core_type b acc in
              With (a, b), acc
            | Kind_of a ->
              let a, acc = self#core_type a acc in
              Kind_of a, acc
            | Product a ->
              let a, acc = self#list self#jkind_annotation a acc in
              Product a, acc

        method jkind_annotation : jkind_annotation -> 'acc -> jkind_annotation * 'acc =
          fun { pjkind_loc; pjkind_desc } acc ->
            let pjkind_loc, acc = self#location pjkind_loc acc in
            let pjkind_desc, acc = self#jkind_annotation_desc pjkind_desc acc in
            { pjkind_loc; pjkind_desc }, acc

        method jfunction_param_desc
          : function_param_desc -> 'acc -> function_param_desc * 'acc =
          fun x acc ->
            match x with
            | Pparam_val (a, b, c) ->
              let a, acc = self#arg_label a acc in
              let b, acc = self#option self#expression b acc in
              let c, acc = self#pattern c acc in
              Pparam_val (a, b, c), acc
            | Pparam_newtype (a, b) ->
              let a, acc = self#loc self#string a acc in
              let b, acc = self#option self#jkind_annotation b acc in
              Pparam_newtype (a, b), acc

        method jfunction_param : function_param -> 'acc -> function_param * 'acc =
          fun { pparam_loc; pparam_desc } acc ->
            let pparam_loc, acc = self#location pparam_loc acc in
            let pparam_desc, acc = self#jfunction_param_desc pparam_desc acc in
            { pparam_loc; pparam_desc }, acc

        method function_constraint
          : function_constraint -> 'acc -> function_constraint * 'acc =
          fun { mode_annotations; type_constraint } acc ->
            let mode_annotations, acc = self#modes mode_annotations acc in
            let type_constraint, acc = self#type_constraint type_constraint acc in
            { mode_annotations; type_constraint }, acc

        method mode : mode -> 'acc -> mode * 'acc =
          fun x acc ->
            match x with
            | Mode a ->
              let a, acc = self#string a acc in
              Mode a, acc

        method modes : modes -> 'acc -> modes * 'acc = self#list (self#loc self#mode)

        method signature_items : signature_items -> 'acc -> signature_items * 'acc =
          self#list self#signature_item

        method signature : signature -> 'acc -> signature * 'acc = self#signature_items
      end

    class virtual ['ctx] map_with_context =
      object (self)
        method virtual arg_label : 'ctx -> arg_label -> arg_label
        method virtual attributes : 'ctx -> attributes -> attributes
        method virtual case : 'ctx -> case -> case
        method virtual core_type : 'ctx -> core_type -> core_type
        method virtual expression : 'ctx -> expression -> expression
        method virtual list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
        method virtual loc : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a loc -> 'a loc
        method virtual location : 'ctx -> location -> location
        method virtual option : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
        method virtual pattern : 'ctx -> pattern -> pattern
        method virtual signature_item : 'ctx -> signature_item -> signature_item
        method virtual string : 'ctx -> string -> string
        method virtual type_constraint : 'ctx -> type_constraint -> type_constraint

        method jkind_annotation_desc
          : 'ctx -> jkind_annotation_desc -> jkind_annotation_desc =
          fun ctx x ->
            match x with
            | Default -> Default
            | Abbreviation a ->
              let a = self#string ctx a in
              Abbreviation a
            | Mod (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#modes ctx b in
              Mod (a, b)
            | With (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#core_type ctx b in
              With (a, b)
            | Kind_of a ->
              let a = self#core_type ctx a in
              Kind_of a
            | Product a ->
              let a = self#list self#jkind_annotation ctx a in
              Product a

        method jkind_annotation : 'ctx -> jkind_annotation -> jkind_annotation =
          fun ctx { pjkind_loc; pjkind_desc } ->
            let pjkind_loc = self#location ctx pjkind_loc in
            let pjkind_desc = self#jkind_annotation_desc ctx pjkind_desc in
            { pjkind_loc; pjkind_desc }

        method jfunction_param_desc : 'ctx -> function_param_desc -> function_param_desc =
          fun ctx x ->
            match x with
            | Pparam_val (a, b, c) ->
              let a = self#arg_label ctx a in
              let b = self#option self#expression ctx b in
              let c = self#pattern ctx c in
              Pparam_val (a, b, c)
            | Pparam_newtype (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              Pparam_newtype (a, b)

        method jfunction_param : 'ctx -> function_param -> function_param =
          fun ctx { pparam_loc; pparam_desc } ->
            let pparam_loc = self#location ctx pparam_loc in
            let pparam_desc = self#jfunction_param_desc ctx pparam_desc in
            { pparam_loc; pparam_desc }

        method function_constraint : 'ctx -> function_constraint -> function_constraint =
          fun ctx { mode_annotations; type_constraint } ->
            let mode_annotations = self#modes ctx mode_annotations in
            let type_constraint = self#type_constraint ctx type_constraint in
            { mode_annotations; type_constraint }

        method mode : 'ctx -> mode -> mode =
          fun ctx x ->
            match x with
            | Mode a ->
              let a = self#string ctx a in
              Mode a

        method modes : 'ctx -> modes -> modes = self#list (self#loc self#mode)

        method signature_items : 'ctx -> signature_items -> signature_items =
          self#list self#signature_item

        method signature : 'ctx -> signature -> signature = self#signature_items
      end

    class virtual ['res] lift =
      object (self)
        method virtual record : (string * 'res) list -> 'res
        method virtual constr : string -> 'res list -> 'res
        method virtual arg_label : arg_label -> 'res
        method virtual attributes : attributes -> 'res
        method virtual case : case -> 'res
        method virtual core_type : core_type -> 'res
        method virtual expression : expression -> 'res
        method virtual list : 'a. ('a -> 'res) -> 'a list -> 'res
        method virtual loc : 'a. ('a -> 'res) -> 'a loc -> 'res
        method virtual location : location -> 'res
        method virtual option : 'a. ('a -> 'res) -> 'a option -> 'res
        method virtual pattern : pattern -> 'res
        method virtual signature_item : signature_item -> 'res
        method virtual string : string -> 'res
        method virtual type_constraint : type_constraint -> 'res

        method jkind_annotation_desc : jkind_annotation_desc -> 'res =
          fun x ->
            match x with
            | Default -> self#constr "Default" []
            | Abbreviation a ->
              let a = self#string a in
              self#constr "Abbreviation" [ a ]
            | Mod (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#modes b in
              self#constr "Mod" [ a; b ]
            | With (a, b) ->
              let a = self#jkind_annotation a in
              let b = self#core_type b in
              self#constr "With" [ a; b ]
            | Kind_of a ->
              let a = self#core_type a in
              self#constr "Kind_of" [ a ]
            | Product a ->
              let a = self#list self#jkind_annotation a in
              self#constr "Product" [ a ]

        method jkind_annotation : jkind_annotation -> 'res =
          fun { pjkind_loc; pjkind_desc } ->
            let pjkind_loc = self#location pjkind_loc in
            let pjkind_desc = self#jkind_annotation_desc pjkind_desc in
            self#record [ "pjkind_loc", pjkind_loc; "pjkind_desc", pjkind_desc ]

        method jfunction_param_desc : function_param_desc -> 'res =
          fun x ->
            match x with
            | Pparam_val (a, b, c) ->
              let a = self#arg_label a in
              let b = self#option self#expression b in
              let c = self#pattern c in
              self#constr "Pparam_val" [ a; b; c ]
            | Pparam_newtype (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              self#constr "Pparam_newtype" [ a; b ]

        method jfunction_param : function_param -> 'res =
          fun { pparam_loc; pparam_desc } ->
            let pparam_loc = self#location pparam_loc in
            let pparam_desc = self#jfunction_param_desc pparam_desc in
            self#record [ "pparam_loc", pparam_loc; "pparam_desc", pparam_desc ]

        method function_constraint : function_constraint -> 'res =
          fun { mode_annotations; type_constraint } ->
            let mode_annotations = self#modes mode_annotations in
            let type_constraint = self#type_constraint type_constraint in
            self#record
              [ "mode_annotations", mode_annotations; "type_constraint", type_constraint ]

        method mode : mode -> 'res =
          fun x ->
            match x with
            | Mode a ->
              let a = self#string a in
              self#constr "Mode" [ a ]

        method modes : modes -> 'res = self#list (self#loc self#mode)
        method signature_items : signature_items -> 'res = self#list self#signature_item
        method signature : signature -> 'res = self#signature_items
      end

    class virtual ['ctx, 'res] lift_map_with_context =
      object (self)
        method virtual record : 'ctx -> (string * 'res) list -> 'res
        method virtual constr : 'ctx -> string -> 'res list -> 'res
        method virtual arg_label : 'ctx -> arg_label -> arg_label * 'res
        method virtual attributes : 'ctx -> attributes -> attributes * 'res
        method virtual case : 'ctx -> case -> case * 'res
        method virtual core_type : 'ctx -> core_type -> core_type * 'res
        method virtual expression : 'ctx -> expression -> expression * 'res

        method
          virtual list
          : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a list -> 'a list * 'res

        method
          virtual loc
          : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a loc -> 'a loc * 'res

        method virtual location : 'ctx -> location -> location * 'res

        method
          virtual option
          : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res

        method virtual pattern : 'ctx -> pattern -> pattern * 'res
        method virtual signature_item : 'ctx -> signature_item -> signature_item * 'res
        method virtual string : 'ctx -> string -> string * 'res
        method virtual type_constraint : 'ctx -> type_constraint -> type_constraint * 'res

        method jkind_annotation_desc
          : 'ctx -> jkind_annotation_desc -> jkind_annotation_desc * 'res =
          fun ctx x ->
            match x with
            | Default -> Default, self#constr ctx "Default" []
            | Abbreviation a ->
              let a = self#string ctx a in
              Abbreviation (Stdlib.fst a), self#constr ctx "Abbreviation" [ Stdlib.snd a ]
            | Mod (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#modes ctx b in
              ( Mod (Stdlib.fst a, Stdlib.fst b)
              , self#constr ctx "Mod" [ Stdlib.snd a; Stdlib.snd b ] )
            | With (a, b) ->
              let a = self#jkind_annotation ctx a in
              let b = self#core_type ctx b in
              ( With (Stdlib.fst a, Stdlib.fst b)
              , self#constr ctx "With" [ Stdlib.snd a; Stdlib.snd b ] )
            | Kind_of a ->
              let a = self#core_type ctx a in
              Kind_of (Stdlib.fst a), self#constr ctx "Kind_of" [ Stdlib.snd a ]
            | Product a ->
              let a = self#list self#jkind_annotation ctx a in
              Product (Stdlib.fst a), self#constr ctx "Product" [ Stdlib.snd a ]

        method jkind_annotation : 'ctx -> jkind_annotation -> jkind_annotation * 'res =
          fun ctx { pjkind_loc; pjkind_desc } ->
            let pjkind_loc = self#location ctx pjkind_loc in
            let pjkind_desc = self#jkind_annotation_desc ctx pjkind_desc in
            ( { pjkind_loc = Stdlib.fst pjkind_loc; pjkind_desc = Stdlib.fst pjkind_desc }
            , self#record
                ctx
                [ "pjkind_loc", Stdlib.snd pjkind_loc
                ; "pjkind_desc", Stdlib.snd pjkind_desc
                ] )

        method jfunction_param_desc
          : 'ctx -> function_param_desc -> function_param_desc * 'res =
          fun ctx x ->
            match x with
            | Pparam_val (a, b, c) ->
              let a = self#arg_label ctx a in
              let b = self#option self#expression ctx b in
              let c = self#pattern ctx c in
              ( Pparam_val (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
              , self#constr ctx "Pparam_val" [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ]
              )
            | Pparam_newtype (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              ( Pparam_newtype (Stdlib.fst a, Stdlib.fst b)
              , self#constr ctx "Pparam_newtype" [ Stdlib.snd a; Stdlib.snd b ] )

        method jfunction_param : 'ctx -> function_param -> function_param * 'res =
          fun ctx { pparam_loc; pparam_desc } ->
            let pparam_loc = self#location ctx pparam_loc in
            let pparam_desc = self#jfunction_param_desc ctx pparam_desc in
            ( { pparam_loc = Stdlib.fst pparam_loc; pparam_desc = Stdlib.fst pparam_desc }
            , self#record
                ctx
                [ "pparam_loc", Stdlib.snd pparam_loc
                ; "pparam_desc", Stdlib.snd pparam_desc
                ] )

        method function_constraint
          : 'ctx -> function_constraint -> function_constraint * 'res =
          fun ctx { mode_annotations; type_constraint } ->
            let mode_annotations = self#modes ctx mode_annotations in
            let type_constraint = self#type_constraint ctx type_constraint in
            ( { mode_annotations = Stdlib.fst mode_annotations
              ; type_constraint = Stdlib.fst type_constraint
              }
            , self#record
                ctx
                [ "mode_annotations", Stdlib.snd mode_annotations
                ; "type_constraint", Stdlib.snd type_constraint
                ] )

        method mode : 'ctx -> mode -> mode * 'res =
          fun ctx x ->
            match x with
            | Mode a ->
              let a = self#string ctx a in
              Mode (Stdlib.fst a), self#constr ctx "Mode" [ Stdlib.snd a ]

        method modes : 'ctx -> modes -> modes * 'res = self#list (self#loc self#mode)

        method signature_items : 'ctx -> signature_items -> signature_items * 'res =
          self#list self#signature_item

        method signature : 'ctx -> signature -> signature * 'res = self#signature_items
      end

    [@@@end]
  end

  module Jane_street_extensions0 (T : sig
      type 'a t
    end) =
  struct
    class type t = object
      method jkind_annotation : jkind_annotation T.t
      method jkind_annotation_desc : jkind_annotation_desc T.t
      method jfunction_param : Pexp_function.jfunction_param T.t
      method jfunction_param_desc : Pexp_function.jfunction_param_desc T.t
      method function_constraint : Pexp_function.function_constraint T.t
      method modes : Modes.t T.t
      method mode : Mode.t T.t
      method signature_items : signature_item list T.t
    end
  end

  module Jane_street_extensions1 (T : sig
      type ('a, 'b) t
    end) =
  struct
    class type ['ctx] t = object
      method jkind_annotation : ('ctx, jkind_annotation) T.t
      method jkind_annotation_desc : ('ctx, jkind_annotation_desc) T.t
      method jfunction_param : ('ctx, Pexp_function.jfunction_param) T.t
      method jfunction_param_desc : ('ctx, Pexp_function.jfunction_param_desc) T.t
      method function_constraint : ('ctx, Pexp_function.function_constraint) T.t
      method modes : ('ctx, Modes.t) T.t
      method mode : ('ctx, Mode.t) T.t
      method signature_items : ('ctx, signature_item list) T.t
    end
  end

  module Ts = struct
    module Map = struct
      type 'a t = 'a Ppxlib_traverse_builtins.T.map
    end

    module Iter = struct
      type 'a t = 'a Ppxlib_traverse_builtins.T.iter
    end

    module Fold = struct
      type ('a, 'b) t = ('b, 'a) Ppxlib_traverse_builtins.T.fold
    end

    module Fold_map = struct
      type ('a, 'b) t = ('b, 'a) Ppxlib_traverse_builtins.T.fold_map
    end

    module Map_with_context = struct
      type ('a, 'b) t = ('a, 'b) Ppxlib_traverse_builtins.T.map_with_context
    end
  end

  class virtual map =
    object
      inherit Ppxlib_ast.Ast.map
      inherit Deriving_inline.map
    end

  class virtual iter =
    object
      inherit Ppxlib_ast.Ast.iter
      inherit Deriving_inline.iter
    end

  class virtual ['ctx] fold =
    object
      inherit ['ctx] Ppxlib_ast.Ast.fold
      inherit ['ctx] Deriving_inline.fold
    end

  class virtual ['ctx] fold_map =
    object
      inherit ['ctx] Ppxlib_ast.Ast.fold_map
      inherit ['ctx] Deriving_inline.fold_map
    end

  class virtual ['ctx] map_with_context =
    object
      inherit ['ctx] Ppxlib_ast.Ast.map_with_context
      inherit ['ctx] Deriving_inline.map_with_context
    end
end
