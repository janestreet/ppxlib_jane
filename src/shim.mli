open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

(** This file can have a different implementation in the Jane Street experimental compiler
    and the upstream compiler, allowing ppxes to easily work with both versions *)

(** The modes that can go on function arguments or return types *)
type mode = Mode of string [@@unboxed]

(** Function arguments; a value of this type represents:
    - [arg_mode arg_type -> ...] when [arg_label] is
      {{!Asttypes.arg_label.Nolabel}[Nolabel]},
    - [l:arg_mode arg_type -> ...] when [arg_label] is
      {{!Asttypes.arg_label.Labelled}[Labelled]}, and
    - [?l:arg_mode arg_type -> ...] when [arg_label] is
      {{!Asttypes.arg_label.Optional}[Optional]}. *)
type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : mode list
  ; arg_type : core_type
  }

(** Function return types; a value of this type represents
    [... -> result_mode result_type]. *)
type arrow_result =
  { result_modes : mode list
  ; result_type : core_type
  }

(** The modalities that can go on constructor fields *)
type modality = Modality of string [@@unboxed]

(** A list of this type is stored in the [Pcstr_tuple] constructor of
    [constructor_arguments]. With JS extensions, fields in constructors can contain
    modalities. *)
module Pcstr_tuple_arg : sig
  type t = core_type

  val extract_modalities : t -> modality list * core_type
  val to_core_type : t -> core_type
  val of_core_type : core_type -> t
  val map_core_type : t -> f:(core_type -> core_type) -> t
  val map_core_type_extra : t -> f:(core_type -> core_type * 'a) -> t * 'a

  (** [loc] is ignored if there is no modality. *)
  val create : loc:Location.t -> modalities:modality list -> type_:core_type -> t
end

(** This is an interface around the [Parsetree.label_declaration] type, describing one
    label in a record declaration. *)
module Label_declaration : sig
  val extract_modalities : label_declaration -> modality list * label_declaration

  val create
    :  loc:Location.t
    -> name:string Location.loc
    -> mutable_:mutable_flag
    -> modalities:modality list
    -> type_:core_type
    -> label_declaration
end

module Value_description : sig
  val extract_modalities : value_description -> modality list * value_description

  val create
    :  loc:Location.t
    -> name:string Location.loc
    -> type_:core_type
    -> modalities:modality list
    -> prim:string list
    -> value_description
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

(** Match and construct [Pexp_function], as in the OCaml parsetree at or after 5.2. *)
module Pexp_function : sig
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

  val to_parsetree
    :  params:function_param list
    -> constraint_:function_constraint option
    -> body:function_body
    -> expression_desc

  val of_parsetree
    :  expression_desc
    -> loc:Location.t
    -> (function_param list * function_constraint option * function_body) option
end

module Expression_desc : sig
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

  val of_parsetree : expression_desc -> loc:Location.t -> t
  val to_parsetree : t -> expression_desc
end
