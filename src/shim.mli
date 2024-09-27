open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

(** This file can have a different implementation in the Jane Street experimental compiler
    and the upstream compiler, allowing ppxes to easily work with both versions *)

module Mode : sig
  (** The modes that can go on function arguments or return types *)
  type t = Mode of string [@@unboxed]
end

module Modes : sig
  type t = Mode.t loc list

  val local : t
  val none : t
end

module Include_kind : sig
  type t =
    | Structure
    | Functor
end

(** Function arguments; a value of this type represents:
    - [arg_mode arg_type -> ...] when [arg_label] is
      {{!Asttypes.arg_label.Nolabel}[Nolabel]},
    - [l:arg_mode arg_type -> ...] when [arg_label] is
      {{!Asttypes.arg_label.Labelled}[Labelled]}, and
    - [?l:arg_mode arg_type -> ...] when [arg_label] is
      {{!Asttypes.arg_label.Optional}[Optional]}. *)
type arrow_argument =
  { arg_label : arg_label
  ; arg_modes : Modes.t
  ; arg_type : core_type
  }

(** Function return types; a value of this type represents
    [... -> result_mode result_type]. *)
type arrow_result =
  { result_modes : Modes.t
  ; result_type : core_type
  }

module Modality : sig
  (** The modalities that can go on constructor fields *)
  type t = Modality of string [@@unboxed]
end

module Modalities : sig
  type t = Modality.t loc list
end

(** A list of this type is stored in the [Pcstr_tuple] constructor of
    [constructor_arguments]. With JS extensions, fields in constructors can contain
    modalities. *)
module Pcstr_tuple_arg : sig
  type t = core_type

  val extract_modalities : t -> Modality.t list * core_type
  val to_core_type : t -> core_type
  val of_core_type : core_type -> t
  val map_core_type : t -> f:(core_type -> core_type) -> t
  val map_core_type_extra : t -> f:(core_type -> core_type * 'a) -> t * 'a

  (** [loc] is ignored if there is no modality. *)
  val create : loc:Location.t -> modalities:Modality.t list -> type_:core_type -> t
end

(** This is an interface around the [Parsetree.label_declaration] type, describing one
    label in a record declaration. *)
module Label_declaration : sig
  val extract_modalities : label_declaration -> Modality.t list * label_declaration

  val create
    :  loc:Location.t
    -> name:string Location.loc
    -> mutable_:mutable_flag
    -> modalities:Modality.t list
    -> type_:core_type
    -> label_declaration
end

module Value_description : sig
  val extract_modalities : value_description -> Modality.t list * value_description

  val create
    :  loc:Location.t
    -> name:string Location.loc
    -> type_:core_type
    -> modalities:Modality.t list
    -> prim:string list
    -> value_description
end

module Value_binding : sig
  val extract_modes : value_binding -> Modes.t * value_binding

  val create
    :  loc:Location.t
    -> pat:pattern
    -> expr:expression
    -> modes:Modes.t
    -> value_binding
end

type jkind_const_annotation = string Location.loc

type jkind_annotation =
  | Default
  | Abbreviation of jkind_const_annotation
  | Mod of jkind_annotation * Modes.t
  | With of jkind_annotation * core_type
  | Kind_of of core_type
  | Product of jkind_annotation list

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
    { mode_annotations : Modes.t
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

module Core_type_desc : sig
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

  val of_parsetree : core_type_desc -> t
  val to_parsetree : t -> core_type_desc
end

module Core_type : sig
  type t =
    { ptyp_desc : Core_type_desc.t
    ; ptyp_loc : Location.t
    ; ptyp_loc_stack : Location.t list
    ; ptyp_attributes : attributes
    }

  val of_parsetree : core_type -> t
  val to_parsetree : t -> core_type
end

module Pattern_desc : sig
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

  val of_parsetree : pattern_desc -> t
  val to_parsetree : t -> pattern_desc
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

  val of_parsetree : expression_desc -> loc:Location.t -> t
  val to_parsetree : t -> expression_desc
end

module Include_infos : sig
  type 'a t =
    { pincl_kind : Include_kind.t
    ; pincl_mod : 'a
    ; pincl_loc : Location.t
    ; pincl_attributes : attributes
    }

  val of_parsetree : 'a include_infos -> 'a t
  val to_parsetree : 'a t -> 'a include_infos
end

module Signature_item_desc : sig
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

  val of_parsetree : signature_item_desc -> t
  val to_parsetree : t -> signature_item_desc
end
