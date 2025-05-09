open! Stdppx

module Language_feature_name = struct
  let of_core_type_desc : Shim.Core_type_desc.t -> string = function
    | Ptyp_var _ -> "type variable"
    | Ptyp_tuple _ -> "tuple type"
    | Ptyp_constr _ -> "type constructor"
    | Ptyp_alias _ -> "type alias"
    | Ptyp_variant (_, Open, _) -> "open polymorphic variant type"
    | Ptyp_variant (_, Closed, _) -> "closed polymorphic variant type"
    | Ptyp_any _ -> "wildcard type"
    | Ptyp_arrow _ -> "arrow type"
    | Ptyp_unboxed_tuple _ -> "unboxed tuple type"
    | Ptyp_object _ -> "object type"
    | Ptyp_class _ -> "class as a type"
    | Ptyp_poly _ -> "explicit polymorphic type"
    | Ptyp_package _ -> "first-class module type"
    | Ptyp_extension _ -> "extension point as a type"
  ;;

  let of_expression_desc : Shim.Expression_desc.t -> string = function
    | Pexp_ident _ -> "variable expression"
    | Pexp_constant c ->
      (match Shim.Constant.of_parsetree c with
       | Pconst_char _ -> "character literal expression"
       | Pconst_integer _ -> "integer literal expression"
       | Pconst_string _ -> "string literal expression"
       | Pconst_float _ -> "floating point literal expression"
       | Pconst_unboxed_float _ -> "unboxed floating point literal expression"
       | Pconst_unboxed_integer _ -> "unboxed integer literal expression")
    | Pexp_let _ -> "let expression"
    | Pexp_function _ -> "function expression"
    | Pexp_apply _ -> "application expression"
    | Pexp_match _ -> "match expression"
    | Pexp_try _ -> "try-with expression"
    | Pexp_tuple _ -> "tuple expression"
    | Pexp_unboxed_tuple _ -> "unboxed tuple expression"
    | Pexp_construct _ -> "variant constructor expression"
    | Pexp_variant _ -> "polymorphic variant constructor expression"
    | Pexp_record (_, None) -> "record expression"
    | Pexp_record (_, Some _) -> "record 'with' expression"
    | Pexp_record_unboxed_product (_, None) -> "unboxed record expression"
    | Pexp_record_unboxed_product (_, Some _) -> "unboxed record 'with' expression"
    | Pexp_field _ -> "field access expression"
    | Pexp_unboxed_field _ -> "unboxed field access expression"
    | Pexp_setfield _ -> "field set expression"
    | Pexp_array (Mutable, _) -> "array expression"
    | Pexp_array (Immutable, _) -> "immutable array expression"
    | Pexp_ifthenelse (_, _, None) -> "if-then expression"
    | Pexp_ifthenelse (_, _, Some _) -> "if-then-else expression"
    | Pexp_sequence _ -> "sequencing ';' expression"
    | Pexp_while _ -> "while loop expression"
    | Pexp_for _ -> "for loop expression"
    | Pexp_constraint _ -> "type constraint ':' expression"
    | Pexp_coerce _ -> "type coercion ':>' expression"
    | Pexp_send _ -> "method send '#' expression"
    | Pexp_new _ -> "new-object expression"
    | Pexp_setinstvar _ -> "instance variable set '<-' expression"
    | Pexp_override _ -> "instance variable override '<{' ... '}>' expression"
    | Pexp_letmodule _ -> "let-module expression"
    | Pexp_letexception _ -> "let-exception expression"
    | Pexp_assert _ -> "assert expression"
    | Pexp_lazy _ -> "lazy expression"
    | Pexp_poly _ -> "polymorphic method body expression"
    | Pexp_object _ -> "object expression"
    | Pexp_newtype _ -> "type-parameterized expression"
    | Pexp_pack _ -> "first-class module expression"
    | Pexp_open _ -> "let-open '.' expression"
    | Pexp_letop _ -> "let-op expression"
    | Pexp_extension _ -> "extension point as an expression"
    | Pexp_unreachable -> "unreachable '.' expression"
    | Pexp_stack _ -> "stack expression"
    | Pexp_comprehension (Pcomp_list_comprehension _) -> "list comprehension expression"
    | Pexp_comprehension (Pcomp_array_comprehension _) -> "array comprehension expression"
    | Pexp_overwrite _ -> "overwrite"
    | Pexp_hole -> "hole"
  ;;

  let of_pattern_desc : Shim.Pattern_desc.t -> string = function
    | Ppat_any -> "wildcard '_' pattern"
    | Ppat_var _ -> "variable pattern"
    | Ppat_alias _ -> "alias 'as' pattern"
    | Ppat_constant c ->
      (match Shim.Constant.of_parsetree c with
       | Pconst_char _ -> "character literal pattern"
       | Pconst_integer _ -> "integer literal pattern"
       | Pconst_unboxed_integer _ -> "unboxed integer literal pattern"
       | Pconst_string _ -> "string literal pattern"
       | Pconst_float _ -> "floating point literal pattern"
       | Pconst_unboxed_float _ -> "unboxed floating point literal pattern")
    | Ppat_interval _ -> "character interval '..' pattern"
    | Ppat_tuple _ -> "tuple pattern"
    | Ppat_unboxed_tuple _ -> "unboxed tuple pattern"
    | Ppat_construct _ -> "variant constructor pattern"
    | Ppat_variant _ -> "polymorphic variant constructor pattern"
    | Ppat_record (_, Closed) -> "closed record pattern"
    | Ppat_record (_, Open) -> "open record pattern"
    | Ppat_record_unboxed_product (_, Closed) -> "closed unboxed record pattern"
    | Ppat_record_unboxed_product (_, Open) -> "open unboxed record pattern"
    | Ppat_array (Mutable, _) -> "array pattern"
    | Ppat_array (Immutable, _) -> "immutable array pattern"
    | Ppat_or _ -> "or '|' pattern"
    | Ppat_constraint _ -> "type constraint ':' pattern"
    | Ppat_type _ -> "type '#' pattern"
    | Ppat_lazy _ -> "lazy pattern"
    | Ppat_unpack _ -> "first-class module pattern"
    | Ppat_exception _ -> "exception pattern"
    | Ppat_extension _ -> "extension point as a pattern"
    | Ppat_open _ -> "let-open '.' pattern"
  ;;

  let of_signature_item_desc : Shim.Signature_item_desc.t -> string = function
    | Psig_value _ -> "val declaration"
    | Psig_type _ -> "type declaration"
    | Psig_typesubst _ -> "type alias ':='"
    | Psig_typext _ -> "extensible variant constructor declaration"
    | Psig_exception _ -> "exception declaration"
    | Psig_module _ -> "module declaration"
    | Psig_modsubst _ -> "module alias ':='"
    | Psig_recmodule _ -> "recursive module declaration"
    | Psig_modtype _ -> "module type declaration"
    | Psig_modtypesubst _ -> "module type alias ':='"
    | Psig_open _ -> "module open (in a sig)"
    | Psig_include _ -> "module include (in a sig)"
    | Psig_class _ -> "class declaration"
    | Psig_class_type _ -> "class type declaration"
    | Psig_attribute _ -> "module-level attribute (in a sig)"
    | Psig_extension _ -> "extension point as a signature item"
    | Psig_kind_abbrev _ -> "kind abbrevation declaration"
  ;;

  let of_structure_item_desc : Shim.Structure_item_desc.t -> string = function
    | Pstr_value _ -> "let definition"
    | Pstr_type _ -> "type definition"
    | Pstr_typext _ -> "extensible variant constructor definition"
    | Pstr_exception _ -> "exception definition"
    | Pstr_module _ -> "module definition"
    | Pstr_recmodule _ -> "recursive module definition"
    | Pstr_modtype _ -> "module type definition"
    | Pstr_open _ -> "module open (in a struct)"
    | Pstr_include _ -> "module include (in a struct)"
    | Pstr_class _ -> "class definition"
    | Pstr_class_type _ -> "class type definition"
    | Pstr_attribute _ -> "module-level attribute (in a struct)"
    | Pstr_extension _ -> "extension point as a structure item"
    | Pstr_kind_abbrev _ -> "kind abbreviation definition"
    | Pstr_eval _ -> "top-level expression"
    | Pstr_primitive _ -> "primitive 'external' binding"
  ;;

  let of_module_type_desc : Shim.Module_type_desc.t -> string = function
    | Pmty_ident _ -> "module type identifier"
    | Pmty_signature _ -> "module type expression"
    | Pmty_functor _ -> "functor module type"
    | Pmty_with _ -> "module type with constraints"
    | Pmty_typeof _ -> "'module type of'"
    | Pmty_extension _ -> "extension point as module type"
    | Pmty_alias _ -> "module type alias"
    | Pmty_strengthen _ -> "strengthened module type"
  ;;

  let of_module_expr_desc : Shim.Module_expr_desc.t -> string = function
    | Pmod_ident _ -> "module identifier"
    | Pmod_structure _ -> "module expression"
    | Pmod_functor _ -> "functor"
    | Pmod_apply _ -> "functor application"
    | Pmod_constraint _ -> "module with constraints"
    | Pmod_unpack _ -> "'val' unpacking of expression as module"
    | Pmod_extension _ -> "extension point as module"
    | Pmod_instance _ -> "module instance"
  ;;
end

module Constructor_name = struct
  let of_core_type_desc : Shim.Core_type_desc.t -> string = function
    | Ptyp_var _ -> "Ptyp_var"
    | Ptyp_tuple _ -> "Ptyp_tuple"
    | Ptyp_constr _ -> "Ptyp_constr"
    | Ptyp_alias _ -> "Ptyp_alias"
    | Ptyp_variant _ -> "Ptyp_variant"
    | Ptyp_any _ -> "Ptyp_any"
    | Ptyp_arrow _ -> "Ptyp_arrow"
    | Ptyp_unboxed_tuple _ -> "Ptyp_unboxed_tuple"
    | Ptyp_object _ -> "Ptyp_object"
    | Ptyp_class _ -> "Ptyp_class"
    | Ptyp_poly _ -> "Ptyp_poly"
    | Ptyp_package _ -> "Ptyp_package"
    | Ptyp_extension _ -> "Ptyp_extension"
  ;;

  let of_expression_desc : Shim.Expression_desc.t -> string = function
    | Pexp_ident _ -> "Pexp_ident"
    | Pexp_constant _ -> "Pexp_constant"
    | Pexp_let _ -> "Pexp_let"
    | Pexp_function _ -> "Pexp_function"
    | Pexp_apply _ -> "Pexp_apply"
    | Pexp_match _ -> "Pexp_match"
    | Pexp_try _ -> "Pexp_try"
    | Pexp_tuple _ -> "Pexp_tuple"
    | Pexp_unboxed_tuple _ -> "Pexp_unboxed_tuple"
    | Pexp_construct _ -> "Pexp_construct"
    | Pexp_variant _ -> "Pexp_variant"
    | Pexp_record _ -> "Pexp_record"
    | Pexp_record_unboxed_product _ -> "Pexp_record_unboxed_product"
    | Pexp_field _ -> "Pexp_field"
    | Pexp_unboxed_field _ -> "Pexp_unboxed_field"
    | Pexp_setfield _ -> "Pexp_setfield"
    | Pexp_array _ -> "Pexp_array"
    | Pexp_ifthenelse _ -> "Pexp_ifthenelse"
    | Pexp_sequence _ -> "Pexp_sequence"
    | Pexp_while _ -> "Pexp_while"
    | Pexp_for _ -> "Pexp_for"
    | Pexp_constraint _ -> "Pexp_constraint"
    | Pexp_coerce _ -> "Pexp_coerce"
    | Pexp_send _ -> "Pexp_send"
    | Pexp_new _ -> "Pexp_new"
    | Pexp_setinstvar _ -> "Pexp_setinstvar"
    | Pexp_override _ -> "Pexp_override"
    | Pexp_letmodule _ -> "Pexp_letmodule"
    | Pexp_letexception _ -> "Pexp_letexception"
    | Pexp_assert _ -> "Pexp_assert"
    | Pexp_lazy _ -> "Pexp_lazy"
    | Pexp_poly _ -> "Pexp_poly"
    | Pexp_object _ -> "Pexp_object"
    | Pexp_newtype _ -> "Pexp_newtype"
    | Pexp_pack _ -> "Pexp_pack"
    | Pexp_open _ -> "Pexp_open"
    | Pexp_letop _ -> "Pexp_letop"
    | Pexp_extension _ -> "Pexp_extension"
    | Pexp_unreachable -> "Pexp_unreachable"
    | Pexp_stack _ -> "Pexp_stack"
    | Pexp_comprehension _ -> "Pexp_comprehension"
    | Pexp_overwrite _ -> "Pexp_overwrite"
    | Pexp_hole -> "Pexp_hole"
  ;;

  let of_pattern_desc : Shim.Pattern_desc.t -> string = function
    | Ppat_any -> "Ppat_any"
    | Ppat_var _ -> "Ppat_var"
    | Ppat_alias _ -> "Ppat_alias"
    | Ppat_constant _ -> "Ppat_constant"
    | Ppat_interval _ -> "Ppat_interval"
    | Ppat_tuple _ -> "Ppat_tuple"
    | Ppat_unboxed_tuple _ -> "Ppat_unboxed_tuple"
    | Ppat_construct _ -> "Ppat_construct"
    | Ppat_variant _ -> "Ppat_variant"
    | Ppat_record _ -> "Ppat_record"
    | Ppat_record_unboxed_product _ -> "Ppat_record_unboxed_product"
    | Ppat_array _ -> "Ppat_array"
    | Ppat_or _ -> "Ppat_or"
    | Ppat_constraint _ -> "Ppat_constraint"
    | Ppat_type _ -> "Ppat_type"
    | Ppat_lazy _ -> "Ppat_lazy"
    | Ppat_unpack _ -> "Ppat_unpack"
    | Ppat_exception _ -> "Ppat_exception"
    | Ppat_extension _ -> "Ppat_extension"
    | Ppat_open _ -> "Ppat_open"
  ;;

  let of_signature_item_desc : Shim.Signature_item_desc.t -> string = function
    | Psig_value _ -> "Psig_value"
    | Psig_type _ -> "Psig_type"
    | Psig_typesubst _ -> "Psig_typesubst"
    | Psig_typext _ -> "Psig_typext"
    | Psig_exception _ -> "Psig_exception"
    | Psig_module _ -> "Psig_module"
    | Psig_modsubst _ -> "Psig_modsubst"
    | Psig_recmodule _ -> "Psig_recmodule"
    | Psig_modtype _ -> "Psig_modtype"
    | Psig_modtypesubst _ -> "Psig_modtypesubst"
    | Psig_open _ -> "Psig_open"
    | Psig_include _ -> "Psig_include"
    | Psig_class _ -> "Psig_class"
    | Psig_class_type _ -> "Psig_class_type"
    | Psig_attribute _ -> "Psig_attribute"
    | Psig_extension _ -> "Psig_extension"
    | Psig_kind_abbrev _ -> "Psig_kind_abbrev"
  ;;

  let of_structure_item_desc : Shim.Structure_item_desc.t -> string = function
    | Pstr_value _ -> "Pstr_value"
    | Pstr_type _ -> "Pstr_type"
    | Pstr_typext _ -> "Pstr_typext"
    | Pstr_exception _ -> "Pstr_exception"
    | Pstr_module _ -> "Pstr_module"
    | Pstr_recmodule _ -> "Pstr_recmodule"
    | Pstr_modtype _ -> "Pstr_modtype"
    | Pstr_open _ -> "Pstr_open"
    | Pstr_include _ -> "Pstr_include"
    | Pstr_class _ -> "Pstr_class"
    | Pstr_class_type _ -> "Pstr_class_type"
    | Pstr_attribute _ -> "Pstr_attribute"
    | Pstr_extension _ -> "Pstr_extension"
    | Pstr_kind_abbrev _ -> "Pstr_kind_abbrev"
    | Pstr_eval _ -> "Pstr_eval"
    | Pstr_primitive _ -> "Pstr_primitive"
  ;;

  let of_module_type_desc : Shim.Module_type_desc.t -> string = function
    | Pmty_ident _ -> "Pmty_ident"
    | Pmty_signature _ -> "Pmty_signature"
    | Pmty_functor _ -> "Pmty_functor"
    | Pmty_with _ -> "Pmty_with"
    | Pmty_typeof _ -> "Pmty_typeof"
    | Pmty_extension _ -> "Pmty_extension"
    | Pmty_alias _ -> "Pmty_alias"
    | Pmty_strengthen _ -> "Pmty_strengthen"
  ;;

  let of_module_expr_desc : Shim.Module_expr_desc.t -> string = function
    | Pmod_ident _ -> "Pmod_ident"
    | Pmod_structure _ -> "Pmod_structure"
    | Pmod_functor _ -> "Pmod_functor"
    | Pmod_apply _ -> "Pmod_apply"
    | Pmod_constraint _ -> "Pmod_constraint"
    | Pmod_unpack _ -> "Pmod_unpack"
    | Pmod_extension _ -> "Pmod_extension"
    | Pmod_instance _ -> "Pmod_instance"
  ;;
end
