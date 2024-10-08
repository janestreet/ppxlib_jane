open! Stdppx

(** A human-readable name for the language feature corresponding to the construct.
*)
module Language_feature_name : sig
  val of_core_type_desc : Shim.Core_type_desc.t -> string
  val of_expression_desc : Shim.Expression_desc.t -> string
  val of_pattern_desc : Shim.Pattern_desc.t -> string
end

(** The literal name of the constructor corresponding to the construct (e.g. Pexp_ident)
*)
module Constructor_name : sig
  val of_core_type_desc : Shim.Core_type_desc.t -> string
  val of_expression_desc : Shim.Expression_desc.t -> string
  val of_pattern_desc : Shim.Pattern_desc.t -> string
end
