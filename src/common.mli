open Ppxlib_ast

(** Similar to {!Ppxlib.get_type_param_name}, but also tells you the jkind of the
    variable. Raises a located error in case of failure. The type parameter should not be
    [_]. One way to ensure this is to get the type from {!Ppxlib.name_type_params_in_td}
    (or one of the similarly-named functions).
*)
val get_type_param_name_and_jkind
  :  Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)
  -> string Location.loc * Shim.jkind_annotation option

(** Check if the parsetree is of an unlabeled tuple (i.e. all labels are [None]), and
    return the components of the tuple dropping its [None] labels. *)
val as_unlabeled_tuple : (string option * 'a) list -> 'a list option
