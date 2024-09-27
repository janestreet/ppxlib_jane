open Shadow_compiler_distribution

(** Similar to {!Ppxlib.get_type_param_name}, but also tells you the jkind of the
    variable. Raises a located error in case of failure. The type parameter should not be
    [_]. One way to ensure this is to get the type from {!Ppxlib.name_type_params_in_td}
    (or one of the similarly-named functions).
*)
val get_type_param_name_and_jkind
  :  Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)
  -> string Location.loc * Jane_syntax.Jkind.annotation option
