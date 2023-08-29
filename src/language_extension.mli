(** A [ppxlib_jane]-specific copy of the Jane Street-internal language
    extensions framework that reports that every language extension
    is enabled. This is the most permissive behavior possible, which
    is what we want for ppxes that process pieces of Jane Syntax.
*)

include Language_extension_kernel.Language_extension_for_jane_syntax (** @inline *)
