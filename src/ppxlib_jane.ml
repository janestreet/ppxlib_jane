module Shim = Shim
module Ast_builder = Ast_builder
module Ast_traverse = Ast_traverse
module Jane_syntax = Jane_syntax
module Legacy_pexp_function = Legacy_pexp_function

(** {2 Common helper functions} *)
include Common

(** {2 Common Jane Street helper types} *)

type modality = Shim.modality = Modality of string [@@unboxed]
type mode = Shim.mode = Mode of string [@@unboxed]

type arrow_result = Shim.arrow_result =
  { result_modes : mode list
  ; result_type : Ppxlib_ast.Parsetree.core_type
  }

type arrow_argument = Shim.arrow_argument =
  { arg_label : Ppxlib_ast.Asttypes.arg_label
  ; arg_modes : mode list
  ; arg_type : Ppxlib_ast.Parsetree.core_type
  }

module For_testing = struct
  module Language_extension = Language_extension
  module Language_extension_kernel = Language_extension_kernel
end
