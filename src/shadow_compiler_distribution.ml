(* This should be opened at the start of every file in Jane_syntax.

   These module definitions shadow the compiler's AST with ppxlib's AST. We use this
   module to manage interface differences between the two AST versions. It allows us to
   import Jane_syntax from our extended compiler with minimal changes. If we instead used
   [open Ppxlib_ast], we'd have to update more callsites. *)

module Parsetree = Ppxlib_ast.Parsetree
module Asttypes = Ppxlib_ast.Asttypes
module Pprintast = Ppxlib_ast.Pprintast

module Ast_helper = struct
  include Ppxlib_ast.Ast_helper

  module Te = struct
    include Te

    let decl ~loc ~vars ~args ?info:_ ?docs:_ ?res name = decl ~loc ~vars ~args ?res name
  end

  module Type = struct
    include Type

    let mk ~loc ~docs:_ ?text:_ ~params ~cstrs ~kind ~priv ?manifest name =
      mk ~loc ~params ~cstrs ~kind ~priv ?manifest name
    ;;

    let constructor ~loc ~vars ~info:_ ~args ?res name =
      constructor ~loc ~vars ~args ?res name
    ;;
  end
end

module Printast = struct
  (* copied and simplified from [Pprintast]. This printing is
     just used in a rarely-exercised (never-exercised?) error message
     so can be ad-hoc.
  *)
  let payload _ fmt (x : Parsetree.payload) =
    match x with
    | PStr x -> Pprintast.structure fmt x
    | PTyp x -> Pprintast.core_type fmt x
    | PSig x -> Pprintast.signature fmt x
    | PPat (x, None) -> Pprintast.pattern fmt x
    | PPat (x, Some e) ->
      Pprintast.pattern fmt x;
      Format.pp_print_string fmt " when ";
      Pprintast.expression fmt e
  ;;

  let expression _ fmt x = Astlib.Pprintast.expression fmt x
end
