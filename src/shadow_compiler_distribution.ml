(* This should be opened at the start of every file in Jane_syntax.

   These module definitions shadow the compiler's AST with ppxlib's AST. We use this
   module to manage interface differences between the two AST versions. It allows us to
   import Jane_syntax from our extended compiler with minimal changes. If we instead used
   [open Ppxlib_ast], we'd have to update more callsites. *)

module Asttypes = Ppxlib_ast.Asttypes
module Pprintast = Ppxlib_ast.Pprintast

module Parsetree = struct
  include Ppxlib_ast.Parsetree

  type mode = Shim.Mode.t = Mode of string [@@unboxed]
  type modes = Shim.Modes.t
  type modality = Shim.Modality.t = Modality of string [@@unboxed]
  type modalities = Shim.Modalities.t
  type jkind_const_annotation = Shim.jkind_const_annotation

  type jkind_annotation = Shim.jkind_annotation =
    | Default
    | Abbreviation of jkind_const_annotation
    | Mod of jkind_annotation * modes
    | With of jkind_annotation * core_type
    | Kind_of of core_type
    | Product of jkind_annotation list
end

module Ast_helper = struct
  include Ppxlib_ast.Ast_helper

  module Te = struct
    include Te

    let decl ~loc ~vars ~args ?info:_ ?docs:_ ?res name = decl ~loc ~vars ~args ?res name
  end

  module Type = struct
    include Type

    let mk ?loc ?docs:_ ?text:_ ?params ?cstrs ?kind ?priv ?manifest name =
      mk ?loc ?params ?cstrs ?kind ?priv ?manifest name
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
    Format_doc.deprecated_printer (fun fmt ->
      match (x : Parsetree.payload) with
      | PStr x -> Pprintast.structure fmt x
      | PTyp x -> Pprintast.core_type fmt x
      | PSig x -> Pprintast.signature fmt x
      | PPat (x, None) -> Pprintast.pattern fmt x
      | PPat (x, Some e) ->
        Pprintast.pattern fmt x;
        Format.pp_print_string fmt " when ";
        Pprintast.expression fmt e)
      fmt
  ;;

  let expression _ fmt x =
    Format_doc.deprecated_printer
      (fun fmt -> Astlib.Pprintast.expression fmt x) fmt
end

module Misc = struct
  module Stdlib = struct
    module List = struct
      let map_option f l =
        let rec aux l acc =
          match l with
          | [] -> Some (List.rev acc)
          | x :: xs ->
            (match f x with
             | None -> None
             | Some x -> aux xs (x :: acc))
        in
        aux l []
      ;;
    end
  end
end
