(*_ This file is manually imported from the Jane Street version of the
   OCaml compiler. Don't make changes directly to this file. *)
[@@@ocaml.warning "-missing-record-field-pattern"]

open! Shadow_compiler_distribution

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Antal Spector-Zabusky, Jane Street, New York             *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary Jane Street extensions to AST types used by parsetree and
    typedtree.

    This file exists because [Asttypes] is considered part of the parse tree,
    and we can't modify the parse tree.  This also enables us to build other
    files with the upstream compiler as long as [jane_asttypes.mli] is present;
    see Note [Buildable with upstream] in jane_syntax.mli for details on that.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

(** [const_jkind] is private to limit confusion with type variables, which
    are also strings in the parser.
*)
type const_jkind

val jkind_of_string : string -> const_jkind
val jkind_to_string : const_jkind -> string

type jkind_annotation = const_jkind Location.loc
