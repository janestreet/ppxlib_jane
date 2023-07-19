open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

module type S = sig
  type 'a with_loc


  (** Many comments below make reference to the Jane Street compiler's treatment of
      function arity. These comments refer to a parsetree change made to upstream OCaml in
      https://github.com/ocaml/ocaml/pull/12236, but that Jane Street has mirrored
      internally already.

      The treatment of arity can be summarized as follows:
      - In a previous version of OCaml, a function's runtime arity was inferred at a
        late stage of the compiler, after typechecking, where it fuses together
        nested lambdas.
      - In the new version of OCaml (both upstream OCaml after #12236 and the
        internal Jane Street compiler), a function's runtime arity is purely a syntactic
        notion: it's the number of parameters in a [fun x1 ... xn -> body] construct,
        with some special allowances for function cases.

      Why is arity important? In native code, application sites of a function to [n]
      syntactic arguments will trigger a fast path (where arguments are passed in
      registers) only if the function's runtime arity is [n].

      As a result, ppxes must take more care than before to generate functions of the
      correct arity. Now, a nested function like [fun x -> fun y -> e] has arity 1
      (returning still another function of arity 1) instead of arity 2. All bindings
      below that construct functions are documented as to the arity of the returned
      function.

      Some examples of arity:
      - 2-ary function: [fun x y -> e]
      - 1-ary function returning 1-ary function: [fun x -> fun y -> e]
      - 3-ary function: [fun x y -> function P1 -> e1 | P2 -> e2]
      - 2-ary function returning 1-ary function: [fun x y -> (function P1 -> e1 | P2 -> e2)]
      - 2-ary function returning 1-ary function: [fun x -> function P1 -> function P2 -> e]

      Notably, unparenthesized [function] has a special meaning when used as a direct body
      of [fun]: the [function] becomes part of the arity of the outer [fun]. The same
      does not apply for multiple nested [function]s, even if they each have a single
      case; the nested [function]s are treated as unary. (See the last example.)
  *)

  (** This type corresponds to [Parsetree.function_param] added in #12236; see the header
      comment. *)
  type function_param =
    | Pparam_val of arg_label * expression option * pattern
    (** Pparam_val (lbl, def, pat)
        - lbl is the parameter label
        - def is the default argument for an optional parameter
        - pat is the pattern that is matched against the argument.
          See comment on {!Parsetree.Pexp_fun} for more detail. *)
    | Pparam_newtype of string loc

  (** Create a function with unlabeled parameters and an expression body. Like
      {!Ppxlib.Ast_builder.eapply}, but for constructing functions.

      [coalesce_arity] is relevant for the Jane Street compiler. By default,
      [coalesce_arity] is [true].

      Suppose there is a call [eabstract pats body ~coalesce_arity]
      - If [colaesce_arity] is true, the arity of the returned function
        is the same as the arity of:
        [Fun.add_params (List.map params ~f:(Fun.param Nolabel)) body]
      - If [coalesce_arity] is [false], then the arity of the returned function
        is the length of [pats].

      In other words, [coalesce_arity = true] allows you to build up the arity of
      an already-constructed function rather than necessarily creating a new function.

  *)
  val eabstract
    : (?coalesce_arity:bool -> pattern list -> expression -> expression) with_loc

  (** [unary_function cases] is [function <cases>]. When used with the Jane Street
      compiler, the function's runtime arity is 1, so the fast path for function
      application happens only when application sites of the resulting function receive
      1 argument. To create a function with multiple argument that pattern-matches on
      the last one, use [add_param] or [add_params] to add more parameters.
      Alternatively, use [pexp_function] to provide all parameters at once.
  *)
  val unary_function : (case list -> expression) with_loc

  (** [fun_param lbl pat] is [Pparam_val (lbl, None, pat)]. This gives a more
      self-documenting way of constructing the usual case: value parameters without
      optional argument defaults.
  *)
  val fun_param : arg_label -> pattern -> function_param


  (** Say an expression is a "function" if it is a [Pexp_fun] or a [Pexp_function].
      All functions have parameters and arity.

      Suppose [add_param lbl def pat e ==> e']. Then, letting
      [param = Pparam_val (lbl, def, pat)],
      - If [e] is a function with arity [n], then [e'] is a function with arity [n+1].
        [param] is added at the outermost layer. For example, if
        [e = fun <params> -> <body>], then [e' = fun <param :: params> -> body].
      - If [e] is not a function, then [e'] is a function with arity [1], namely:
        [fun <param> -> <e>].
  *)
  val add_fun_param
    : (arg_label -> expression option -> pattern -> expression -> expression) with_loc

  (** [add_params params e] is [List.fold_right params ~init:e ~f:add_param].
      Note the [fold_right]: if [e] is [fun <params'> -> <body>], then
      [add_params params e] is [fun <params @ params'> -> <body>].
  *)
  val add_fun_params : (function_param list -> expression -> expression) with_loc
end

module type S_with_implicit_loc = S with type 'a with_loc := 'a
module type S_with_explicit_loc = S with type 'a with_loc := loc:Location.t -> 'a

module type Ast_builder = sig
  (** Jane Street-internal extensions to {!Ppxlib.Ast_builder}. The bindings below
      ([Default], [Make], etc.) are parallel to bindings exported from
      [Ppxlib.Ast_builder]. *)

  module type S_with_implicit_loc = S_with_implicit_loc

  module Default : S_with_explicit_loc

  module Make (Loc : sig
      val loc : Location.t
    end) : S_with_implicit_loc

  val make : Location.t -> (module S_with_implicit_loc)
end
