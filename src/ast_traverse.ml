open Ppxlib_ast.Asttypes
module T = Ppxlib_traverse_builtins.T

class ['ctx] ppxlib_map_with_context =
  object
    inherit ['ctx] Ppxlib_traverse_builtins.map_with_context
    inherit ['ctx] Ppxlib_ast.Ast.map_with_context
  end

let both f g ctx (a, b) = f ctx a, g ctx b
let triple f g h ctx (a, b, c) = f ctx a, g ctx b, h ctx c

class ['ctx] map_with_context =
  object (self)
    inherit ['ctx] ppxlib_map_with_context as super

    method const_jkind : ('ctx, Jane_syntax.Jkind.Const.t) T.map_with_context =
      fun ctx jkind ->
        let { txt; loc } = (jkind :> string loc) in
        Jane_syntax.Jkind.Const.mk (self#string ctx txt) (self#location ctx loc)

    method jkind : ('ctx, Jane_syntax.Jkind.t) T.map_with_context =
      fun ctx -> function
        | Default -> Default
        | Abbreviation const -> Abbreviation (self#const_jkind ctx const)
        | Mod (jkind, mode_expr) ->
          Mod (self#jkind ctx jkind, self#loc (self#list self#const_mode) ctx mode_expr)
        | With (jkind, typ) -> With (self#jkind ctx jkind, self#core_type ctx typ)
        | Kind_of typ -> Kind_of (self#core_type ctx typ)

    method const_mode : ('ctx, Jane_syntax.Mode_expr.Const.t) T.map_with_context =
      fun ctx mode ->
        let { txt; loc } = (mode :> string loc) in
        Jane_syntax.Mode_expr.Const.mk (self#string ctx txt) (self#location ctx loc)

    method! attributes ctx attrs =
      let mode_expr, attrs = Jane_syntax.Mode_expr.of_attrs attrs in
      Option.to_list
        (Jane_syntax.Mode_expr.attr_of
           (self#loc (self#list self#const_mode) ctx mode_expr))
      @ super#attributes ctx attrs

    method! constructor_declaration ctx decl =
      match Jane_syntax.Layouts.of_constructor_declaration decl with
      | Some (vars_jkinds, attrs) ->
        Jane_syntax.Layouts.constructor_declaration_of
          ~loc:(self#location ctx decl.pcd_loc)
          ~attrs:(self#attributes ctx attrs)
          ~info:None
          ~vars_jkinds:
            (self#list
               (both (self#loc self#string) (self#option (self#loc self#jkind)))
               ctx
               vars_jkinds)
          ~args:(self#constructor_arguments ctx decl.pcd_args)
          ~res:(self#option self#core_type ctx decl.pcd_res)
          (self#loc self#string ctx decl.pcd_name)
      | None -> super#constructor_declaration ctx decl

    method! type_declaration ctx decl =
      match Jane_syntax.Layouts.of_type_declaration decl with
      | Some (jkind, attrs) ->
        Jane_syntax.Layouts.type_declaration_of
          ~loc:(self#location ctx decl.ptype_loc)
          ~attrs:(self#attributes ctx attrs)
          ~docs:Docstrings.empty_docs
          ~text:None
          ~params:
            (self#list
               (both self#core_type (both self#variance self#injectivity))
               ctx
               decl.ptype_params)
          ~cstrs:
            (self#list
               (triple self#core_type self#core_type self#location)
               ctx
               decl.ptype_cstrs)
          ~kind:(self#type_kind ctx decl.ptype_kind)
          ~priv:(self#private_flag ctx decl.ptype_private)
          ~manifest:(self#option self#core_type ctx decl.ptype_manifest)
          ~jkind:(Some (self#loc self#jkind ctx jkind))
          (self#loc self#string ctx decl.ptype_name)
      | None -> super#type_declaration ctx decl

    method unboxed_constant : ('ctx, Jane_syntax.Layouts.constant) T.map_with_context =
      fun ctx -> function
        | Float (s, c) -> Float (self#string ctx s, self#option self#char ctx c)
        | Integer (s, c) -> Integer (self#string ctx s, self#char ctx c)

    method! core_type ctx typ =
      match Jane_syntax.Core_type.of_ast typ with
      | Some (jtyp, attrs) ->
        Jane_syntax.Core_type.core_type_of
          ~loc:(self#location ctx typ.ptyp_loc)
          ~attrs:(self#attributes ctx attrs)
          (match jtyp with
           | Jtyp_layout ltyp ->
             Jtyp_layout
               (match ltyp with
                | Ltyp_var { name; jkind } ->
                  Ltyp_var
                    { name = self#option self#string ctx name
                    ; jkind = self#loc self#jkind ctx jkind
                    }
                | Ltyp_poly { bound_vars; inner_type } ->
                  Ltyp_poly
                    { bound_vars =
                        self#list
                          (both
                             (self#loc self#string)
                             (self#option (self#loc self#jkind)))
                          ctx
                          bound_vars
                    ; inner_type = self#core_type ctx inner_type
                    }
                | Ltyp_alias { aliased_type; name; jkind } ->
                  Ltyp_alias
                    { aliased_type = self#core_type ctx aliased_type
                    ; name = self#option self#string ctx name
                    ; jkind = self#loc self#jkind ctx jkind
                    })
           | Jtyp_tuple types ->
             Jtyp_tuple
               (self#list (both (self#option self#string) self#core_type) ctx types))
      | None -> super#core_type ctx typ

    method clause_binding
      : ('ctx, Jane_syntax.Comprehensions.clause_binding) T.map_with_context =
      fun ctx { pattern; iterator; attributes } ->
        { pattern = self#pattern ctx pattern
        ; iterator =
            (match iterator with
             | Range { start; stop; direction } ->
               Range
                 { start = self#expression ctx start
                 ; stop = self#expression ctx stop
                 ; direction = self#direction_flag ctx direction
                 }
             | In expr -> In (self#expression ctx expr))
        ; attributes = self#attributes ctx attributes
        }

    method clause : ('ctx, Jane_syntax.Comprehensions.clause) T.map_with_context =
      fun ctx -> function
        | For bindings -> For (self#list self#clause_binding ctx bindings)
        | When expr -> When (self#expression ctx expr)

    method comprehension
      : ('ctx, Jane_syntax.Comprehensions.comprehension) T.map_with_context =
      fun ctx { body; clauses } ->
        { body = self#expression ctx body; clauses = self#list self#clause ctx clauses }

    method function_param : ('ctx, Shim.Pexp_function.function_param) T.map_with_context =
      fun ctx { pparam_desc; pparam_loc } ->
        { pparam_desc =
            (match pparam_desc with
             | Pparam_val (label, expr, pat) ->
               Pparam_val
                 ( self#arg_label ctx label
                 , self#option self#expression ctx expr
                 , self#pattern ctx pat )
             | Pparam_newtype (name, jkind) ->
               Pparam_newtype
                 ( self#loc self#string ctx name
                 , self#option (self#loc self#jkind) ctx jkind ))
        ; pparam_loc = self#location ctx pparam_loc
        }
    [@@warning "-7"]
    (* Silence "the following methods are overridden by the class," since we need to
       compile against both the upstream shim (which does not define [function_param]) and
       our internal shim (which does). *)

    method function_constraint
      : ('ctx, Shim.Pexp_function.function_constraint) T.map_with_context =
      fun ctx { mode_annotations; type_constraint } ->
        { mode_annotations = self#loc (self#list self#const_mode) ctx mode_annotations
        ; type_constraint =
            (match type_constraint with
             | Pconstraint typ -> Pconstraint (self#core_type ctx typ)
             | Pcoerce (from_, to_) ->
               Pcoerce (self#option self#core_type ctx from_, self#core_type ctx to_))
        }
    [@@warning "-7"]
    (* See comment on [function_param]. *)

    method function_body : ('ctx, Shim.Pexp_function.function_body) T.map_with_context =
      fun ctx -> function
        | Pfunction_body expr -> Pfunction_body (self#expression ctx expr)
        | Pfunction_cases (cases, loc, attrs) ->
          Pfunction_cases
            ( self#list self#case ctx cases
            , self#location ctx loc
            , self#attributes ctx attrs )
    [@@warning "-7"]
    (* See comment on [function_param]. *)

    method! expression ctx expr =
      match Jane_syntax.Expression.of_ast expr with
      | Some (jexp, attrs) ->
        Jane_syntax.Expression.expr_of
          ~loc:(self#location ctx expr.pexp_loc)
          ~attrs:(self#attributes ctx attrs)
          (match jexp with
           | Jexp_comprehension cexp ->
             Jexp_comprehension
               (match cexp with
                | Cexp_list_comprehension comp ->
                  Cexp_list_comprehension (self#comprehension ctx comp)
                | Cexp_array_comprehension (flag, comp) ->
                  Cexp_array_comprehension
                    (self#mutable_flag ctx flag, self#comprehension ctx comp))
           | Jexp_immutable_array iaexp ->
             Jexp_immutable_array
               (match iaexp with
                | Iaexp_immutable_array exprs ->
                  Iaexp_immutable_array (self#list self#expression ctx exprs))
           | Jexp_layout lexp ->
             Jexp_layout
               (match lexp with
                | Lexp_constant constant ->
                  Lexp_constant (self#unboxed_constant ctx constant)
                | Lexp_newtype (name, jkind, expr) ->
                  Lexp_newtype
                    ( self#loc self#string ctx name
                    , self#loc self#jkind ctx jkind
                    , self#expression ctx expr ))
           | Jexp_tuple exprs ->
             Jexp_tuple
               (self#list (both (self#option self#string) self#expression) ctx exprs)
           | Jexp_modes mexp ->
             Jexp_modes
               (match mexp with
                | Coerce (modes, expr) ->
                  Coerce
                    ( self#loc (self#list self#const_mode) ctx modes
                    , self#expression ctx expr )))
      | None -> super#expression ctx expr

    method! pattern ctx pat =
      match Jane_syntax.Pattern.of_ast pat with
      | Some (jpat, attrs) ->
        Jane_syntax.Pattern.pat_of
          ~loc:(self#location ctx pat.ppat_loc)
          ~attrs:(self#attributes ctx attrs)
          (match jpat with
           | Jpat_immutable_array iapat ->
             Jpat_immutable_array
               (match iapat with
                | Iapat_immutable_array pats ->
                  Iapat_immutable_array (self#list self#pattern ctx pats))
           | Jpat_layout lpat ->
             Jpat_layout
               (match lpat with
                | Lpat_constant constant ->
                  Lpat_constant (self#unboxed_constant ctx constant))
           | Jpat_tuple (pats, flag) ->
             Jpat_tuple
               ( self#list (both (self#option self#string) self#pattern) ctx pats
               , self#closed_flag ctx flag ))
      | None -> super#pattern ctx pat

    method! module_type ctx mty =
      match Jane_syntax.Module_type.of_ast mty with
      | Some (jmty, attrs) ->
        Jane_syntax.Module_type.mty_of
          ~loc:(self#location ctx mty.pmty_loc)
          ~attrs:(self#attributes ctx attrs)
          (match jmty with
           | Jmty_strengthen { mty; mod_id } ->
             Jmty_strengthen
               { mty = self#module_type ctx mty; mod_id = self#longident_loc ctx mod_id })
      | None -> super#module_type ctx mty

    method! signature_item ctx sigi =
      match Jane_syntax.Signature_item.of_ast sigi with
      | Some (Jsig_include_functor ifsigi) ->
        Jane_syntax.Include_functor.sig_item_of
          ~loc:(self#location ctx sigi.psig_loc)
          (match ifsigi with
           | Ifsig_include_functor desc ->
             Ifsig_include_functor (self#include_description ctx desc))
      | Some (Jsig_layout lsigi) ->
        Jane_syntax.Layouts.sig_item_of
          ~loc:(self#location ctx sigi.psig_loc)
          (match lsigi with
           | Lsig_kind_abbrev (name, jkind) ->
             Lsig_kind_abbrev
               (self#loc self#string ctx name, self#loc self#jkind ctx jkind))
      | None -> super#signature_item ctx sigi

    method! structure_item ctx stri =
      match Jane_syntax.Structure_item.of_ast stri with
      | Some (Jstr_include_functor ifstri) ->
        Jane_syntax.Include_functor.str_item_of
          ~loc:(self#location ctx stri.pstr_loc)
          (match ifstri with
           | Ifstr_include_functor decl ->
             Ifstr_include_functor (self#include_declaration ctx decl))
      | Some (Jstr_layout lstri) ->
        Jane_syntax.Layouts.str_item_of
          ~loc:(self#location ctx stri.pstr_loc)
          (match lstri with
           | Lstr_kind_abbrev (name, jkind) ->
             Lstr_kind_abbrev
               (self#loc self#string ctx name, self#loc self#jkind ctx jkind))
      | None -> super#structure_item ctx stri

    method! extension_constructor ctx ext =
      match Jane_syntax.Extension_constructor.of_ast ext with
      | Some (jext, attrs) ->
        Jane_syntax.Extension_constructor.extension_constructor_of
          ~loc:(self#location ctx ext.pext_loc)
          ~name:(self#loc self#string ctx ext.pext_name)
          ~attrs:(self#attributes ctx attrs)
          (match jext with
           | Jext_layout lext ->
             Jext_layout
               (match lext with
                | Lext_decl (vars, args, typ) ->
                  Lext_decl
                    ( self#list
                        (both (self#loc self#string) (self#option (self#loc self#jkind)))
                        ctx
                        vars
                    , self#constructor_arguments ctx args
                    , self#option self#core_type ctx typ )))
      | None -> super#extension_constructor ctx ext
  end
