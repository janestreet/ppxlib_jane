open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
open Stdppx
include Ast_builder_intf
include Shim

let core_type ptyp_desc ~loc:ptyp_loc =
  { ptyp_desc; ptyp_loc; ptyp_attributes = []; ptyp_loc_stack = [] }
;;

module Default = struct
  include Shim

  type function_param = Shim.Pexp_function.function_param
  type function_constraint = Shim.Pexp_function.function_constraint
  type function_body = Shim.Pexp_function.function_body

  let mark_type_with_mode_expr modes ty =
    let attr = Jane_syntax.Mode_expr.attr_of modes in
    match attr with
    | None -> ty
    | Some attr -> { ty with ptyp_attributes = attr :: ty.ptyp_attributes }
  ;;

  let mode_expr_of_modes ~loc modes =
    List.fold_right modes ~init:Jane_syntax.Mode_expr.empty ~f:(fun (Mode name) acc ->
      let mode = Jane_syntax.Mode_expr.Const.mk name loc in
      Jane_syntax.Mode_expr.concat { txt = [ mode ]; loc } acc)
  ;;

  let mark_type_with_modes ~loc mode ty =
    mark_type_with_mode_expr (mode_expr_of_modes ~loc mode) ty
  ;;

  let ptyp_arrow ~loc { arg_label; arg_modes; arg_type } { result_modes; result_type } =
    core_type
      ~loc
      (Ptyp_arrow
         ( arg_label
         , mark_type_with_modes ~loc arg_modes arg_type
         , mark_type_with_modes ~loc result_modes result_type ))
  ;;

  let tarrow ~loc args result =
    match args with
    | [] ->
      raise
        (Invalid_argument
           "tarrow: Can't construct a 0-ary arrow, argument list must be nonempty")
    | _ :: _ ->
      let result_mode_and_type =
        let { result_modes; result_type } = result in
        mark_type_with_modes ~loc result_modes result_type
      in
      List.fold_right
        args
        ~init:result_mode_and_type
        ~f:(fun { arg_label; arg_modes; arg_type } arrow_type ->
          let arg_type = mark_type_with_modes ~loc arg_modes arg_type in
          core_type ~loc (Ptyp_arrow (arg_label, arg_type, arrow_type)))
  ;;

  let tarrow_maybe ~loc args result_type =
    match args with
    | [] -> result_type
    | _ :: _ -> tarrow ~loc args { result_modes = []; result_type }
  ;;

  let get_modes ty =
    let modes, ptyp_attributes = Jane_syntax.Mode_expr.of_attrs ty.ptyp_attributes in
    let modes =
      List.map
        (modes.txt : Jane_syntax.Mode_expr.Const.t list :> _ Location.loc list)
        ~f:(fun { txt = name; _ } -> Mode name)
    in
    modes, { ty with ptyp_attributes }
  ;;

  let pcstr_tuple ~loc modalities_tys =
    Pcstr_tuple
      (List.map modalities_tys ~f:(fun (modalities, type_) ->
         Shim.Pcstr_tuple_arg.create ~loc ~modalities ~type_))
  ;;

  let pcstr_tuple_no_modalities tys =
    Pcstr_tuple
      (List.map tys ~f:(fun type_ ->
         Shim.Pcstr_tuple_arg.create ~loc:type_.ptyp_loc ~modalities:[] ~type_))
  ;;

  let get_tuple_field_modalities = Shim.Pcstr_tuple_arg.extract_modalities
  let get_label_declaration_modalities = Shim.Label_declaration.extract_modalities
  let label_declaration = Shim.Label_declaration.create
  let get_value_description_modalities = Shim.Value_description.extract_modalities
  let value_description = Shim.Value_description.create
  let pcstr_tuple_arg = Shim.Pcstr_tuple_arg.create

  let pexp_function ~loc ~attrs ~params ~constraint_ ~body =
    { pexp_desc = Shim.Pexp_function.to_parsetree ~params ~constraint_ ~body
    ; pexp_loc = loc
    ; pexp_attributes = attrs
    ; pexp_loc_stack = []
    }
  ;;

  let unary_function ~loc ?(attrs = []) cases =
    pexp_function
      ~attrs
      ~params:[]
      ~constraint_:None
      ~body:(Pfunction_cases (cases, loc, []))
      ~loc
  ;;

  let fun_param ~loc arg_label pattern : function_param =
    { pparam_desc = Pparam_val (arg_label, None, pattern); pparam_loc = loc }
  ;;

  let function_constraint type_constraint : function_constraint =
    { type_constraint = Pconstraint type_constraint
    ; mode_annotations = { loc = Location.none; txt = [] }
    }
  ;;

  let maybe_constrain body return_constraint ~loc =
    match return_constraint with
    | None -> body
    | Some return_constraint ->
      Ppxlib_ast.Ast_helper.Exp.constraint_ ~loc body return_constraint
  ;;

  let add_fun_params ~loc ?(attrs = []) ?return_constraint new_params body =
    match new_params with
    | [] -> maybe_constrain body return_constraint ~loc
    | _ :: _ ->
      (* If the body is already a function, extend its arity rather than creating a new
         function.
      *)
      (match Shim.Pexp_function.of_parsetree body.pexp_desc ~loc:body.pexp_loc with
       | Some (params, constraint_, function_body) ->
         let existing_attrs =
           List.filter body.pexp_attributes ~f:(fun attr ->
             (* We drop "merlin.loc" attributes inserted by merlin's parser.

                These attributes are always fine to drop -- they're a best-effort attempt
                to encode extra location information -- and usually not fine to move.
                That's because merlin expects certain invariants to hold between
                the location encoded by the "merlin.loc" and locations of sub-ASTs.
             *)
             String.( <> ) attr.attr_name.txt "merlin.loc")
         in
         let fun_ =
           pexp_function
             ~params:(new_params @ params)
             ~constraint_
             ~body:function_body
             ~loc
             ~attrs:(existing_attrs @ attrs)
         in
         maybe_constrain fun_ return_constraint ~loc
       | None ->
         pexp_function
           ~params:new_params
           ~constraint_:(Option.map return_constraint ~f:function_constraint)
           ~body:(Pfunction_body body)
           ~loc
           ~attrs)
  ;;

  let add_fun_param ~loc ?attrs ?return_constraint lbl def pat body =
    add_fun_params
      ?attrs
      ?return_constraint
      ~loc
      [ { pparam_desc = Pparam_val (lbl, def, pat); pparam_loc = pat.ppat_loc } ]
      body
  ;;

  let coalesce_fun_arity ast =
    match Shim.Pexp_function.of_parsetree ast.pexp_desc ~loc:ast.pexp_loc with
    | None | Some (_, Some _, _) | Some (_, _, Pfunction_cases _) -> ast
    | Some (params1, None, Pfunction_body ({ pexp_attributes = []; _ } as outer_body)) ->
      (match
         Shim.Pexp_function.of_parsetree outer_body.pexp_desc ~loc:outer_body.pexp_loc
       with
       | Some (params2, constraint_, inner_body) ->
         pexp_function
           ~params:(params1 @ params2)
           ~constraint_
           ~body:inner_body
           ~loc:ast.pexp_loc
           ~attrs:ast.pexp_attributes
       | None -> ast)
    | Some _ -> ast
  ;;

  let eabstract ~loc ?(coalesce_fun_arity = true) ?return_constraint pats body =
    let params = List.map pats ~f:(fun pat -> fun_param ~loc:pat.ppat_loc Nolabel pat) in
    if coalesce_fun_arity
    then add_fun_params ~loc ?return_constraint params body
    else
      pexp_function
        ~loc
        ~params
        ~constraint_:(Option.map return_constraint ~f:function_constraint)
        ~body:(Pfunction_body body)
        ~attrs:[]
  ;;

  module Latest = struct
    let pexp_function ~loc ?(attrs = []) params constraint_ body =
      pexp_function ~loc ~attrs ~params ~constraint_ ~body
    ;;
  end

  let lexp_constant ~loc c =
    Jane_syntax.Expression.expr_of ~loc ~attrs:[] (Jexp_layout (Lexp_constant c))
  ;;

  let lpat_constant ~loc c =
    Jane_syntax.Pattern.pat_of ~loc ~attrs:[] (Jpat_layout (Lpat_constant c))
  ;;

  let eint64_u ~loc i = lexp_constant ~loc (Integer (Int64.to_string i, 'L'))
  let eint32_u ~loc i = lexp_constant ~loc (Integer (Int32.to_string i, 'l'))
  let enativeint_u ~loc i = lexp_constant ~loc (Integer (Nativeint.to_string i, 'n'))
  let efloat_u ~loc f = lexp_constant ~loc (Float (Float.to_string f, None))
  let pint64_u ~loc i = lpat_constant ~loc (Integer (Int64.to_string i, 'L'))
  let pint32_u ~loc i = lpat_constant ~loc (Integer (Int32.to_string i, 'l'))
  let pnativeint_u ~loc i = lpat_constant ~loc (Integer (Nativeint.to_string i, 'n'))
  let pfloat_u ~loc f = lpat_constant ~loc (Float (Float.to_string f, None))
end

module Make (Loc : sig
    val loc : Location.t
  end) =
struct
  include Default

  let loc = Loc.loc
  let ptyp_arrow arg res : core_type = ptyp_arrow ~loc arg res
  let tarrow args res : core_type = tarrow ~loc args res
  let tarrow_maybe args res : core_type = tarrow_maybe ~loc args res
  let pcstr_tuple fields : constructor_arguments = pcstr_tuple ~loc fields

  let label_declaration ~name ~mutable_ ~modalities ~type_ : label_declaration =
    label_declaration ~loc ~name ~mutable_ ~modalities ~type_
  ;;

  let value_description ~name ~type_ ~modalities ~prim : value_description =
    value_description ~loc ~name ~type_ ~modalities ~prim
  ;;

  let pcstr_tuple_arg ~modalities ~type_ : Pcstr_tuple_arg.t =
    pcstr_tuple_arg ~loc ~modalities ~type_
  ;;

  module Latest = struct
    let pexp_function ?attrs a b c : expression = Latest.pexp_function ~loc ?attrs a b c
  end

  let eabstract ?coalesce_fun_arity ?return_constraint a b : expression =
    eabstract ~loc ?coalesce_fun_arity ?return_constraint a b
  ;;

  let fun_param a b : function_param = fun_param ~loc a b
  let unary_function ?attrs a : expression = unary_function ~loc ?attrs a

  let add_fun_param ?attrs ?return_constraint a b c d : expression =
    add_fun_param ~loc ?attrs ?return_constraint a b c d
  ;;

  let add_fun_params ?attrs ?return_constraint a b : expression =
    add_fun_params ~loc ?attrs ?return_constraint a b
  ;;

  let eint64_u c : expression = eint64_u ~loc c
  let eint32_u c : expression = eint32_u ~loc c
  let enativeint_u c : expression = enativeint_u ~loc c
  let efloat_u c : expression = efloat_u ~loc c
  let pint64_u c : pattern = pint64_u ~loc c
  let pint32_u c : pattern = pint32_u ~loc c
  let pnativeint_u c : pattern = pnativeint_u ~loc c
  let pfloat_u c : pattern = pfloat_u ~loc c
end

let make loc : (module S_with_implicit_loc) =
  (module Make (struct
      let loc = loc
    end))
;;
