module T := Ppxlib_traverse_builtins.T

class ['ctx] map_with_context : object
  inherit ['ctx] Ppxlib_traverse_builtins.map_with_context
  inherit ['ctx] Ppxlib_ast.Ast.map_with_context
  method const_jkind : ('ctx, Jane_syntax.Jkind.Const.t) T.map_with_context
  method mode : ('ctx, Shim.Mode.t) T.map_with_context
  method modes : ('ctx, Shim.Modes.t) T.map_with_context
  method jkind : ('ctx, Jane_syntax.Jkind.t) T.map_with_context
  method unboxed_constant : ('ctx, Jane_syntax.Layouts.constant) T.map_with_context

  method clause_binding :
    ('ctx, Jane_syntax.Comprehensions.clause_binding) T.map_with_context

  method clause : ('ctx, Jane_syntax.Comprehensions.clause) T.map_with_context

  method comprehension :
    ('ctx, Jane_syntax.Comprehensions.comprehension) T.map_with_context

  method function_param : ('ctx, Shim.Pexp_function.function_param) T.map_with_context
  method function_body : ('ctx, Shim.Pexp_function.function_body) T.map_with_context

  method function_constraint :
    ('ctx, Shim.Pexp_function.function_constraint) T.map_with_context
end
