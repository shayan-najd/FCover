-module(literal).
-compile(export_all).

transL(Ast={Lit,_,_}) when Lit== integer;Lit== string;Lit==float;Lit== atom ->
    Ast.
