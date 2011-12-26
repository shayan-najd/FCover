-module(recordfield).
-compile(export_all).
-import(expression,[transE/2]).
-import(ast,[transA/1]).

transV({record_field,Line,A}) ->
    {record_field,Line,transA(A)};
transV({record_field,Line,A,E}) ->
    {record_field,Line,transA(A),transE(E,'record')}.
