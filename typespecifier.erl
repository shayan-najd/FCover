-module(typespecifier).
-compile(export_all).


transTSL(TSs=[_H|_T]) -> %Tss can be empty? 
    lists:map(fun(TS)-> transTS(TS) end,TSs).

transTS(A)when is_atom(A) ->
    A;
transTS({A,Value})when is_atom(A),is_integer(Value) ->
    {A,Value}.
