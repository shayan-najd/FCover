-module (a).
-compile (export_all).
ast(Input) ->
    {ok,Tkns,_}= erl_scan:string(Input),
    Ss = spliter(Tkns),
    [Ast || {ok,Ast} <-(lists:map(fun erl_parse:parse_form/1,Ss))].
 
spliter(Input) ->
    spliter(Input,[],[]).
spliter([H],A,P) ->
    A++[P++[H]];
spliter([{dot,R}|T],A,P) ->
    spliter(T,A++[P++[{dot,R}]],[]);
spliter([H|T],A,P)-> 
    spliter(T,A,P++[H]).
