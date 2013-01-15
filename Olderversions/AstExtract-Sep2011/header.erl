-module(header).
-compile(export_all).
-import(recordfield,[transV/1]).
-import(clause,[transFcs/2]).

transF({attribute,Line,record,{Name, Lst}}) when is_list(Lst)->
    {attribute,Line,record, {Name,
			     lists:map(fun(V)->transV(V)end,Lst)}}; 
transF(Ast = {attribute,_,_,_}) -> 
    Ast;  
transF({function,LINE,Name,Arity,Fcs = [_H|_T]}) ->
    {function,LINE,Name,Arity,transFcs(Fcs,Name)}.
