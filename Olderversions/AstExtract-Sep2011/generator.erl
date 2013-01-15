-module(generator).
-compile(export_all).
-import(pattern,[transP/2]).
-import(expression,[transE/2]).
transW({generate,LINE,P,E},_Name) ->
    {generate,LINE,transP(P,_Name),transE(E,_Name)};%-----------?
transW({b_generate,LINE,P,E},_Name) ->
    {b_generate,LINE,transP(P,_Name),transE(E,_Name)};%-----------?
transW(E,_Name) -> % incase of filter
    transE(E,_Name).
