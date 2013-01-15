-module(pattern).
-compile(export_all).
-import(literal,[transL/1]).
-import(ast,[transField/1,transTSL/1]).

transPs(Ast,_Name) when is_list(Ast) -> 
    lists:map((fun(X)-> transP(X,_Name) end),Ast).
transP(Ast={Lit,_,_},_Name)
  when Lit== integer;Lit== string;Lit==float;Lit== atom ->
   transL(Ast);
transP({match,LINE,P_1,P_2},_Name)->
    {match,LINE,transP(P_1,_Name),transP(P_2,_Name)};
transP(Ast = {var,_,_},_Name) ->
    Ast;
transP({tuple,LINE,Lst},_Name) when is_list(Lst) ->
    {tuple,LINE,lists:map(fun(P)->transP(P,_Name)end,Lst)};
transP(Ast = {nil,_},_Name) ->
    Ast; 
transP({cons,LINE,P_h, P_t},_Name) ->
    {cons,LINE,transP(P_h,_Name),transP(P_t,_Name)};
transP({bin,LINE,Lst},_Name) when is_list(Lst) -> %---- why rep(E)
    {bin,LINE,lists:map(fun(BinElement)->transPBinElement(BinElement,_Name)end,Lst)};
transP({op,LINE,Op,P_1,P_2},_Name) ->
    {op,LINE,Op,transP(P_1,_Name),transP(P_2,_Name)};
transP({op,LINE,Op,P_0},_Name) ->
    {op,LINE,Op,transP(P_0,_Name)};
transP({record,LINE,Name,Lst},_Name) when is_list(Lst)->
    {record,LINE,Name,lists:map(fun(RecordField)->transPRecordField(RecordField,_Name)end,Lst)};
transP({record_index,LINE,Name,Field},_Name) -> %-- not sure of transfield
    {record_index,LINE,Name,transField(Field)}.

transPBinElement({bin_element,Line,P,Size,Tsl},_Name)-> %not sure of transSize (TransP(Size...)
    {bin_element,Line,transP(P,_Name),transP(Size,_Name),transTSL(Tsl)}.

transPRecordField({record_field,LINE,Field,P},_Name)->   %-- not sure of transfield
    {record_field,LINE,transField(Field),transP(P,_Name)}.
