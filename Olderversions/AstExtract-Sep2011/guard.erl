-module(guard).
-compile(export_all).
-import(literal,[transL/1]).
-import(ast,[transField/1,transTSL/1,transA/1]).

transGs(Gs,_Name) when is_list(Gs)-> 
    lists:map((fun(G)-> transG(G,_Name) end),Gs).

transG(Gts=[_H|_T],_Name)-> 
    lists:map((fun(Gt)-> transGt(Gt,_Name) end),Gts).

transGt(Ast={Lit,_,_},_Name)
  when Lit== integer;Lit== string;Lit==float;Lit== atom ->
   transL(Ast);
transGt(Ast = {var,_,_},_Name) ->
    Ast;
transGt({tuple,LINE,Lst},_Name) when is_list(Lst) ->
    {tuple,LINE,lists:map(fun(Gt)->transGt(Gt,_Name)end,Lst)};
transGt(Ast = {nil,_},_Name) ->
    Ast; 
transGt({cons,LINE,Gt_h, Gt_t},_Name) ->
    {cons,LINE,transGt(Gt_h,_Name),transGt(Gt_t,_Name)};
transGt({bin,LINE,Lst},_Name) when is_list(Lst) ->
    {bin,LINE,lists:map(fun(BinElement)->transGtBinElement(BinElement,_Name)end,Lst)};
transGt({op,LINE,Op,Gt_1,Gt_2},_Name) ->
    {op,LINE,Op,transGt(Gt_1,_Name),transGt(Gt_2,_Name)};
transGt({op,LINE,Op,Gt_0},_Name) ->
    {op,LINE,Op,transGt(Gt_0,_Name)};
transGt({record,LINE,Name,Lst},_Name) when is_list(Lst)->
    {record,LINE,Name
     ,lists:map(fun(RecordField)->transGtRecordField(RecordField,_Name)end,Lst)};
transGt({record_index,LINE,Name,Field},_Name) ->
    {record_index,LINE,Name,transField(Field)};
transGt({record_field,LINE,Gt_0,Name,Field},_Name)->
    {record_field,LINE,transGt(Gt_0,_Name),Name,transField(Field)};%-- transfield?
transGt({call,LINE,{remote,LINE,Am,A}, Lst },_Name) when is_list(Lst) ->
    {call,LINE,{remote,LINE,transA(Am),transA(A)}
     ,lists:map(fun(Gt)->transGt(Gt,_Name)end,Lst)};
transGt({call,LINE,{tuple,1,[{atom,1,'erlang'},{atom,1,A}]},Lst},_Name)
 when is_list(Lst)->
    {call,LINE,{tuple,1,[{atom,1,'erlang'},{atom,1,transA(A)}]}
     ,lists:map(fun(Gt)->transGt(Gt,_Name)end,Lst) }; %'erlang'
transGt({call,LINE,A, Lst},_Name)when is_list(Lst)->
    {call,LINE,transA(A),lists:map(fun(Gt)->transGt(Gt,_Name)end,Lst) }. %'erlang'

transGtBinElement({bin_element,Line,Gt,Size,Tsl},_Name)->
 %not sure of transSize (TransP(Size...)
    {bin_element,Line,transGt(Gt,_Name),transGt(Size,_Name),transTSL(Tsl)}.

transGtRecordField({record_field,LINE,Field,Gt},_Name)->
   %-- not sure of transfield
    {record_field,LINE,transField(Field),transGt(Gt,_Name)}.


			
