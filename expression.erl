-module(expression).
-compile(export_all).
-import(literal,[transL/1]).
-import(ast,[encloseData/2,transField/1,transA/1]).
-import(pattern,[transP/2]).
-import(branch,[transFunction/2,transIf/2,transCase/2,transCatches/2,transTry/2
		,transReceive/2]).
-import(typespecifier,[transTSL/2]).
-import(generator,[transW/2]).

transB(Lst,_Name) when is_list(Lst) ->
    lists:map((fun(E)-> transE(E,_Name) end),Lst).

%transE(Exp,_Name) when element(2,Exp)=<0 ->
%    Exp; % don't process furthur if it is generated code
transE(Ast={Lit,_,_},_Name)
  when Lit== integer;Lit== string;Lit==float;Lit== atom ->
   transL(Ast);
transE({match,LINE,P,E},_Name) ->
    Match={match, LINE,transP(P,_Name),transE(E,_Name)},
    encloseData(Match,_Name);
transE(Ast = {var,_,_},_Name) ->
    Ast;
transE({tuple,LINE,Lst},_Name) when is_list(Lst) ->
    {tuple,LINE,lists:map(fun(E)->transE(E,_Name)end,Lst)};
transE(Ast = {nil,_},_Name) ->
    Ast;
transE({cons,LINE,E_h, E_t},_Name) ->
    Cons={cons,LINE,transE(E_h,_Name),transE(E_t,_Name)},
    encloseData(Cons,_Name);
transE({bin,LINE,Lst },_Name) when is_list(Lst) ->
    Bin={bin,LINE,lists:map(fun(BinElement)-> transEBinElement(BinElement,_Name)end
			    ,Lst)},
    encloseData(Bin,_Name);
transE({op,LINE,Op,E_1,E_2},_Name) ->
    OpV={op,LINE,Op,transE(E_1,_Name),transE(E_2,_Name)},
    encloseData(OpV ,_Name);
transE({op,LINE,Op,E_0},_Name) ->
    OpV={op,LINE,Op,transE(E_0,_Name)},
    encloseData(OpV,_Name);
transE({record,LINE,Name,Lst},_Name) when is_list(Lst)->
    {record,LINE,Name
     ,lists:map(fun(RecordField)->transERecordField(RecordField,_Name)end,Lst)};
transE({record,LINE,E_0,Name, Lst},_Name) when is_list(Lst) ->
    Record={record,LINE,transE(E_0,_Name),Name
	   ,lists:map(fun(RecordField)->transERecordField(RecordField,_Name)end,Lst)},
    encloseData(Record ,_Name);
transE({record_index,LINE,Name,Field},_Name) -> %-- transfield?
    {record_index,LINE,Name,transField(Field)};
transE({record_field,LINE,E_0,Name,Field},_Name)-> %-- transfield?
    RecordField={record_field,LINE,transE(E_0,_Name),Name,transField(Field)},
    encloseData(RecordField,_Name);
transE({'catch',LINE,E_0},_Name) ->
    {'catch',LINE,transE(E_0,_Name)};
transE({call,LINE1,{remote,LINE,E_m,E_0}, Lst},_Name)when is_list(Lst) ->
 %-- reorder because of normal call
    Call={call,LINE1,{remote,LINE,transE(E_m,_Name),transE(E_0,_Name)}
	  ,lists:map(fun(E)->transE(E,_Name)end,Lst)},
     encloseData(Call,_Name);
transE({call,LINE,E_0, Lst},_Name)when is_list(Lst)->
    {call,LINE,transE(E_0,_Name), lists:map(fun(E)->transE(E,_Name)end,Lst)};
transE({lc,LINE,E_0, Lst = [_H|_T]},_Name) ->
    Lc={lc,LINE,transE(E_0,_Name),lists:map(fun(W)->transW(W,_Name)end,Lst)},
    encloseData(Lc,_Name);
transE({bc,LINE,E_0, Lst = [_H|_T]},_Name) ->
    Bc={bc,LINE,transE(E_0,_Name),lists:map(fun(W)->transW(W,_Name)end,Lst)},
    encloseData(Bc,_Name);
transE({block,LINE,B},_Name) ->
    {block,LINE,transB(B,_Name)};
%------------------------
transE(Exp={'if',_,_Ics = [_H|_T]},_Name) ->
    If=transIf(Exp,_Name),
    encloseData( If,_Name);
%------------------------
transE(Exp={'case',_,_, _Ccs = [_H|_T]},_Name) ->
    Case = transCase(Exp,_Name),
    encloseData(Case,_Name);
%------------------------
transE(Exp={'try',_,_,_,_,_},_Name) -> 
    Try=transTry(Exp,_Name),
    encloseData(Try,_Name);
%------------------------------------------------
transE(Exp={'receive',_, _Rcs = [_H|_T]},_Name) ->
    transReceive(Exp,_Name);
transE(Exp={'receive',_,_Rcs = [_H|_T],_, _},_Name) ->
    Receive= transReceive(Exp,_Name),
    encloseData(Receive,_Name);
transE(Ast = {'fun',_,{function,_,_}},_Name ) ->
    Ast;
transE(Ast = {'fun',_,{function,_,_,_}},_Name) ->
    Ast;
transE({'fun',LINE,{clauses,Fcs = [_H|_T]}},_Name) ->
    {'fun',LINE,{clauses,transFunction(Fcs,_Name)}};
transE({'query',_,_ },_Name)->
    exit(notdefined);  % old syntax
transE({record_field,_LINE,_E_0,_Field},_Name)->
    exit(notdefined). %mnesia old syntax

transEBinElement({bin_element,LINE,V,Size,TSL},_Name) -> 
% not sure about transE(V and transE(Size
    {bin_element,LINE,transE(V,_Name),transE(Size,_Name),transTSL(TSL,_Name)}.
    
transERecordField({record_field,LINE,Field,E},_Name)-> 
%not sure about transfield
    {record_field,LINE,transField(Field),transE(E,_Name)}.



