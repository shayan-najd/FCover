-module(expression).
-compile(export_all).
-import(literal,[transL/1]).
-import(ast,[encloseData/3,encloseDataNum/4,transField/1,transA/1,newProgramPoint/1]).
-import(pattern,[transP/2]).
-import(clause,[transFcs/2,transCatches/2,transIcs/2,transCcs/2,logPsGs/6,logGs/4]).
-import(typespecifier,[transTSL/2]).
-import(generator,[transW/2]).

transB(Lst,_Name) when is_list(Lst) ->
    lists:map((fun(E)-> transE(E,_Name) end),Lst).

transE(Ast={Lit,_,_},_Name)
  when Lit== integer;Lit== string;Lit==float;Lit== atom ->
   transL(Ast);
transE({match,LINE,P,E},_Name) ->
    encloseData({match, LINE,transP(P,_Name),transE(E,_Name)},_Name,LINE);
transE(Ast = {var,_,_},_Name) ->
    Ast;
transE({tuple,LINE,Lst},_Name) when is_list(Lst) ->
    {tuple,LINE,lists:map(fun(E)->transE(E,_Name)end,Lst)};
transE(Ast = {nil,_},_Name) ->
    Ast;
transE({cons,LINE,E_h, E_t},_Name) ->
    encloseData({cons,LINE,transE(E_h,_Name),transE(E_t,_Name)},_Name,LINE);
transE({bin,LINE,Lst },_Name) when is_list(Lst) ->
    encloseData({bin,LINE,lists:map(fun(BinElement)-> transEBinElement(BinElement,_Name)end,Lst)},_Name,LINE);
transE({op,LINE,Op,E_1,E_2},_Name) ->
    encloseData( {op,LINE,Op,transE(E_1,_Name),transE(E_2,_Name)},_Name,LINE);
transE({op,LINE,Op,E_0},_Name) ->
    encloseData({op,LINE,Op,transE(E_0,_Name)},_Name,LINE);
transE({record,LINE,Name,Lst},_Name) when is_list(Lst)->
    {record,LINE,Name
     ,lists:map(fun(RecordField)->transERecordField(RecordField,_Name)end,Lst)};
transE({record,LINE,E_0,Name, Lst},_Name) when is_list(Lst) ->
    encloseData( {record,LINE,transE(E_0,_Name),Name
		  ,lists:map(fun(RecordField)->transERecordField(RecordField,_Name)end,Lst)},_Name,LINE);
transE({record_index,LINE,Name,Field},_Name) -> %-- transfield?
    {record_index,LINE,Name,transField(Field)};
transE({record_field,LINE,E_0,Name,Field},_Name)-> %-- transfield?
    encloseData({record_field,LINE,transE(E_0,_Name),Name,transField(Field)},_Name,LINE);
transE({'catch',LINE,E_0},_Name) ->
    {'catch',LINE,transE(E_0,_Name)};
transE({call,LINE,{remote,LINE,E_m,E_0}, Lst},_Name)when is_list(Lst) ->
 %-- reorder because of normal call
     encloseData({call,LINE,{remote,LINE,transE(E_m,_Name),transE(E_0,_Name)}
     ,lists:map(fun(E)->transE(E,_Name)end,Lst)},_Name,LINE);
transE({call,LINE,E_0, Lst},_Name)when is_list(Lst)->
    {call,LINE,transE(E_0,_Name), lists:map(fun(E)->transE(E,_Name)end,Lst)};
transE({lc,LINE,E_0, Lst = [_H|_T]},_Name) ->
    encloseData( {lc,LINE,transE(E_0,_Name),lists:map(fun(W)->transW(W,_Name)end,Lst)},_Name,LINE);
transE({bc,LINE,E_0, Lst = [_H|_T]},_Name) ->
    encloseData( {bc,LINE,transE(E_0,_Name),lists:map(fun(W)->transW(W,_Name)end,Lst)},_Name,LINE);
transE({block,LINE,B},_Name) ->
    {block,LINE,transB(B,_Name)};
%------------------------
transE({'if',LINE,Ics = [_H|_T]},_Name) ->
    Num=newProgramPoint(_Name),
    encloseData( 
      {block,LINE,
       [logGs(GsP,Num,_Name,_Line)|| {clause,_Line,[],GsP,_B} <- Ics]
       ++ [{'if',LINE, transIcs(Ics,_Name)}]
      },_Name,LINE);
%------------------------
transE(Exp={'case',LINE,_E_0, _Ccs = [_H|_T]},_Name) ->
    Case = transCase(Exp,_Name),
    encloseData(Case,_Name,LINE);
%------------------------
transE({'try',LINE,B,[],[],A},_Name) -> %try B  after A end
    {'try',LINE,transB(B,_Name),[],[],transA(A)};
transE({'try',LINE,B,[],Tcs=[_H|_T],[]},_Name) -> %try B  catch Tc1;... Tcn; end
    {'try',LINE,transB(B,_Name),[],transCatches(Tcs,_Name),[]};
transE({'try',LINE,B,[],Tcs=[_H|_T],A},_Name)-> %try B catch Tc1;... Tcn; after A end
    {'try',LINE,transB(B,_Name),[],transCatches(Tcs,_Name),transA(A)};
%------
transE({'try',LINE,B,Ccs=[_H|_T],[],A},_Name) when A/= []-> % try B of Ccs after A end
    Pp = newProgramPoint(_Name),
    Var1 = list_to_atom("Xunique"++integer_to_list(Pp)),
    Case = transE({'case',LINE,{block,LINE,B},Ccs
		++[{clause,LINE,[{var,LINE,Var1}],[],[
						      {call,LINE,
						       {remote,LINE,{atom,LINE,erlang},{atom,LINE,error}},
						       [{tuple,LINE,[{atom,LINE,try_clause},{var,LINE,Var1}]}]
						      }
						     ]}]},_Name),
    {'try',LINE,[Case],[],[],transA(A)};

transE({'try',LINE,B,Ccs=[_H|_T],Tcs=[_H|_T],A},_Name) when is_list(Tcs)->
    Pp1 = newProgramPoint(_Name),
    Et={'try',LINE,[{tuple,LINE,[{atom,LINE,ok},{block,LINE,B}]} ],[]
	,transCatches([
		       {clause,LineB,PsB,GsB,[{tuple,LineB,[{atom,LineB,error},{block,LineB,BB}]}]}
		       || {clause,LineB,PsB,GsB,BB} <- Tcs ],_Name),[]},
    Var1=list_to_atom("Tvar1_"++integer_to_list(Pp1)),
    Var2=list_to_atom("Tvar2_"++integer_to_list(Pp1)),
    Var3=list_to_atom("Tvar3_"++integer_to_list(Pp1)),
    TCase=encloseDataNum(transCase({'case',LINE,{var,LINE,Var1},Ccs ++[{clause,LINE,[{var,LINE,Var3}],[]
						      ,[{call,LINE,
							 {remote,LINE,{atom,LINE,erlang},{atom,LINE,error}},
							 [{tuple,LINE,[{atom,LINE,try_clause},{var,LINE,Var3}]}]
							}]}]},_Name),_Name,LINE,Pp1),
    Case={'case',LINE,Et,[
			  {clause,LINE,[
					{tuple,LINE,[
						     {atom,LINE,ok},{var,LINE,Var1}
						    ]
					}
				       ],[],[TCase]}
			  ,{clause,LINE,[
					 {tuple,LINE,[
						      {atom,LINE,error},{var,LINE,Var2}
						     ]
					 }
					],[],[{var,LINE,Var2}]
			   }
			 ]},
    if A==[]->Case;
       true -> {'try',LINE,[Case],[],[],transA(A)} 
    end;  
transE({'receive',LINE, Ccs = [_H|_T]} ,_Name) ->
    {'receive',LINE,transCcs(Ccs,_Name)};
transE({'receive',LINE,Ccs = [_H|_T],E_0, B_t},_Name) ->
    encloseData({'receive',LINE, transCcs(Ccs,_Name),transE(E_0,_Name)
		 ,transB(B_t,_Name)},_Name,LINE);
transE(Ast = {'fun',_,{function,_,_}},_Name ) ->
    Ast;
transE(Ast = {'fun',_,{function,_,_,_}},_Name) ->
    Ast;
transE({'fun',LINE,{clauses,Fcs = [_H|_T]}},_Name) ->
    {'fun',LINE,{clauses,transFcs(Fcs,_Name)}};
transE({'query',_,_ },_Name)->
    exit(notdefined);  % old syntax
transE({record_field,_LINE,_E_0,_Field},_Name)->
    exit(notdefined). %mnesia old syntax

transEBinElement({bin_element,LINE,V,Size,TSL},_Name) -> 
% not sure about transE(V and transE(Size
    {bin_element,LINE,transE(V,_Name),transE(Size,_Name),transTSL(TSL,_Name)}.
    
transERecordField({record_field,LINE,Field,E},_Name)-> %not sure about transfield
    {record_field,LINE,transField(Field),transE(E,_Name)}.

transCase({'case',LINE,E_0, Ccs = [_H|_T]},_Name)->
    Num=newProgramPoint(_Name),
    Var=list_to_atom("ExpUnique_"++integer_to_list(Num)),
    ME={match,LINE,{var,LINE,Var},transE(E_0,_Name)},
    Logs=[logPsGs(Var,PsP,GsP,Num,_Name,_Line)
	  || {clause,_Line,PsP,GsP,_B} <- Ccs
		 ,not(PsP ==[{var,element(2,PsP),'_'}]
		      andalso GsP == [])%no _ -> clause
	 ], 
    Case={'case',LINE,{var,LINE,Var},transCcs(Ccs,_Name)},  
    {block,LINE,[ME]++ Logs++[Case]}.

%io:format("~p~n",[lists:nth(1,Logs)]),   
%   ,encloseData({'try',LINE,transB(B,_Name),transCcs(Ccs,_Name)
%		, if Tcs==[]-> [];
%		     true -> transCatches(Tcs,_Name)
%		  end
%		,if A==[]->[];
%		    true -> transA(A)
%		 end},_Name,LINE);    
%transE({'try',LINE,B,Ccs=[_H|_T],[],A},_Name)-> %try B of Cc1;... Ccn; after A end
%    CatchC={clause,1,
%	    [{tuple,1,
%	      [{atom,1,error},
%	       {match,1,
%		{var,1,'M'},
%		{tuple,1,[{atom,1,case_clause},{var,1,'_'}]}},
%	       {var,1,'_'}]}],
%	    [],
%	    [{call,1,
%	      {remote,1,{atom,1,erlang},{atom,1,error}},
%	      [{var,1,'M'}]}]},
%    Case=transCase({'case',LINE,{block,LINE,B},Ccs},_Name),
%    encloseData({'try',LINE,Case,[],[CatchC],transA(A)},_Name,LINE) ;
%transE({'try',LINE,B,Ccs=[_H|_T],Tcs=[_H|_T],[]},_Name)-> %try B of Cc1;... Ccn; catch Tc1;... Tcn;end 
%    encloseData(  {'try',LINE,transB(B,_Name),transCcs(Ccs,_Name),transCatches(Tcs,_Name),[]},_Name,LINE);
%transE({'try',LINE,B,Ccs=[_H|_T],Tcs=[_H|_T],A},_Name)-> %try B of Cc1;... Ccn; catch Tc1;... Tcn; after A end
%    encloseData( {'try',LINE,transB(B,_Name),transCcs(Ccs,_Name),transCatches(Tcs,_Name),transA(A)},_Name,LINE);
%------------------------
