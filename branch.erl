-module(branch).
-compile(export_all).
-import(clause,[transFcs/2,transIcs/2,transCcs/2,transCts/2,transRcs/2]).
-import(ast,[newProgramPoint/1,transA/1]).
-import(guard,[transGs/2]).
-import(pattern,[transPs/2]).
-import(expression,[transE/2,transB/2]).
-import(mcdcLib,[logPsGs/6,logGs/4]).
%------------------------------------------------------------------
% transforms Function Clause
%------------------------------------------------------------------
transFunction([{clause,CLine,[],[],B}],_Name) ->    
    counter!{self(),_Name,0,CLine,{}},
    [{clause,-(abs(CLine) bor 2)
     ,[]
     ,[]
     ,[ast:logData(_Name,CLine,funcEntry)| transB(B,_Name)]}];
transFunction(Fcs=[_H={clause,CLine,CPs,_,_}|_T],_Name) ->    
    counter!{self(),_Name,0,CLine,{}},
    Vars=[ {var,-14,list_to_atom("XUnique"++integer_to_list(I))} 
	   || I <- lists:seq(1,length(CPs))],
    Clauses= transFcs(Fcs,_Name)	
	++ [{clause,-14,
	     [{var,-14,'_'}],
	     [],
	     [{call,-14,
	       {remote,-14,{atom,-14,erlang},{atom,-14,error}},
	       [ {atom,-14,function_clause} ] }]}],
    Case={'case',-2,{tuple,CLine,Vars},Clauses}, 
    CaseP=transE(Case,_Name),
   [ {clause,CLine,Vars,[],[CaseP]}]. %casep
%------------------------------------------------------------------
%
%------------------------------------------------------------------
transIf(Exp={'if',L,_Ics = [_H|_T]},_Name) when L=<0, (abs(L) bor 8)== abs(L) ->
    Exp;
transIf({'if',_LINE,Ics = [_H|_T]},_Name)->
    Num=newProgramPoint(_Name),
    {block,-10,
     [logGs(transGs(GsP,_Name),Num,_Name,_Line)|| {clause,_Line,[],GsP,_B} <- Ics]
     ++ [{'if',-10, transIcs(Ics,_Name)}]
    }.
%------------------------------------------------------------------
%
%------------------------------------------------------------------
transCase(Exp={'case',L,_E_0, _Ccs = [_H|_T]},_Name)when L=<0, (abs(L) bor 8)== abs(L)->
    Exp;
transCase({'case',_LINE,E_0, Ccs = [_H|_T]},_Name)->
  Num=newProgramPoint(_Name),
    Var=list_to_atom("ExpUnique_"++integer_to_list(Num)),
    ME={match,-10,{var,-10,Var},transE(E_0,_Name)},
    Logs=[logPsGs(Var,transPs(PsP,_Name),transGs(GsP,_Name),Num,_Name,_Line)
	  || {clause,_Line,PsP,GsP,_B} <- Ccs
		 ,not(case PsP of 
			  [{var,_,'_'}] when GsP==[]-> true; 
			  _-> false end)% no _ -> clause
	 ],     
    Case={'case',-10,{var,-10,Var},transCcs(Ccs,_Name)},  
    {block,-10,[ME]++ Logs++[Case]}.
%------------------------------------------------------------------
%
%------------------------------------------------------------------
transCatches(Cs=[_H|_T],_Name)->
    Pp=newProgramPoint(_Name),
    Var=list_to_atom("XUnique_"++integer_to_list(Pp)),
[{clause,-14
	,[{tuple,-14,[{atom,-14,error},{var,-14,Var},{var,-14,'_'}]}]
	,[]
	,[ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
	   || {clause,Linep,Ps,Gs,_} <-Cs] 
	++
	[{'try',-14,[{call,-14,
		      {remote,-14,{atom,-14,erlang},{atom,-14,error}},
		      [{var,-14, Var} ] }],[],transCts(Cs,_Name),[]}
	]}
       ,{clause,-14,[{tuple,-14,[{atom,-14,throw},{var,-14,Var},{var,-14,'_'}]}],[],
	 [ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
	   || {clause,Linep,Ps,Gs,_} <-Cs] 
	 ++
	 [{'try',-14,[{call,-14,
		       {atom,-14,throw},
		       [{var,-14, Var} ] }],[],transCts(Cs,_Name),[]}
	 ] }
       ,{clause,-14,[{tuple,-14,[{atom,-14,exit},{var,-14,Var},{var,-14,'_'}]}],[]
	 ,[ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
	    || {clause,Linep,Ps,Gs,_} <-Cs] 
	 ++
	 [{'try',-14,[{call,-14,
		       {atom,-14,exit},
		       [{var,-14, Var} ] }],[],transCts(Cs,_Name),[]}
	 ]}].
    %% [{clause,-14,[{error,{var,-14,Var}}],[]
    %%   ,[ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
    %% 	 || {clause,Linep,Ps,Gs,_} <-Cs] 
    %%   ++
    %%   [{'try',-14,[{call,-14,
    %% 		     {remote,-14,{atom,-14,erlang},{atom,-14,error}},
    %% 		     [{var,-14, Var} ] }],[],transCts(Cs,_Name),[]}
    %%   ]}
    %%  ,{clause,-14,[{throw,{var,-14,Var}}],[],
    %%    [ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
    %% 	 || {clause,Linep,Ps,Gs,_} <-Cs] 
    %%    ++
    %%    [{'try',-14,[{call,-14,
    %% 		       {atom,-14,throw},
    %% 		      [{var,-14, Var} ] }],[],transCts(Cs,_Name),[]}
    %%    ] }
    %%  ,{clause,-14,[{exit,{var,-14,Var}}],[]
    %%    ,[ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
    %% 	  || {clause,Linep,Ps,Gs,_} <-Cs] 
    %%    ++
    %%    [{'try',-14,[{call,-14,
    %% 		      {atom,-14,exit},
    %% 		      [{var,-14, Var} ] }],[],transCts(Cs,_Name),[]}
    %%    ]}].
%------------------------------------------------------------------
%
%------------------------------------------------------------------
transTry(Exp={'try',L,_,_,_,_},_Name) when element(2,Exp)=<0, (abs(L) bor 8)== abs(L)  -> 
    Exp;
transTry({'try',_LINE,B,[],[],A},_Name) ->
 %try B  after A end
    {'try',-12,transB(B,_Name),[],[],transA(A)};
transTry({'try',_LINE,B,[],Tcs=[_H|_T],[]},_Name) ->
 %try B  catch Tc1;... Tcn; end
    {'try',-12,transB(B,_Name),[],transCatches(Tcs,_Name),[]};
transTry({'try',_LINE,B,[],Tcs=[_H|_T],A},_Name)->
 %try B catch Tc1;... Tcn; after A end
    {'try',-12,transB(B,_Name),[],transCatches(Tcs,_Name),transA(A)};
%transTry({'try',LINE,B,Ccs=[_H|_T],[],A},_Name) when A/= []-> % try B of Ccs after A end
%    Pp = newProgramPoint(_Name),
%    Var1 = list_to_atom("Xunique"++integer_to_list(Pp)),
%    Case = transE({'case',LINE,{block,LINE,B},Ccs
%		++[{clause,LINE,[{var,LINE,Var1}],[],[
%						      {call,LINE,
%						       {remote,LINE,{atom,LINE,erlang},{atom,LINE,error}},
%						       [{tuple,LINE,[{atom,LINE,try_clause},{var,LINE,Var1}]}]
%						      }
%						     ]}]},_Name),
%   {'try',LINE,[Case],[],[],transA(A)};
%------------------------------------------------
transTry({'try',LINE,B,Ccs=[_H|_T],Tcs,A},_Name) when  is_list(Tcs)->
    Ec={tuple,LINE,[{atom,LINE,ok},{block,LINE,transB(B,_Name)}]},
    Et= if 
	    Tcs == [] -> Ec;
	    true -> transE({'try',-10,[Ec],[]
			    ,transCatches([{clause,LineB,transPs(PsB,_Name),transGs(GsB,_Name)
					    ,[{tuple,LineB,[{atom,LineB,error},{block,LineB,BB}]}]}
					   || {clause,LineB,PsB,GsB,BB} <- Tcs ],_Name),[]} ,_Name)
	end,
    Pp1=newProgramPoint(_Name),
    Var1=list_to_atom("Tvar1_"++integer_to_list(Pp1)),
    Var2=list_to_atom("Tvar2_"++integer_to_list(Pp1)),
    Var3=list_to_atom("Tvar3_"++integer_to_list(Pp1)),
    TCase= transE({'case',LINE,{var,LINE,Var1},
		   Ccs ++[{clause,-14,[{var,LINE,Var3}],[]
			   ,[ {call,LINE
			       ,{remote,LINE,{atom,LINE,erlang},{atom,LINE,error}}
			       ,[{tuple,LINE,[{atom,LINE,try_clause},{var,LINE,Var3}]}]
			      }
			    ]
			  }
			 ]
		  },_Name),
    Case={'case',-10,Et,[
			 {clause,-10,[ {tuple,LINE,[{atom,LINE,ok},{var,LINE,Var1}]}],[],[TCase]}
			 ,{clause,-10,[ {tuple,LINE,[{atom,LINE,error},{var,LINE,Var2}]}],[],[{var,LINE,Var2}]}
			]},
    if A==[]->Case;
       true -> {'try',-10,[Case],[],[],transA(A)} 
    end.
%------------------------------------------------------------------
% Transform Receive
%------------------------------------------------------------------
transReceive({'receive',LINE, Ccs = [_H|_T]} ,_Name) ->
    {'receive',LINE,transRcs(Ccs,_Name)};
transReceive({'receive',LINE,Ccs = [_H|_T],E_0, B_t},_Name) ->
    {'receive',LINE, transRcs(Ccs,_Name),transE(E_0,_Name)
		 ,transB(B_t,_Name)}.
