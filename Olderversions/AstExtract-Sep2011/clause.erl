-module(clause).
-compile(export_all).
-import(ast,[logData/3,logDataAST/4,newProgramPoint/1]).
-import(pattern,[transPs/2]).
-import(guard,[transGs/2]).
-import(expression,[transE/2,transB/2]).
-import(mcdc,[nonLogicalsRaw/1,toLogicalGs/1,toLogicalPs/2]).

%---------------------------------------- 
transFcs(Fcs=[_H={clause,Line,Ps,_Gs,_B}|_T],_Name) ->    
    counter!{self(),_Name,0,Line,{}},
    Vars=[ {var,Line,list_to_atom("XUnique"++integer_to_list(I))} 
	   || I <- lists:seq(1,length(Ps))],
    Clauses= [{clause,Line
	       ,[{tuple,Line,transPs(PsP,_Name)}]
	       ,transGs(GsP,_Name)
	       ,[logData(_Name,LineP,funcEntry)| transB(BP,_Name)]}
	      || {clause,LineP,PsP,GsP,BP} <- Fcs ]
	++ [{clause,Line,
                 [{var,Line,'_'}],
                 [],
                 [{call,Line,
                        {remote,Line,{atom,Line,erlang},{atom,Line,error}},
                        [ {atom,Line,function_clause} ] }]}],
    Case={'case',Line,{tuple,Line,Vars},Clauses}, 
    CaseP=transE(Case,_Name),
   [ {clause,Line,Vars,[],[CaseP]}].

transCatches(Cs=[{clause,Line,_,_,_}|_T],_Name)->
    Pp=newProgramPoint(_Name),
    Var=list_to_atom("XUnique_"++integer_to_list(Pp)),
    [{clause,Line,[{error,{var,Line,Var}}],[]
      ,[ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Line)
	 || {clause,_,Ps,Gs,_} <-Cs] 
      ++
      [{'try',Line,[{call,Line,
		     {remote,Line,{atom,Line,erlang},{atom,Line,error}},
		     [{var,Line, Var} ] }],[],Cs,[]}
      ]}
     ,{clause,Line,[{throw,{var,Line,Var}}],[],
       [ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
	 || {clause,Linep,Ps,Gs,_} <-Cs] 
       ++
       [{'try',Line,[{call,Line,
		       {atom,Line,throw},
		      [{var,Line, Var} ] }],[],Cs,[]}
       ] }
     ,{clause,Line,[{exit,{var,Line,Var}}],[]
       ,[ logPsGs(Var,transPs(Ps,_Name),transGs(Gs,_Name),Pp,_Name,Linep)
	  || {clause,Linep,Ps,Gs,_} <-Cs] 
       ++
       [{'try',Line,[{call,Line,
		      {atom,Line,exit},
		      [{var,Line, Var} ] }],[],Cs,[]}
       ]}].

transIcs(Ics=[_H|_T],_Name)->
    [ {clause,Line,[],transGs(Gs,_Name),
       [logData(_Name,Line,clauseEntry)| transB(B,_Name)] } 
      || {clause,Line,[],Gs,B} <-Ics].

transCcs(Ccs=[_H|_T],_Name)->
 [ {clause,Line,transPs(Ps,_Name),transGs(Gs,_Name),
       [logData(_Name,Line,clauseEntry)| transB(B,_Name)] } 
      || {clause,Line,Ps,Gs,B} <-Ccs].


logPsGs(VarName,Ps,Gs,Num,_Name,Line)->
  {'case',Line,{var,Line,VarName},
     [
      {clause,Line,Ps,[],[logDataAST(_Name,Line,mcdcASTPsGs(VarName,Ps,Gs,Line),Num)]}
      ,{clause,Line,[{var,Line,'_'}],[],[logDataAST(_Name,Line
					     ,mcdcASTNotPsGs(VarName,Ps,Line),Num)]}
     ]}.

logGs(Gs,Num,_Name,Line)->
   logDataAST(_Name,Line,mcdcASTGs(Gs,Line),Num).
 
mcdcASTGs(Gs=[_H|_T],Line) ->
    Lg=toLogicalGs(Gs),
    Conditions=sets:to_list(nonLogicalsRaw(Lg)),
    ExpStr= erl_prettypr:format(Lg),	  
    {tuple,Line,[ {string,Line,ExpStr}
		      ,{tuple,Line,Conditions}]}.

mcdcASTNotPsGs(Var,Ps=[_H|_T],Line)->
    Lg=toLogicalPs(Ps,Var),
    PsConditions=sets:to_list(nonLogicalsRaw(Lg)),
    PsStr= "("++erl_prettypr:format(_H)
	++lists:foldl(fun(P, Sum) -> Sum ++ ","++ erl_prettypr:format(P)  end, "",_T)
	++")", 	  
    {tuple,Line,[ {string,Line,PsStr}
		  ,{tuple,Line,PsConditions}]}.

mcdcASTPsGs(Var,Ps=[_H|_T],[],Line)->
    LgPs=toLogicalPs(Ps,Var),
    CondPs=sets:to_list(nonLogicalsRaw(LgPs)),
    PsStr= "("++erl_prettypr:format(_H)
	++lists:foldl(fun(P, Sum) -> Sum ++ ","++ erl_prettypr:format(P)  end, "",_T)
	++")",	  
   {tuple,Line,[ {string,Line,PsStr}
		  ,{tuple,Line,CondPs}
		]};
mcdcASTPsGs(Var,Ps=[_H|_T],Gs=[_Hp|_Tp],Line)->
    LgGs=toLogicalGs(Gs),
    LgPs=toLogicalPs(Ps,Var),
    CondGs=sets:to_list(nonLogicalsRaw(LgGs)),
    CondPs=sets:to_list(nonLogicalsRaw(LgPs)),
    GsStr= erl_prettypr:format(LgGs),
    PsStr= "("++erl_prettypr:format(_H)
	++lists:foldl(fun(P, Sum) -> Sum ++ ","++ erl_prettypr:format(P)  end, "",_T)
	++")",	  
    {tuple,Line,[ {string,Line,PsStr}
		  ,{string,Line,GsStr}
		  ,{tuple,Line,CondPs}
		  ,{tuple,Line,CondGs}]}.

%mcdcASTGs gets AST of Gs and calculates AST of code that returns values of each boolean variable with their names
%transFc({clause,Line,Ps,Gs,B},_Name) ->
%   
%    {clause,Line,transPs(Ps,_Name),transGs(Gs,_Name),
%     [logDataASTP(_Name,Line,funcEntry,1)| transB(B,_Name)]}.
%----------------------------------------
%transIcs(Ics=[_H|_T],_Name)->
%    transCls(Ics,_Name).
%transIc(_T,_Name)->
%    transC(_T,_Name).
%----------------------------------------
%transCcs(Ccs=[_H|_T],_Name)->
%    transCls(Ccs,_Name).
%transCc(_T,_Name)->
%    transC(_T,_Name).
%----------------------------------------
%transTcs(Tcs=[_H|_T],_Name)->
%    transCls(Tcs,_Name).
%transTc(_T,_Name)->
%    transC(_T,_Name).
%----------------------------------------
%transC(_T={clause,Line,Ps,Gs,B},_Name) ->
						% io:format("-------------"),
						% io:fwrite("~w~n", [_T]),
%   Res= {clause,Line,transPs(Ps,_Name),transGs(Gs,_Name), 
%	 [logDataASTP(_Name,Line,clauseEntry,updateCounter(self(),_Name))
%	  |transB(B,_Name)]},
						%  io:format("-------------\n"),
						%  io:fwrite("~w~n", [Res]),
%   Res. 

%----------------------------------------
%transCls(Cls,_Name)->
%    transClsP(Cls,_Name,[],"match(_)","true").
%----------------
%transClsP([],_,_,_,_)->
%    [];
%transClsP([{clause,Line,Ps,Gs,B}|Cls],_Name,OldGt,OldPs,OldExp) ->
%    Lg=toLogicalGs(Gs),
%    Pg=undefined,
%    Conditions=sets:to_list(nonLogicalsRaw(Lg)),
%    ExpStr= erl_prettypr:format(Lg),
%    PsStr= erl_prettypr:format(Pg),
%    OldGtP=OldGt++Conditions,
%    CurrentPsStr = "match("++PsStr ++ ") and "++ OldPs,   
%    OldPsP="(not match("++ PsStr ++ ") and "++ OldPs ++ ")",	  
%    CurrentExpStr = ExpStr ++ " and "++ OldExp,
%    OldExpP= "(not "++ ExpStr ++ " and "++ OldExp ++ ")",
%    Info={tuple,Line,[{atom,Line,clauseEntry}
%		      ,{string,Line,CurrentExpStr}
%		      ,{string,Line,CurrentPsStr},
%		      ,{tuple,Line,OldGtP}]}, 
 %   [{clause,Line,transPs(Ps,_Name),transGs(Gs,_Name), 
%	 [logDataASTP(_Name,Line,Info,updateCounter(self(),_Name))
%	  |transB(B,_Name)]}
%     |transClsP(Cls,_Name,OldGtP,OldPsP,OldExpP)].
