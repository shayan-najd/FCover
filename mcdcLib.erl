-module(mcdcLib).
-compile(export_all).
-import(ast,[logDataAST/4]).
-import(mcdc,[nonLogicalsRaw/2,toLogicalGs/1,toLogicalPs/2]).
%------------------------------------------------------------------
%
%------------------------------------------------------------------
logPsGs(VarName,Ps,Gs,Num,_Name,Line)->
  {'case',-14,{var,-14,VarName},
     [
      {clause,-14,Ps,[],[logDataAST(_Name,Line,mcdcASTPsGs(VarName,Ps,Gs,_Name),Num)]}
      ,{clause,-14,[{var,-14,'_'}],[],[logDataAST(_Name,Line
					     ,mcdcASTNotPsGs(VarName,Ps,_Name),Num)]}
     ]}.
%------------------------------------------------------------------
%
%------------------------------------------------------------------
logGs(Gs,Num,_Name,Line)->
   logDataAST(_Name,Line,mcdcASTGs(Gs,_Name),Num).
%------------------------------------------------------------------
%
%------------------------------------------------------------------
mcdcASTGs(Gs=[_H|_T],_Name) ->
    Lg=toLogicalGs(Gs),
    Conditions=sets:to_list(nonLogicalsRaw(Lg,_Name)),
    ExpStr= erl_prettypr:format(Lg),	  
    {tuple,-14,[ {string,-14,ExpStr}
		      ,{tuple,-14,Conditions}]}.
%------------------------------------------------------------------
%
%------------------------------------------------------------------
mcdcASTNotPsGs(Var,Ps=[_H|_T],_Name)->
    Lg=toLogicalPs(Ps,Var),
    PsConditions=sets:to_list(nonLogicalsRaw(Lg,_Name)),
    PsStr= "("++erl_prettypr:format(_H)
	++lists:foldl(fun(P, Sum) -> Sum ++ ","++ erl_prettypr:format(P)  end, "",_T)
	++")", 	  
    {tuple,-14,[ {string,-14,PsStr}
		  ,{tuple,-14,PsConditions}]}.
%------------------------------------------------------------------
%
%------------------------------------------------------------------
mcdcASTPsGs(Var,Ps=[_H|_T],[],_Name)->
    LgPs=toLogicalPs(Ps,Var),
    CondPs=sets:to_list(nonLogicalsRaw(LgPs,_Name)),
    PsStr= "("++erl_prettypr:format(_H)
	++lists:foldl(fun(P, Sum) -> Sum ++ ","++ erl_prettypr:format(P)  end, "",_T)
	++")",	  
   {tuple,-14,[ {string,-14,PsStr}
		  ,{tuple,-14,CondPs}
		]};
mcdcASTPsGs(Var,Ps=[_H|_T],Gs=[_Hp|_Tp],_Name)->
    LgGs=toLogicalGs(Gs),
    LgPs=toLogicalPs(Ps,Var),
    CondGs=sets:to_list(nonLogicalsRaw(LgGs,_Name)),
    CondPs=sets:to_list(nonLogicalsRaw(LgPs,_Name)),
    GsStr= erl_prettypr:format(LgGs),
    PsStr= "("++erl_prettypr:format(_H)
	++lists:foldl(fun(P, Sum) -> Sum ++ ","++ erl_prettypr:format(P)  end, "",_T)
	++")",	  
    {tuple,-14,[ {string,-14,PsStr}
		  ,{string,-14,GsStr}
		  ,{tuple,-14,CondPs}
		  ,{tuple,-14,CondGs}]}.
