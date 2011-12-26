-module(ast).
-compile(export_all).
-import(logger,[logger/1,updateCounter/2,receiveTable/0]).
-import(header,[transF/1]).
%------------------------------------------------------------------
trans(Ast) when is_list(Ast) -> 
    lists:map(fun(X)-> transF(X) end,Ast).
%------------------------------------------------------------------
transA(A)
-> A.%Assume it is only atom
%------------------------------------------------------------------
transField(A)
-> A.%--we assume field is atom
%------------------------------------------------------------------
newProgramPoint(_Name)->
    updateCounter(self(),_Name).
%------------------------------------------------------------------
runTrans(Ast) ->
    CounterPid = spawn(logger,logger,[dict:new()]),
    W = whereis(counter),
    if is_pid(W) -> 
	    unregister(counter);
       true -> nothing
    end,
    register(counter,CounterPid),
    ResultAst = trans(Ast),
    counter!{getTable,self()},
    {ResultAst,receiveTable()}.
%------------------------------------------------------------------
logData(Name,Line,Info)->
    Num=newProgramPoint(Name),
    logDataASTP(Name,Line,Info,Num).
%-------------------
logDataASTP(Name,Line,Info,Num)->
    logDataAST(Name,Line,{atom,-14,Info},Num).
%-------------------
logDataAST(Name,Line,Info,Num)->
%don't mcdc,enclose,dc and not logic 14
    {op,-14,'!'
     ,{atom,-14,rareLoggerName}
     ,{tuple,-14,[ {call,-14,{atom,-14,self},[]}
		      ,{atom,-14,Name}
		      ,{integer,-14,Num}
		      ,{integer,-14,Line}
 		      ,{tuple,-14,[Info]}
		    ]
	}
      }.
%------------------------------------------------------------------
encloseData(Exp,_)when element(2,Exp)=<0, (abs(element(2,Exp)) bor 4) == abs(element(2,Exp))   ->    
    Exp;
encloseData(Exp,_Name)->  
    Num = updateCounter(self(),_Name),
    encloseDataNum(Exp,_Name,Num).
%------------------------------------------------------------------
encloseDataNum(Exp,_,_)when element(2,Exp)=<0,(abs(element(2,Exp)) bor 4) == abs(element(2,Exp)) ->
    Exp;
encloseDataNum(Exp,_Name,Num)->
    Line=element(2,Exp),
    enclose(Exp
	    ,logDataASTP(_Name,Line,exception,Num)
	    ,"VarUnique"++integer_to_list(Num)).
%------------------------------------------------------------------
enclose(Exp,_,_) when element(2,Exp)=<0, (abs(element(2,Exp)) bor 4) == abs(element(2,Exp))->
    Exp;
enclose(Exp,LogCode,VarName)->
    {'try',-14,
     [Exp],
     [],
     [{clause,-14,
       [{tuple,-14,[{atom,-14,exit},{var,-14,list_to_atom(VarName)}
		     ,{var,-14,'_'}]}],
       [],
       [LogCode,{call,-14,{atom,-14,exit}
		 ,[{var,-14,list_to_atom(VarName)}]}]},
      {clause,-14,
       [{tuple,-14,[{atom,-14,error},{var,-14,list_to_atom(VarName)}
		     ,{var,-14,'_'}]}],
       [],
       [LogCode,{call,-14,{remote,-14,{atom,-14,erlang}
			    ,{atom,-14,error}}
		 ,[{var,-14,list_to_atom(VarName)}]}]},
      {clause,-14,
       [{tuple,-14,[{atom,-14,throw},{var,-14,list_to_atom(VarName)}
		     ,{var,-14,'_'}]}],
       [],
       [LogCode,{call,-14,{atom,-14,throw},[{var,-14,list_to_atom(VarName)}]}]}],
     []}.
