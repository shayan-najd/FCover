-module(ast).
-compile(export_all).
-import(logger,[logger/1,updateCounter/2,receiveTable/0]).
-import(header,[transF/1]).
%------------------------------------------------------------------
trans(Ast) when is_list(Ast) -> 
    lists:map(fun(X)-> transF(X) end,Ast).
%------------------------------------------------------------------
transA(A)
%when is_atom(A) ->%Assume it is only atom
-> A.
%------------------------------------------------------------------
transField(A)
%when is_atom(A) -> %--we assume field is atom
-> A.
%------------------------------------------------------------------
log(AST,Pid,FName,PointNumber,Line,Detail) ->
    rareLoggerName!{Pid,FName,PointNumber,Line,Detail},
    AST.
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
logData(_Name,Line,Info)->
    Num=newProgramPoint(_Name),
     logDataASTP(_Name,Line,Info,Num).
%-------------------
logDataASTP(Name,Line,Info,Num)->
     logDataAST(Name,Line,{atom,Line,Info},Num).
%-------------------
logDataAST(Name,Line,Info,Num)->
    {op,0,'!'
     ,{atom,0,rareLoggerName}
     ,{tuple,0,[ {call,0,{atom,0,self},[]}
		      ,{atom,0,Name}
		      ,{integer,0,Num}
		      ,{integer,0,Line}
		      %,{tuple,Line,[{atom,Line,Info}]}
%{var,Line,'X'}
		      ,{tuple,0,[Info]}
		    ]
	}
      }.
%------------------------------------------------------------------
encloseData(Exp,_)when element(2,Exp)==0 ->
    Exp;
encloseData(Exp,_Name)->  
    Num = updateCounter(self(),_Name),
    encloseDataNum(Exp,_Name,Num).
%------------------------------------------------------------------
encloseDataNum(Exp,_,_)when element(2,Exp)==0 ->
    Exp;
encloseDataNum(Exp,_Name,Num)->
    Line=element(2,Exp),
    enclose(Exp
	    ,logDataASTP(_Name,Line,exception,Num)
	    ,"X"++integer_to_list(Num)).
%------------------------------------------------------------------
enclose(Exp,_,_) when element(2,Exp)==0 ->
    Exp;
enclose(Exp,LogCode,VarName)->
    Line=element(2,Exp),
        {'try',Line,
                 [Exp],
                 [],
                 [{clause,Line,
                          [{tuple,Line,[{atom,Line,exit},{var,Line,list_to_atom(VarName)}
					,{var,Line,'_'}]}],
                          [],
                          [LogCode,{call,Line,{atom,Line,exit}
				    ,[{var,Line,list_to_atom(VarName)}]}]},
		  {clause,Line,
		          [{tuple,Line,[{atom,Line,error},{var,Line,list_to_atom(VarName)}
					,{var,Line,'_'}]}],
                	  [],
		          [LogCode,{call,Line,{remote,Line,{atom,Line,erlang}
					       ,{atom,Line,error}}
				    ,[{var,Line,list_to_atom(VarName)}]}]},
                  {clause,Line,
                          [{tuple,Line,[{atom,Line,throw},{var,Line,list_to_atom(VarName)}
					,{var,Line,'_'}]}],
                          [],
                          [LogCode,{call,Line,{atom,Line,throw},[{var,Line,list_to_atom(VarName)}]}]}],
                 []}.
