-module (astExtract).
-compile (export_all).

%------------------------------------------------------------------
trans(Ast) when is_list(Ast) -> 
    lists:map((fun astExtract:trans/1),Ast); 
trans({attribute,A,record, Lst = [_H|_T]})->
    {attribute,A,record, transExp(Lst,"record")}; 
trans(Ast = {attribute,_,_,_}) -> 
    Ast;  
trans({function,LINE,Name,Arity,Lst = [_H|_T]}) ->
    {function,LINE,Name,Arity,transFunctionClause(Lst,Name)};
%------------
trans({error,_}) -> exit(notdefined) ;
trans({eof,_}) ->exit(notdefined) ;
trans({warning,_}) ->exit(notdefined).
%------------
transExp(Ast,_Name) when is_list(Ast) -> 
    lists:map((fun(X)-> astExtract:transExp(X,_Name) end),Ast); 
transExp({record_field,LINE,E_0,Name,Field},_Name)->
    {record_field,LINE,transExp(E_0,_Name),Name,transExp(Field,_Name)};
transExp({record_field,LINE,A},_Name) -> 
    {record_field,LINE,transExp(A,_Name)}; % ?
transExp({record_field,LINE,A,E},_Name)->  
    {record_field,LINE,transExp(A,_Name),transExp(E,_Name)}; 
%------------
transExp(Ast = {integer,_,_},_Name) -> 
    Ast;
transExp(Ast = {float,_,_},_Name ) ->
    Ast;
transExp(Ast = {string,_,_},_Name) ->
    Ast;
transExp(Ast = {atom,_,_},_Name) ->
    Ast;
%------------
transExp(Ast = {var,_,_},_Name) ->
    Ast;
transExp(Ast = {nil,_},_Name) ->
    Ast;
transExp({record,LINE,Name,Lst = [_H|_T]},_Name)->
    {record,LINE,Name,transExp(Lst,_Name)};
transExp({record,LINE,E_0,Name, Lst = [_H|_T]},_Name) ->
    {record,LINE,transExp(E_0,_Name),Name,transExp(Lst,_Name)};
transExp({record_index,LINE,Name,Field},_Name) ->
    {record_index,LINE,Name,trans(Field)};
transExp({match,LINE,P_1,P_2},_Name) ->
    {match, LINE,transExp(P_1,_Name),transExp(P_2,_Name)}; 
transExp({tuple,LINE,Lst = [_H|_T]},_Name) ->
    {tuple,LINE,transExp(Lst,_Name)};
transExp({cons,LINE,P_h, P_t},_Name) ->
    {cons,LINE,transExp(P_h,_Name),transExp(P_t,_Name)};
transExp({bin,LINE,Lst = [_H|_T]},_Name) ->
    {bin,LINE,transExp(Lst,_Name)};
transExp({op,LINE,Op,P_1,P_2},_Name) ->
    {op,LINE,Op,transExp(P_1,_Name),transExp(P_2,_Name)};
transExp({op,LINE,Op,P_0},_Name) ->
    {op,LINE,Op,transExp(P_0,_Name)};
transExp({'catch',LINE,E_0},_Name) ->
    {'catch',LINE,transExp(E_0,_Name)};
transExp({call,LINE,{remote,LINE,E_m,E_0}, Lst = [_H|_T] },_Name) ->
    {call,LINE,{remote,LINE,transExp(E_m,_Name),transExp(E_0,_Name)},transExp(Lst,_Name)};
transExp({call,LINE,E_0, Lst},_Name)when is_list(Lst)->
    {call,LINE,transExp(E_0,_Name), transExp(Lst,_Name)};
transExp({lc,LINE,E_0, Lst = [_H|_T]},_Name) ->
    {lc,LINE,transExp(E_0,_Name),transExp(Lst,_Name)};
transExp({bc,LINE,E_0, Lst = [_H|_T]},_Name) ->
    {bc,LINE,transExp(E_0,_Name),transExp(Lst,_Name)};
transExp({block,LINE,B},_Name) ->
    {block,LINE,transExp(B,_Name)};
transExp({'if',LINE,Lst = [_H|_T]},_Name) ->
    {'if',LINE,transConditionClause(Lst,_Name)};
transExp({'case',LINE,E_0, Lst = [_H|_T]},_Name) ->
    {'case',LINE,transConditionClause(E_0,_Name),transConditionClause(Lst,_Name)};
transExp({'try',LINE,B,Lst1,Lst2,A},_Name) when is_list(Lst1) and is_list(Lst2) ->
    {'try',LINE,transExp(B,_Name),transExp(Lst1,_Name)
     ,transExp(Lst2,_Name) ,transExp(A,_Name)};
transExp({'receive',LINE, Lst = [_H|_T]} ,_Name) ->
    {'receive',LINE,transExp(Lst,_Name)};
transExp({'receive',LINE,Lst = [_H|_T],E_0, B_t},_Name) ->
    {'receive',LINE,transExp(Lst,_Name),transExp(E_0,_Name),transExp(B_t,_Name)};
transExp(Ast = {'fun',_,{function,_,_}},_Name ) ->
    Ast;
transExp(Ast = {'fun',_,{function,_,_,_}},_Name) ->
    Ast;
transExp({'fun',LINE,{clauses,Lst = [_H|_T]}},_Name) ->
    {'fun',LINE,{clauses,transExp(Lst,_Name)}};
transExp({'query',LINE,{lc,LINE,E_0,Lst}},_Name)->
    {'query',LINE,{lc,LINE,transExp(E_0,_Name),transExp(Lst,_Name)}};
%------------ 
transExp({generate,LINE,P,E},_Name) ->
    {generate,LINE,transExp(P,_Name),transExp(E,_Name)};
transExp({b_generate,LINE,P,E},_Name) ->
    {b_generate,LINE,transExp(P,_Name),transExp(E,_Name)};
%------------
transExp({bin_element,LINE,P_1,Size_1,TSL_1},_Name) ->
    {bin_element,LINE,transExp(P_1,_Name),transExp(Size_1,_Name),transExp(TSL_1,_Name)};
%------------
transExp({clause,LINE,Ps,Gs,B},_Name) ->
    {clause,LINE,transExp(Ps,_Name),transExp(Gs,_Name),transExp(B,_Name)}.
%------------------------------
transConditionClause(Cls,_Name) when is_list(Cls) -> 
    lists:map((fun(X)-> transConditionClause(X,_Name) end),Cls);
transConditionClause(_T={clause,Line,Ps,Gs,B},_Name) ->
   % io:format("-------------"),
   %   io:fwrite("~w~n", [_T]),
    {clause,Line,transExp(Ps,_Name),transExp(Gs,_Name),
    [
{op,Line,'!',{atom,Line,rareLoggerName}
       ,{tuple,Line,[ {call,Line,{atom,Line,self},[]}
		      ,{tuple,Line,[{atom,Line,_Name}
				    ,{integer,Line,updateCounter(self(),_Name)}]}
		    ]}
      }| 
transExp(B,_Name)]
    }. 
%------------------------------
transFunctionClause(Cls,_Name) when is_list(Cls) -> 
    lists:map((fun(X)-> transFunctionClause(X,_Name) end),Cls);
transFunctionClause({clause,Line,Ps,Gs,B},_Name) ->
    counter!{self(),{_Name,1}},
    {clause,Line,transExp(Ps,_Name),transExp(Gs,_Name),
     [{op,Line,'!',{atom,Line,rareLoggerName}
       ,{tuple,Line,[ {call,Line,{atom,Line,self},[]}
		      ,{tuple,Line,[{atom,Line,_Name},{integer,Line,1}]}
		    ]}
      }| transExp(B,_Name)]
    }. 
%------------------------------------------------------------------
astFile(Path) ->
    {_,BC}=file:read_file(Path),
    C=binary_to_list(BC),
    ast(C).
%----------
ast(Input) ->
    {ok,Tkns,_}= erl_scan:string(Input),
    Ss = spliter(Tkns),
    [Ast || {ok,Ast} <-(lists:map(fun erl_parse:parse_form/1,Ss))].
%------------------------------------------------------------------
compileLoad(Ast) ->
   compileLoad(lists:nth(1,  [X ||{attribute,1,module,X} <- Ast ]) , Ast).
compileLoad(ModuleName,Ast) ->
    {ok, ModuleName, Bin} = compile:forms(Ast),
    code:load_binary(ModuleName, "nofile", Bin).
%------------------------------------------------------------------
runTrans(Ast) ->
    CounterPid = spawn(astExtract,logger,[dict:new()]),
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
logger(Table) ->
    receive
	{getLast,SPid,Pid,FName} ->
	    {ok,Lst} = dict:find(Pid,Table),
	    {FName,Lv}=lists:last(proplists:lookup_all(FName,Lst)),
	    SPid!Lv,
	    logger(Table);
	{getTable,Pid} -> 
	    Pid!{table,Table},
	    logger(Table);
	{Pid,{FName,PointNumber}} ->
	    logger(dict:append(Pid,{FName,PointNumber},Table));
	M -> 
	    exit(M)
	end.
%-----------------------------
getLast(Pid,FName)->
    counter!{getLast,self(),Pid,FName},
 receive
  	N -> N
 end.
%-----------------------------
updateCounter(Pid,FName) ->
    Lv = getLast(Pid,FName),
    Nv = Lv + 1,
    counter!{Pid,{FName,Nv}},
    Nv.
%-----------------------------------------------------------------
runTest(ModuleName,FunctionName) ->
    runTest(ModuleName,FunctionName,[]).
%-----------------------------
runTest(ModuleName,FunctionName,Args) ->
    LoggerPid = spawn(astExtract,logger,[dict:new()]),
    W = whereis(rareLoggerName),
    if is_pid(W) -> 
	    unregister(rareLoggerName);
       true -> nothing
    end,
    register(rareLoggerName,LoggerPid),
    TestProcess = spawn(ModuleName,FunctionName,Args),
    erlang:monitor(process, TestProcess),
    {receiveTable(),TestProcess}.
%-----------------------------------------------------------------
runTestonString(TestModuleName,TestFunctionName,TestArgs,Code) ->
    Ast1 = ast(Code),
    {Ast2,_} = runTrans(Ast1),
    compileLoad(Ast2),
    runTest(TestModuleName,TestFunctionName,TestArgs).
%------------------------------
getPath(TestModuleName,TestFunctionName,TestArgs,Code) ->
    {T,Pid}=runTestonString(TestModuleName,TestFunctionName,TestArgs,Code),
    {ok,Lst} = dict:find(Pid,T), Lst.
    %proplists:lookup_all(FName,Lst).
%------------------------------
receiveTable() ->	
    receive
	{table,Table} -> 
	    Table;
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    rareLoggerName!{getTable,self()},
	    receiveTable()
    end.
%-----------------------------------------------------------------
spliter(Input) ->
    spliter(Input,[],[]).
spliter([H],A,P) ->
    A++[P++[H]];
spliter([{dot,R}|T],A,P) ->
    spliter(T,A++[P++[{dot,R}]],[]);
spliter([H|T],A,P)-> 
    spliter(T,A,P++[H]).
%-----------------------------------------------------------------
testCode1() ->
    "-module(tm). -compile(export_all). tf()-> 9.".

testCode2() ->
    "-module(tm). -compile(export_all). tf(X)-> if X > 2 -> 0; true -> 1 end.".
test1() ->
    tm:tf().

test2()->
    tm:tf(1).

test3()->
    tm:tf(3).

sample1() ->
    astExtract:getPath(astExtract,test1,[],astExtract:testCode1()).

sample2() ->
    astExtract:getPath(astExtract,test2,[],astExtract:testCode2()).

sample3() ->
    astExtract:getPath(astExtract,test3,[],astExtract:testCode2()).
    
