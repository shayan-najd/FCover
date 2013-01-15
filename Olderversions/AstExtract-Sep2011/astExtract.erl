-module (astExtract).
-compile (export_all).
-import (ast,[runTrans/1]).
-import (logger,[receiveTable/0]).
%------------------------------------------------------------------
% extracts abstract syntax tree out of file at given path
%------------------------------------------------------------------
astFile(Path) ->
    {_,BC}=file:read_file(Path),
    C=binary_to_list(BC),
    ast(C).
%------------------------------------------------------------------
% extracts abstract syntax tree out of input string
%------------------------------------------------------------------
ast(Input) ->
    {ok,Tkns,_}= erl_scan:string(Input),
    Ss = spliter(Tkns),
    [Ast || {ok,Ast} <-(lists:map(fun erl_parse:parse_form/1,Ss))].
%------------------------------------------------------------------
% extracts the ast of a give expression 
%------------------------------------------------------------------
astExp(Input) ->
    {ok,Tkns,_}= erl_scan:string(Input),
    {ok,[Ast]}=erl_parse:parse_exprs(Tkns),
    Ast.
%------------------------------------------------------------------
% compile and loads given abstract syntax tree
%------------------------------------------------------------------
compileLoad(Ast) ->
   compileLoad(lists:nth(1,  [X ||{attribute,1,module,X} <- Ast ]) , Ast).
compileLoad(ModuleName,Ast) ->
    {ok, ModuleName, Bin} = compile:forms(Ast),
    code:load_binary(ModuleName, "nofile", Bin).
%------------------------------------------------------------------
% runs test located at given module with given function name
%------------------------------------------------------------------
runTest(TestModuleName,TestFunctionName) ->
    runTest(TestModuleName,TestFunctionName,[]).
%-----------------------------
runTest(TestModuleName,TestFunctionName,Args) ->
    LoggerPid = spawn(logger,logger,[dict:new()]),
    W = whereis(rareLoggerName),
    if is_pid(W) -> 
	    unregister(rareLoggerName);
       true -> nothing
    end,
    register(rareLoggerName,LoggerPid),
    TestProcess = spawn(TestModuleName,TestFunctionName,Args),
    erlang:monitor(process, TestProcess),
    {receiveTable(),TestProcess}.
%------------------------------------------------------------------
% loads the code and then runs test located at given modulename and
% function name with arguments
%------------------------------------------------------------------
runTestonString(TestModuleName,TestFunctionName,TestArgs,Code) ->
    Ast1 = ast(Code),
    {Ast2,_} = runTrans(Ast1),
    compileLoad(Ast2),
    runTest(TestModuleName,TestFunctionName,TestArgs).
%------------------------------------------------------------------
% loads the file and then runs test located at given modulename and
% function name with (/out) arugments
%------------------------------------------------------------------
runTestonFile(CodeModuleName,TestModuleName,TestFunctionName) ->
    runTestonFile(CodeModuleName,TestModuleName,TestFunctionName,[]).
%----------
runTestonFile(CodeModuleName,TestModuleName,TestFunctionName,TestArgs) ->
    Ast1 = astFile(atom_to_list(CodeModuleName)++".erl"),
    {Ast2,_} = runTrans(Ast1),
    compileLoad(Ast2),
    runTest(TestModuleName,TestFunctionName,TestArgs).
%------------------------------------------------------------------
% executes test by parameters on file and returns path
%------------------------------------------------------------------
getPathFile(TestModuleName,TestFunctionName,TestArgs,CodeModuleName) ->
    {T,Pid}=runTestonFile(CodeModuleName,TestModuleName,TestFunctionName,TestArgs),
    {ok,Lst} = dict:find(Pid,T), 
    Lst.
%------------------------------------------------------------------
% executes test by parameters on string and returns path
%------------------------------------------------------------------
getPathString(TestModuleName,TestFunctionName,TestArgs,Code) ->
    {T,Pid}=runTestonString(TestModuleName,TestFunctionName,TestArgs,Code),
    {ok,Lst} = dict:find(Pid,T),
    Lst.
    %proplists:lookup_all(FName,Lst).
%-----------------------------------------------------------------
spliter(Input) ->
    spliter(Input,[],[]).
spliter([H],A,P) ->
    A++[P++[H]];
spliter([{dot,R}|T],A,P) ->
    spliter(T,A++[P++[{dot,R}]],[]);
spliter([H|T],A,P)-> 
    spliter(T,A,P++[H]).
%-------------------------------------
transCode(Code)->
    Ast1 = ast(Code),
    {Ast2,_} = runTrans(Ast1),
    lists:foldl(fun(Format,Sum)->Sum++"\n"++ erl_prettypr:format(Format)end,"",Ast2).

