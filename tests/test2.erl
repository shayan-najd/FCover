-module (tests).
-compile (export_all).
-import(logger,[receiveTable/0]).



run() ->
    CounterPid = spawn(logger,logger,[dict:new()]),
    W = whereis(counter),
    if 
	is_pid(W) -> unregister(counter);
	true -> nothing
    end,
    register(counter,CounterPid).
 %   counter!
%    ResultAst = trans(Ast),
 %  counter!{getTable,self()},
 %  receiveTable().
