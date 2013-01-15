-module (logger).
-compile (export_all).

%-----------------------------
logger(Table) ->
    receive
	{getLast,SPid,Pid,FName} ->
	    {ok,Lst} = dict:find(Pid,Table),
	    io:format("here"),
	    {FName,Lv,_,_}=lists:last(proplists:lookup_all(FName,Lst)),
	    SPid!Lv,
	    logger(Table);
	{getTable,Pid} -> 
	    Pid!{table,Table},
	    logger(Table);
	{Pid,FName,PointNumber,Line,Detail} ->
	    logger(dict:append(Pid,{FName,PointNumber,Line,Detail},Table));
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
    counter!{Pid,FName,Nv,1,{}},
    Nv.
%------------------------------
receiveTable() ->	
    receive
	{table,Table} -> 
	    Table;
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    rareLoggerName!{getTable,self()},
	    receiveTable()
    end.
