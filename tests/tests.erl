-module(tests).
-compile (export_all).
-import(astExtract,[getPathFile/4,getPathString/4]).
-include_lib("eqc/include/eqc.hrl").

%-----------------------------------------------------------------
testCode1() ->
    "-module(tm). \n -compile(export_all). \n tf()-> 9.".

testCode2() ->
    "-module(tm).\n -compile(export_all). \ntf(X)-> \n if X > 2 -> \n 0; true -> \n 1 end.".

testCode3() ->
    "-module(tm).\n -compile(export_all). \ntf(X)-> \n if X > 2 -> \n (1+a); \n true -> \n 1 end.".    

testCode4() ->   
     "-module(tm).\n -compile(export_all).\n tf()-> 1+a.".

testCode5() ->   
     "-module(tm).\n -compile(export_all).\n testf(A)-> 1+A.".


test1() ->
    tm:tf().

test1p() ->
    catch test1().

test2()->
    tm:tf(1).

test2p()->
    catch test2().

test3()->
    tm:tf(3).

test3p()->
    catch test3().

test4() ->
    catch tm:testf(a).

test5() ->
    catch tm:testf(2).

sample1() ->
    getPathString(tests,test1,[],testCode1()).

sample2() ->
     getPathString(tests,test2,[],testCode2()).

sample3() ->
     getPathString(tests,test3,[],testCode2()).

sample4() ->
    eqc_suite:feature_based(tests:prop_fact()).

sample5() ->
     getPathString(tests,test2p,[],testCode3()).    

sample6() ->
     getPathString(tests,test3p,[],testCode3()).    

sample7() ->
     getPathString(tests,test1p,[],testCode4()).     

sample8() ->
     getPathString(tests,test4,[],testCode5()).     

sample9() ->
     getPathString(tests,test5,[],testCode5()).     


prop_fact() ->
    ?FORALL(N,nat(),
	    begin
		Lst = getPathFile(fact,fact,[N],fact),
		features(Lst,true)
	    end).

sample10()->
   io:format( astExtract:transCode("-module(test). -compile(export_all). f()->1.")).

sample11()->io:format("\n+++++++\n"),
    io:format( 
      astExtract:transCode("-module(test)."++ 
			   "-compile(export_all)."++
			   " f()-> if true -> 3 end."
			  )).

sample12()->
    io:format( 
      astExtract:transCode("-module(test)."++ 
			   "-compile(export_all)."++
			   " f()-> if 1>2 -> 2; true -> 3 end."
			  )).
    
%io:format("~p~n",[lists:nth(1,Logs)]),   
