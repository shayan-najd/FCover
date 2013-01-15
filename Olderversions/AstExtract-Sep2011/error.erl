-module(error).
-compile(export_all).

transX({error,_}) -> exit(notdefined) ;
transX({eof,_}) ->exit(notdefined) ;
transX({warning,_})->exit(notdefined);
transX({throw,_P,_})->exit(notdefined);
transX({_X,_P,_})->exit(notdefined) ;
transX({_A_m,_A})->exit(notdefined) .
