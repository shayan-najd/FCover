-module(test).
-compile(export_all).
 
 isNonNegative(X) -> 
       if  (X >-1) orelse ((1/X)>0)-> yes;
           true  -> no
       end.   

foo(X)->
    if is_tuple(X) andalso element(1,X)==2 -> ok;
       true -> false
end.
    
	  
