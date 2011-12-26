-module(testMcdc).
-compile(export_all).
-import(mcdc,[checkMCDCString/2,normalize/3,calcMCDCString/1]).
-import(astExtract,[astExp/1,astString/1]).
test1()->
    Cases=[[{'A',true},{'B',true},{'C',false}]
	   ,[{'A',true},{'B',false},{'C',true}]
	   ,[{'A',true},{'B',false},{'C',false}]
	   ,[{'A',false},{'B',true},{'C',false}]],
    checkMCDCString("A and ( B or C).",Cases).

test2() ->
    calcMCDCString("A and (B or C).").

test3() ->    
    {Smt,_}=normalize(astExp(" (X>0) or ((X==0) and (X>0))."),[],"V"),
    astString(Smt).

test4() ->
   TestCases = [
		[
		 {{op,1,'>',{var,1,'X'},{integer,1,0}},false},
		 {{op,1,'==',{var,1,'X'},{integer,1,0}},true}
		],
		[
		 {{op,1,'>',{var,1,'X'},{integer,1,0}},true},
		 {{op,1,'==',{var,1,'X'},{integer,1,0}},true}
		]
	       ],
    ExpStr = " (X>0) or ((X==0) and (X>0)).",
    checkMCDCString(ExpStr,TestCases).
