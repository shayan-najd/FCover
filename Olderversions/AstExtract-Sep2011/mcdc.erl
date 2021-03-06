-module (mcdc).
-compile (export_all).
-import(astExtract,[astExp/1]).
%---------------------------
expAstToString({op,_,Op,Exp1,Exp2})
  when Op=='or';Op=='and';Op=='xor';Op=='orelse';Op=='andalso' ->
   "( "++ expAstToString(Exp1) ++" "++atom_to_list(Op)++" "++expAstToString(Exp2)++" )";
expAstToString ({op,_,'not',Exp1}) ->
    "not "++expAstToString(Exp1);
expAstToString ({atom,_,V})
  when V == true; V==false->
    atom_to_list(V);
expAstToString({var,_,V}) ->
    atom_to_list(V).
%------------------------
expAstToStringRaw({op,_,Op,Exp1,Exp2})
  when Op=='or';Op=='and';Op=='xor';Op=='orelse';Op=='andalso' ->
   "( "++ expAstToStringRaw(Exp1) ++" "++atom_to_list(Op)++" "++expAstToStringRaw(Exp2)++" )";
expAstToStringRaw ({op,_,'not',Exp1}) ->
    "not "++expAstToStringRaw(Exp1);
expAstToStringRaw ({atom,_,V})
  when V == true; V==false->
    atom_to_list(V);
expAstToStringRaw({var,_,V}) ->
    atom_to_list(V);
expAstToStringRaw(Exp) ->
   Res= erl_prettypr:format(Exp),io:format("-after-"),Res.
%---------------------------
normString(Str,Prefix)->
    {Nrm,_}=normalize(astExp(Str),[],Prefix),
    expAstToString(Nrm)++".".

normalize({op,Line,Op,Exp1,Exp2},Env,Prefix)
  when Op=='or';Op=='and';Op=='xor';Op=='orelse';Op=='andalso' ->
    {Exp1P,Env1}=normalize(Exp1,Env,Prefix),
    {Exp2P,Env2}=normalize(Exp2,Env1,Prefix),	
    {{op,Line,Op,Exp1P,Exp2P},Env2};
normalize({op,Line,'not',Exp1},Env,Prefix) ->
    {Exp1P,Env1}=normalize(Exp1,Env,Prefix),
    {{op,Line,'not',Exp1P},Env1}; 
normalize({atom,Line,V},Env,_)
  when V == true; V==false->
    {{atom,Line,V},Env};
normalize({var,Line,V},Env,_) ->
    {{var,Line,V},Env};
normalize(Exp,Env,Prefix) ->
    Res = lists:keyfind(Exp,1,Env),
    case Res of
	{Exp,V} -> {{var,element(2,Exp),V},Env};
	false   -> NewName = list_to_atom(Prefix++integer_to_list(length(Env))),
		   Env1 = [{Exp,NewName}|Env],
		   {{var,element(2,Exp),NewName},Env1}		   		   
    end.	 
%---------------------------
nonLogicals({op,_,Op,Exp1,Exp2})
  when Op=='or';Op=='and';Op=='xor';Op=='orelse';Op=='andalso' ->
    sets:union(nonLogicals(Exp1),nonLogicals(Exp2));
nonLogicals({op,_,'not',Exp1}) ->
    nonLogicals(Exp1);
nonLogicals({atom,_,V})
  when V == true; V==false->
    sets:new();
nonLogicals({var,_,V}) ->
    sets:from_list([{V,false}]).
%------------------------------
nonLogicalsRaw({op,_,Op,Exp1,Exp2})
  when Op=='or';Op=='and';Op=='xor';Op=='orelse';Op=='andalso' ->
    sets:union(nonLogicalsRaw(Exp1),nonLogicalsRaw(Exp2));
nonLogicalsRaw({op,_,'not',Exp1}) ->
    nonLogicalsRaw(Exp1);
nonLogicalsRaw({atom,_,V})
  when V == true; V==false->
    sets:new();
nonLogicalsRaw(Exp) ->    
 ExpStr= erl_prettypr:format(Exp),
 Line= element(2,Exp),
 sets:from_list([{tuple,Line ,[{string,Line,ExpStr},Exp]}]).

%---------------------------
isLogical({op,_,Op,Exp1,Exp2})
  when Op=='or';Op=='and';Op=='xor';Op=='orelse';Op=='andalso'  ->
    isLogical(Exp1) and isLogical(Exp2);
isLogical({op,_,'not',Exp1})->
    isLogical(Exp1);
isLogical({atom,_,V})
  when V == true; V==false->
    true;
isLogical({var,_,_}) ->
    true;
isLogical(_) ->
    false.
%---------------------------
eval({op,_,'or',Exp1,Exp2},Env) ->
    eval(Exp1,Env) or eval(Exp2,Env);
eval({op,_,'and',Exp1,Exp2},Env) ->
    eval(Exp1,Env) and eval(Exp2,Env);
eval({op,_,'xor',Exp1,Exp2},Env) ->
    eval(Exp1,Env) xor eval(Exp2,Env);
eval({op,_,'orelse',Exp1,Exp2},Env) ->
    eval(Exp1,Env) orelse eval(Exp2,Env);
eval({op,_,'andalso',Exp1,Exp2},Env) ->
    eval(Exp1,Env) andalso eval(Exp2,Env);
eval({op,_,'not',Exp1},Env) ->
    not eval(Exp1,Env);
eval({atom,_,true},_)->
    true;
eval({atom,_,false},_)->    
    false;
eval({var,_,V},Env) ->
    {V,Res}=lists:keyfind(V,1,Env),
    Res.
%---------------------------
truthString(ExpStr)->
    truth(astExp(ExpStr)).

truth(Exp)->
    Lst = sets:to_list(nonLogicals(Exp)),
    N = length(Lst),
    truthP(Exp,Lst,N,1).

truthP(Exp,Lst,N,I) when I < N-> 
    LstP = changeNth(Lst,I,true),
    truthP(Exp,Lst,N,I+1) ++ truthP(Exp,LstP,N,I+1);
truthP(Exp,Lst,N,I) when I==N ->
    LastZero=changeNth(Lst,N,false),
    LastOne=changeNth(Lst,N,true),
    [{LastZero,eval(Exp,LastZero)}
     ,{LastOne,eval(Exp,LastOne)}].
%---------------------------  
changeNth(Lst,I,V)->
    {Knth,_} = lists:nth(I,Lst),
    lists:keyreplace(Knth,1,Lst,{Knth,V}).
%---------------------------
viewList(Lst)->    
    [ V || {_,V} <- Lst].
%---------------------------
negateAtom(Case,Atom) ->
 {Atom,V}= lists:keyfind(Atom,1,Case),  
 lists:keyreplace(Atom,1,Case,{Atom,not V}).
%---------------------------
checkMCDCString(Str,Cases) ->
    checkMCDC(astExp(Str),Cases).
%---------------------------   
checkMCDC(InputExp,InputCases) ->
    IL=isLogical(InputExp),
    {Exp,Cases} = if IL -> {InputExp,InputCases};
		     not IL -> {E,Env} = normalize(InputExp,[],"V"),
			       CasesP = reNameCases(InputCases,Env),
			       {E,CasesP}
		  end,
    Atoms = sets:to_list(nonLogicals(Exp)), 
    Lst = [checkCases(Exp,Atom,Cases) || {Atom,_} <- Atoms],
    lists:foldr(fun(X,Prod)-> X and Prod end,true,Lst).
%---------------------------
checkCases(Exp,Atom,[]) ->   
  calcCases(Exp,Atom,truth(Exp))==[];% is result independent from condition
checkCases(Exp,Atom,[Case|Cases]) ->
    NotCase   = negateAtom(Case,Atom),
    Cont = contains(NotCase,Cases), 
    Val  = eval(Exp,Case),
    ValN = eval(Exp,NotCase),
    if 
	Cont, Val== (not ValN) -> true;
	true -> checkCases(Exp,Atom,Cases)
    end.
%---------------------------	    
contains(_,[]) -> false; 
contains(Key, List) -> lists:any(fun(X) -> Key == X end, List). 

%---------------------------
calcMCDCString(ExpString) ->
    calcMCDC(astExp(ExpString)).
%---------------------------
calcMCDC(Exp) ->
    TruthTbl = truth(Exp),
    Atoms = sets:to_list(nonLogicals(Exp)), 
    Lst = [calcCases(Exp,Atom,TruthTbl) || {Atom,_} <- Atoms],
    sets:to_list(lists:foldr(fun(X,Prod)-> 
				     sets:union(sets:from_list(X),Prod) 
			     end
			     ,sets:new(),Lst)).
%---------------------------
calcCases(_,_,[]) ->
    [];
calcCases(Exp,Atom,[TCase|TCases])->
    {Case ,Val} = TCase,
    NotCase   = negateAtom(Case,Atom),
    NotTCase  = {NotCase,not Val},
    Cont = contains(NotTCase,TCases), 
    if 
	Cont -> [Case,NotCase];
	true -> calcCases(Exp,Atom,TCases)
    end. 
%---------------------------
% in case of contradiction ( not A and A ) MCDC is not covered!    
%---------------------------
calculateMCDC(ExpStr)->
    {Nrm,Env}=normalize(astExp(ExpStr),[],"V"),
    %NormStr=astToString(Nrm)++".";
    Mcdcs=calcMCDC(Nrm), 
    lists:map(fun(Case) ->
		      lists:map(fun({K,V}) ->
					{E,K}=lists:keyfind(K,2,Env),
					{E,V}
				end,Case)

	      end,Mcdcs).	      

%---------------------------
reNameCases(Cases,Env)->
    lists:map(fun(Case) ->
		      lists:map(fun({K,V}) ->
					{K,E}=lists:keyfind(K,1,Env),
					{E,V}
				end,Case)

	      end,Cases).	      
    
%---------------------------
test1()->
    Cases=[[{'A',true},{'B',true},{'C',false}]
	   ,[{'A',true},{'B',false},{'C',true}]
	   ,[{'A',true},{'B',false},{'C',false}]
	   ,[{'A',false},{'B',true},{'C',false}]],
    mcdc:checkMCDCString("A and ( B or C).",Cases).

test2() ->
    calcMCDCString("A and (B or C).").

test3() ->    
    {Smt,_}=mcdc:normalize(mcdc:astExp(" (X>0) or ((X==0) and (X>0))."),[],"V"),
    mcdc:astToString(Smt).

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
%----------------
toLogicalGs([G])->
    encloseLogical(toLogicalG(G));
toLogicalGs(_Gs=[H|T])-> 
   {op,1,'orelse',encloseLogical(toLogicalG(H)),toLogicalGs(T)}.

toLogicalG([Gt])->
    encloseLogical(Gt);
toLogicalG(_Gts=[H|T]) ->
    {op,1,'andalso',encloseLogical(H),toLogicalG(T)}.

encloseLogical(ExpAst)->
    Line=element(2,ExpAst),
    {'try',Line,
       [ExpAst],
       [],
       [{clause,Line,
                [{tuple,Line,[{var,Line,'_'},{var,Line,'_'},{var,Line,'_'}]}],
                [],
                [{atom,Line,false}]}],
       []}.
 
toLogicalPs([P],Var)->
    encloseLogicalP(P,Var);
toLogicalPs(_Ps=[H|T],Var) ->
    {op,1,'andalso',encloseLogicalP(H,Var),toLogicalPs(T,Var)}.

encloseLogicalP(P,Var)->
    Line=element(2,P),
    {'case',Line,
        {var,Line,Var},
        [{clause,Line,[P],[],[{atom,Line,true}]},
         {clause,Line,[{var,Line,'_'}],[],[{atom,Line,false}]}]}.

    %{'try',Line,[{match,Line,P,{var,Line,Var}},{atom,Line,true}],[],[{clause,Line,
    %            [{tuple,Line,[{var,Line,'_'},{var,Line,'_'},{var,Line,'_'}]}],
    %            [],
    %            [{atom,Line,false}]}],[]}.

% shortcut operators, where to put the logger for guards, format of project
% 29 september, 3:00 pm chalmers john hughes
 
%% test()->
%%     {block,1,
%%      [
%%       {match,1,{var,1,'ExpUnique_2'},{tuple,1,[]}}
%%       ,{'case',1,{var,1,'ExpUnique_2'},	[]}
%%       ,{'case',1,{var,1,'ExpUnique_2'},
%% 	[{clause,1,[{var,1,'_'}],[],[]},
%% 	 {clause,1,[{var,1,'_'}],[],[]}
%% 	]},
%%   {'case',1,
%%    {var,1,'ExpUnique_2'},
%%    [{clause,1,
%%      [{tuple,1,[]}],
%%      [],
%%      [{op,1,'!',
%%        {atom,1,rareLoggerName},
%%        {tuple,1,
%%         [{call,1,{atom,1,self},[]},
%%          {atom,1,f},
%%          {integer,1,3},
%%          {integer,1,1},
%%          {tuple,1,[{atom,1,clauseEntry}]}]}},
%%       {'try',1,
%%        [{op,1,'!',
%%          {atom,1,rareLoggerName},
%%          {tuple,1,
%%           [{call,1,{atom,1,self},[]},
%%            {atom,1,f},
%%            {integer,1,1},
%%            {integer,1,1},
%%            {tuple,1,[{atom,1,funcEntry}]}]}}],
%%        [],
%%        [{clause,1,
%%          [{tuple,1,[{atom,1,exit},{var,1,'X4'},{var,1,'_'}]}],
%%          [],
%%          [{op,1,'!',
%%            {atom,1,rareLoggerName},
%%            {tuple,1,
%%             [{call,1,{atom,1,self},[]},
%%              {atom,1,f},
%%              {integer,1,4},
%%              {integer,1,1},
%%              {tuple,1,[{atom,1,exception}]}]}},
%%           {call,1,{atom,1,exit},[{var,1,'X4'}]}]},
%%         {clause,1,
%%          [{tuple,1,[{atom,1,error},{var,1,'X4'},{var,1,'_'}]}],
%%          [],
%%          [{op,1,'!',
%%            {atom,1,rareLoggerName},
%%            {tuple,1,
%%             [{call,1,{atom,1,self},[]},
%%              {atom,1,f},
%%              {integer,1,4},
%%              {integer,1,1},
%%              {tuple,1,[{atom,1,exception}]}]}},
%%           {call,1,{remote,1,{atom,1,erlang},{atom,1,error}},[{var,1,'X4'}]}]},
%%         {clause,1,
%%          [{tuple,1,[{atom,1,throw},{var,1,'X4'},{var,1,'_'}]}],
%%          [],
%%          [{op,1,'!',
%%            {atom,1,rareLoggerName},
%%            {tuple,1,
%%             [{call,1,{atom,1,self},[]},
%%              {atom,1,f},
%%              {integer,1,4},
%%              {integer,1,1},
%%              {tuple,1,[{atom,1,exception}]}]}},
%%           {call,1,{atom,1,throw},[{var,1,'X4'}]}]}],
%%        []},
%%       {atom,1,fff}]},
%%     {clause,1,
%%      [{var,1,'_'}],
%%      [],
%%      [{op,1,'!',
%%        {atom,1,rareLoggerName},
%%        {tuple,1,
%%         [{call,1,{atom,1,self},[]},
%%          {atom,1,f},
%%          {integer,1,5},
%%          {integer,1,1},
%%          {tuple,1,[{atom,1,clauseEntry}]}]}},
%%       {'try',1,
%%        [{call,1,
%%          {remote,1,{atom,1,erlang},{atom,1,error}},
%%          [{atom,1,function_clause}]}],
%%        [],
%%        [{clause,1,
%%          [{tuple,1,[{atom,1,exit},{var,1,'X6'},{var,1,'_'}]}],
%%          [],
%%          [{op,1,'!',
%%            {atom,1,rareLoggerName},
%%            {tuple,1,
%%             [{call,1,{atom,1,self},[]},
%%              {atom,1,f},
%%              {integer,1,6},
%%              {integer,1,1},
%%              {tuple,1,[{atom,1,exception}]}]}},
%%           {call,1,{atom,1,exit},[{var,1,'X6'}]}]},
%%         {clause,1,
%%          [{tuple,1,[{atom,1,error},{var,1,'X6'},{var,1,'_'}]}],
%%          [],
%%          [{op,1,'!',
%%            {atom,1,rareLoggerName},
%%            {tuple,1,
%%             [{call,1,{atom,1,self},[]},
%%              {atom,1,f},
%%              {integer,1,6},
%%              {integer,1,1},
%%              {tuple,1,[{atom,1,exception}]}]}},
%%           {call,1,{remote,1,{atom,1,erlang},{atom,1,error}},[{var,1,'X6'}]}]},
%%         {clause,1,
%%          [{tuple,1,[{atom,1,throw},{var,1,'X6'},{var,1,'_'}]}],
%%          [],
%%          [{op,1,'!',
%%            {atom,1,rareLoggerName},
%%            {tuple,1,
%%             [{call,1,{atom,1,self},[]},
%%              {atom,1,f},
%%              {integer,1,6},
%%              {integer,1,1},
%%              {tuple,1,[{atom,1,exception}]}]}},
%%           {call,1,{atom,1,throw},[{var,1,'X6'}]}]}],
%%        []}]}]}]}.
