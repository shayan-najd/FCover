-module(clause).
-compile(export_all).
-import(ast,[logData/3]).
-import(pattern,[transPs/2]).
-import(guard,[transGs/2]).
-import(expression,[transE/2,transB/2]).
%------------------------------------------------------------------
% Transform Function Clause
%------------------------------------------------------------------
transFcs(Fcs=[_H|_T],_Name)->
    lists:map(fun(Fc)->transFc(Fc,_Name)end,Fcs).
%-------------------------------------------------
transFc(Fc={clause,L,_Ps,_Gs,_B},_Name) when L=<0, (abs(L) bor 2) == abs(L)->
    Fc; % for generated clauses don't log entry
transFc({clause,Line,Ps,Gs,B},_Name)->
    {clause,-(abs(Line) bor 2)
     ,[{tuple,-(abs(Line) bor 2),transPs(Ps,_Name)}]
     ,transGs(Gs,_Name)
     ,[logData(_Name,Line,funcEntry)| transB(B,_Name)]}.
%------------------------------------------------------------------
% Transform If Clause
%------------------------------------------------------------------
transIcs(Ics=[_H|_T],_Name)->
    lists:map(fun(Ic)->transIc(Ic,_Name)end,Ics).
%-------------------------------------------------		      
transIc(Exp={clause,L,[],_Gs,_B},_Name) when   L=<0,(abs(L) bor 2) == abs(L)->
    Exp;
transIc({clause,Line,[],Gs,B},_Name)->
    {clause,-(abs(Line) bor 2)
     ,[]
     ,transGs(Gs,_Name)
     ,[logData(_Name,Line,clauseEntry)| transB(B,_Name)] }. 
%------------------------------------------------------------------
% Transform Case Clause
%------------------------------------------------------------------
transCcs(Ccs=[_H|_T],_Name)->
    lists:map(fun(Cc)->transCc(Cc,_Name)end,Ccs).
%-------------------------------------------------		      
transCc(Exp={clause,L,_Ps,_Gs,_B},_Name) when  L=<0, (abs(L) bor 2) == abs(L)->
    Exp;
transCc({clause,Line,Ps,Gs,B},_Name)->
    {clause,-(abs(Line) bor 2)
     ,transPs(Ps,_Name)
     ,transGs(Gs,_Name)
     ,[logData(_Name,Line,clauseEntry)| transB(B,_Name)] }.
%------------------------------------------------------------------
% Transform Receive Clause
%------------------------------------------------------------------
transCts(Cts=[_H|_T],_Name)->
    lists:map(fun(Ct)->transCt(Ct,_Name)end,Cts).
%-------------------------------------------------		 
transCt(Exp={clause,L,_Ps,_Gs,_B},_Name) when  L=<0, (abs(L) bor 2) == abs(L)->     
    Exp;
transCt({clause,Line,Ps,Gs,B},_Name) ->
    {clause,-(abs(Line) bor 2)
     ,transPs(Ps,_Name)
     ,transGs(Gs,_Name)
     ,[logData(_Name,Line,clauseEntry)| transB(B,_Name)] }.
%------------------------------------------------------------------
% Transform Receive Clause
%------------------------------------------------------------------
transRcs(Rcs=[_H|_T],_Name)->
    lists:map(fun(Rc)->transRc(Rc,_Name)end,Rcs).
%-------------------------------------------------		      
transRc(Exp={clause,L,_Ps,_Gs,_B},_Name) when  L=<0, (abs(L) bor 2) == abs(L) ->
    Exp;
transRc({clause,Line,Ps,Gs,B},_Name) ->
    {clause,-(abs(Line) bor 2)
     ,transPs(Ps,_Name)
     ,transGs(Gs,_Name)
     ,[logData(_Name,Line,clauseEntry)| transB(B,_Name)] }.
 
