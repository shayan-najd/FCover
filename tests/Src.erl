-module('Src').

f() ->{clause,1,
       [{error,
	 {var,1,'XUnique_7'}}],
       [],
       [{'case',1,
	 {var,1,'XUnique_7'},
	 [{clause,1,
	   [{tuple,1,
	     [{var,1,'_'},
	      {var,1,'_'},
	      {var,1,'_'}]}],
	   [],
	   [{op,0,'!',
	     {atom,0,
	      rareLoggerName},
	     {tuple,0,
	      [{call,0,...},
	       {atom,...},
	       {...}|...]}}]},
	  {clause,1,
	   [{var,1,'_'}],
	   [],
	   [{op,0,'!',
	     {atom,0,
	      rareLoggerName},
	     {tuple,0,
	      [{call,...},
	       {...}|...]}}]}]},
	{'try',1,
	 [{call,1,
	   {remote,1,
	    {atom,1,erlang},
	    {atom,1,error}},
	   [{var,1,'XUnique_7'}]}],
	 [],
	 [{clause,1,
	   [{tuple,1,
	     [{var,1,'_'},
	      {var,1,'_'},
	      {var,1,'_'}]}],
	   [],
	   [{atom,1,false}]}],
	 []}]}
