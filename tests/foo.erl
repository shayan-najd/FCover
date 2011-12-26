-module(foo).
-compile(export_all).
-compile(debug_info).

foo() ->
    ets:update_counter(foo,foo,2),
    1+2.
