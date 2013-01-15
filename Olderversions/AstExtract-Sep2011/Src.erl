-module('Src').
f() ->
    try begin
          ExpUnique_2 = {},
          case ExpUnique_2 of
            {} ->
                rareLoggerName !
                  {self(), f, 2, 1,
                   {{"({})",
                     {{"case ExpUnique_2 of\n  {} -> true;\n "
                       " _ -> false\nend",
                       case ExpUnique_2 of
                         {} -> true;
                         _ -> false
                       end}}}}};
            _ ->
                rareLoggerName !
                  {self(), f, 2, 1,
                   {{"({})",
                     {{"case ExpUnique_2 of\n  {} -> true;\n "
                       " _ -> false\nend",
                       case ExpUnique_2 of
                         {} -> true;
                         _ -> false
                       end}}}}}
          end,
          case ExpUnique_2 of
            _ ->
                rareLoggerName !
                  {self(), f, 2, 1,
                   {{"(_)",
                     {{"case ExpUnique_2 of\n  _ -> true;\n "
                       " _ -> false\nend",
                       case ExpUnique_2 of
                         _ -> true;
                         _ -> false
                       end}}}}};
            _ ->
                rareLoggerName !
                  {self(), f, 2, 1,
                   {{"(_)",
                     {{"case ExpUnique_2 of\n  _ -> true;\n "
                       " _ -> false\nend",
                       case ExpUnique_2 of
                         _ -> true;
                         _ -> false
                       end}}}}}
          end,
          case ExpUnique_2 of
            {} ->
                rareLoggerName ! {self(), f, 3, 1, {clauseEntry}},
                try rareLoggerName ! {self(), f, 1, 1, {funcEntry}}
                catch
                  exit:X4 ->
                      rareLoggerName ! {self(), f, 4, 1, {exception}},
                      exit(X4);
                  error:X4 ->
                      rareLoggerName ! {self(), f, 4, 1, {exception}},
                      erlang:error(X4);
                  X4 ->
                      rareLoggerName ! {self(), f, 4, 1, {exception}},
                      throw(X4)
                end,
                fff;
            _ ->
                rareLoggerName ! {self(), f, 5, 1, {clauseEntry}},
                try erlang:error(function_clause) catch
                  exit:X6 ->
                      rareLoggerName ! {self(), f, 6, 1, {exception}},
                      exit(X6);
                  error:X6 ->
                      rareLoggerName ! {self(), f, 6, 1, {exception}},
                      erlang:error(X6);
                  X6 ->
                      rareLoggerName ! {self(), f, 6, 1, {exception}},
                      throw(X6)
                end
          end
        end
    catch
      exit:X7 ->
          rareLoggerName ! {self(), f, 7, 1, {exception}},
          exit(X7);
      error:X7 ->
          rareLoggerName ! {self(), f, 7, 1, {exception}},
          erlang:error(X7);
      X7 ->
          rareLoggerName ! {self(), f, 7, 1, {exception}},
          throw(X7)
    end.
