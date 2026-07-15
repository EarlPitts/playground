-module(atoi).

-export([atoi/1
        ,atoi2/1
        ,to_string/1]).

% public
atoi([$- | String]) -> % negative
    -1 * atoi(String, 0);
atoi(String) -> % non-negative
    atoi(String, 0).

% internal
atoi([], Acc) ->
    Acc;
atoi([C | Rest], Acc) when C >= $0, C =< $9 ->
    atoi(Rest, 10 * Acc + (C - $0)).

atoi2(String) ->
    lists:foldl(fun(X,Acc) -> 10 * Acc + (X - $0) end, 0, String).

% public
to_string(0) ->
    [$0];
to_string(Integer) when Integer < 0 -> % negative
    [$-|to_string(-1 * Integer, [])];
to_string(Integer) -> % positive
    to_string(Integer, []).

% internal
to_string(0, Acc) ->
    Acc;
to_string(Integer, Acc) ->
    to_string(Integer div 10, [(Integer rem 10) + $0 | Acc]).
