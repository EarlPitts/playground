-module(upper).

-export([uppercase/1
        ,uppercase_tail/2
        ,uppercase2/1]).


to_upper(Char) when Char >= $a, Char =< $z ->
    Char + ($A - $a);
to_upper(Char) -> Char.

uppercase([]) -> [];
uppercase([H|T]) -> [to_upper(H)|uppercase(T)].

uppercase_tail([H|T], Acc) ->
    uppercase_tail(T, [to_upper(H)|Acc]);
uppercase_tail([], Acc) ->
    lists:reverse(Acc).

uppercase2(String) ->
    lists:foldr(fun(H,T) -> [to_upper(H)|T] end, [], String).
