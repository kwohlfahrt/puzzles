-module(util).
-compile(export_all).

max_with(First, Second, Fun) ->
    case Fun(First) > Fun(Second) of
        true -> First;
        false -> Second
    end.
max_with(List, Fun) ->
    {[Head], Tail} = lists:split(1, List),
    lists:foldl(fun (Value, Acc) -> max_with(Value, Acc, Fun) end, Head, Tail).

min_with(First, Second, Fun) ->
    case Fun(First) < Fun(Second) of
        true -> First;
        false -> Second
    end.
min_with(List, Fun) ->
    {[Head], Tail} = lists:split(1, List),
    lists:foldl(fun (Value, Acc) -> min_with(Value, Acc, Fun) end, Head, Tail).
