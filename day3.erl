-module(day3).
-compile(export_all).

main([Filename]) ->
    io:format("Overlap: ~B\n", [overlap(file:open(Filename, [read]))]),
    erlang:halt(0).

% Phase 1

cloth({Cloth, Claims}) ->
    receive
        {From, readout} -> From ! length(lists:filter(fun (X) -> X =:= 'X' end, maps:values(Cloth)));
        {Id, Origin, Size} -> cloth(add_claim(Id, Origin, Size, {Cloth, Claims}))
    end.
cloth() -> cloth({maps:new(), sets:new()}).

coords({BaseX, Y}, {W, 1}) ->
    Xs = lists:seq(BaseX, BaseX + W - 1),
    lists:map(fun (X) -> {X, Y} end, Xs);
coords({X, BaseY}, {W, H}) ->
    Ys = lists:seq(BaseY, BaseY + H - 1),
    lists:flatmap(fun (Y) -> coords({X, Y}, {W, 1}) end, Ys).

add_claim(Id, Coord, {Cloth, Claims}) ->
    {maps:update_with(Coord, fun (_) -> 'X' end, Id, Cloth), Claims}.
add_claim(Id, Origin, Size, {Cloth, Claims}) ->
    Fn = fun (Coord, Acc) -> add_claim(Id, Coord, Acc) end,
    lists:foldl(Fn, {Cloth, Claims}, coords(Origin, Size)).

add_claims(File, Pid) ->
    case io:get_line(File, "") of
        eof -> Pid ! {self(), readout},
               receive Msg -> Msg
               end;
        Line ->
            [Id, Origin, Size] = string:lexemes(string:chomp(Line), " @#:"),
            [{X, []}, {Y, []}] = lists:map(fun string:to_integer/1, string:split(Origin, ",")),
            [{W, []}, {H, []}] = lists:map(fun string:to_integer/1, string:split(Size, "x")),
            Pid ! {Id, {X + 1, Y + 1}, {W, H}}, add_claims(File, Pid)
    end.

overlap({ok, File}) -> add_claims(File, spawn(day3, cloth, [])).

