-module(day2).
-compile(export_all).

main(Args) ->
    Filename = lists:nth(1, Args),
    io:format("Checksum: ~B\n", [checksum_lines(file:open(Filename, [read]))]),
    erlang:halt(0).

%% Phase 1

checksum_line([], Sum) ->
    F = fun (_, 2, {_, Triples}) -> {1, Triples};
            (_, 3, {Doubles, _}) -> {Doubles, 1};
            (_, _, Acc) -> Acc
        end,
    dict:fold(F, {0, 0}, Sum);
checksum_line([C|Rest], Sum) ->
    checksum_line(Rest, dict:update_counter(C, 1, Sum)).

checksum1({Doubles, Triples}) ->
    receive
        {From, readout} -> From ! (Doubles * Triples);
        Line -> {NewDoubles, NewTriples} = checksum_line(Line, dict:new()),
                checksum1({Doubles + NewDoubles, Triples + NewTriples})
    end.
checksum() -> checksum1({0, 0}).

update_checksum(Pid, Line) -> Pid ! Line.
get_checksum(Pid) ->
    Pid ! {self(), readout},
    receive Msg -> Msg end.

checksum_lines(File, Pid) ->
    case io:get_line(File, "") of
        eof -> get_checksum(Pid);
        Line -> update_checksum(Pid, Line),
                checksum_lines(File, Pid)
    end.
checksum_lines({ok, File}) -> checksum_lines(File, spawn(day2, checksum, [])).
