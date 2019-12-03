-module(day2).
-compile(export_all).

main([Filename]) ->
    io:format("Checksum: ~B\n", [checksum_lines(file:open(Filename, [read]))]),
    {Prefix, Suffix} = search_lines(file:open(Filename, [read])),
    io:format("Crates: ~s~s\n", [Prefix, Suffix]),
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
    case file:read_line(File) of
        eof -> get_checksum(Pid);
        {ok, Line} -> update_checksum(Pid, Line),
                checksum_lines(File, Pid)
    end.
checksum_lines({ok, File}) -> checksum_lines(File, spawn(day2, checksum, [])).

search1(Seen) ->
    receive
        {From, eof} -> From ! not_found;
        {From, Line} ->
            New = sets:from_list(splits(Line)),
            Intersection = sets:intersection(Seen, New),
            case sets:size(Intersection) of
                0 -> search1(sets:union(Seen, New));
                1 -> [Element] = sets:to_list(Intersection),
                     From ! Element
            end

    end.
search() -> search1(sets:new()).

splits2(String, Pos) ->
    case Pos < string:length(String) of
        true -> [{string:slice(String, 0, Pos), string:slice(String, Pos + 1)}] ++ splits2(String, Pos + 1);
        false -> []
    end.
splits(String) -> splits2(String, 0).

search_lines(File, Pid) ->
    case file:read_line(File) of
        eof -> Pid ! {self(), eof},
               receive Msg -> Msg
               end;
        {ok, Line} -> Pid ! {self(), string:chomp(Line)}, search_lines(File, Pid)
    end.
search_lines({ok, File}) -> search_lines(File, spawn(day2, search, [])).
