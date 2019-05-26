-module(day4).
-compile(export_all).

main([Filename]) ->
    {Part1, Part2} = analyze_schedule(file:open(Filename, [read])),
    io:format("Part 1: ~B\n", [Part1]),
    io:format("Part 2: ~B\n", [Part2]),
    erlang:halt(0).

max_with(First, Second, Fun) ->
    case Fun(First) > Fun(Second) of
        true -> First;
        false -> Second
    end.
max_with(List, Fun) ->
    {[Head], Tail} = lists:split(1, List),
    lists:foldl(fun (Value, Acc) -> max_with(Value, Acc, Fun) end, Head, Tail).

guard(awake, Record) ->
    receive
        {Minute, "falls asleep"} -> guard(asleep, Minute, Record);
        {readout1, Id, Pid} ->
            MinutesAsleep = orddict:fold(fun (_, Value, Acc) -> Value + Acc end, 0, Record),
            MostAsleep =
                try max_with(orddict:to_list(Record), fun ({_, Value}) -> Value end) of
                    {Minute, _} -> Minute
                catch
                    error:badarg -> nil
                end,
            Pid ! {Id, MinutesAsleep, MostAsleep},
            guard(awake, Record);
        {readout2, Id, Pid} ->
            {MinuteAsleep, AmountAsleep} =
                try max_with(orddict:to_list(Record), fun ({_, Value}) -> Value end)
                catch
                    error:badarg -> {nil, 0}
                end,
            Pid ! {Id, AmountAsleep, MinuteAsleep}
    end.
guard(asleep, Since, Record) ->
    receive
        {Minute, "wakes up"} ->
            NewRecord = lists:foldl(fun (Elem, Acc) -> orddict:update_counter(Elem, 1, Acc) end,
                                    Record, lists:seq(Since, Minute)),
            guard(awake, NewRecord)
    end.
guard() -> guard(awake, orddict:new()).


guards_recv(Acc, 0) -> Acc;
guards_recv(Acc, Remaining) ->
    receive
        Msg -> NewAcc = max_with(Acc, Msg, fun({_, MinAsleep, _}) -> MinAsleep end),
               guards_recv(NewAcc, Remaining - 1)
    end.
guards_recv(Remaining) ->
    receive
        Msg -> guards_recv(Msg, Remaining - 1)
    end.

guards(Active, Guards) ->
    receive
        {{_, _, Minute}, Action} ->
            {NewActive, NewGuards} = case string:prefix(Action, "Guard #") of
                nomatch -> Active ! {Minute, Action}, {Active, Guards};
                Description ->
                    {Guard, " begins shift"} = string:to_integer(Description),
                    case dict:find(Guard, Guards) of
                        {ok, Value} -> {Value, Guards};
                        error -> Value = spawn(?MODULE, guard, []),
                                 {Value, dict:store(Guard, Value, Guards)}
                    end
            end,
            guards(NewActive, NewGuards);
        {readout, Pid} ->
            dict:map(fun (Key, Value) -> Value ! {readout1, Key, self()} end, Guards),
            Part1 = guards_recv(dict:size(Guards)),
            dict:map(fun (Key, Value) -> Value ! {readout2, Key, self()} end, Guards),
            Part2 = guards_recv(dict:size(Guards)),
            Pid ! [Part1, Part2]
    end.
guards() -> guards(none, dict:new()).

parse(Line) ->
    N = string:length("[YYYY-MM-DD HH:MM]"),
    Timestamp = string:trim(string:slice(Line, 0, N), both, "[]"),
    Action = string:trim(string:slice(Line, N), both),
    [Date, Time] = string:split(Timestamp, " "),
    [{Hours, ""}, {Minutes, ""}] = lists:map(fun string:to_integer/1, string:split(Time, ":")),
    {{Date, Hours, Minutes}, Action}.

parse_schedule(File, Schedule) ->
    case file:read_line(File) of
        eof -> Schedule;
        {ok, Line} -> {Timestamp, Action} = parse(string:chomp(Line)),
                parse_schedule(File, orddict:store(Timestamp, Action, Schedule))
    end.
parse_schedule(File) -> parse_schedule(File, orddict:new()).

analyze_schedule({ok, File}) ->
    Schedule = parse_schedule(File),
    Guards = spawn(?MODULE, guards, []),
    orddict:fold(fun (Key, Value, Acc) -> Guards ! {Key, Value}, Acc end, nil, Schedule),
    Guards ! {readout, self()},
    [Part1, Part2] = receive Msg -> lists:map(fun ({Id, _, MinuteAsleep}) -> Id * MinuteAsleep end, Msg) end,
    {Part1, Part2}.
