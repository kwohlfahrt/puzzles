-module(day4).
-compile(export_all).

main([Filename]) ->
    [Guard, Minute] = analyze_schedule(file:open(Filename, [read])),
    io:format("Part 1: ~B\n", [Guard * Minute]),
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
        {readout, Id, Pid} ->
            MinutesAsleep = orddict:fold(fun (_, Value, Acc) -> Value + Acc end, 0, Record),
            MostAsleep =
                try max_with(orddict:to_list(Record), fun ({_, Value}) -> Value end) of
                    {Minute, _} -> Minute
                catch
                    error:badarg -> nil
                end,
            Pid ! {Id, MinutesAsleep, MostAsleep}
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
            dict:map(fun (Key, Value) -> Value ! {readout, Key, self()} end, Guards),
            Pid ! guards_recv(dict:size(Guards))
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
    case io:get_line(File, "") of
        eof -> Schedule;
        Line -> {Timestamp, Action} = parse(string:chomp(Line)),
                parse_schedule(File, orddict:store(Timestamp, Action, Schedule))
    end.
parse_schedule(File) -> parse_schedule(File, orddict:new()).

analyze_schedule({ok, File}) ->
    Schedule = parse_schedule(File),
    Guards = spawn(?MODULE, guards, []),
    orddict:fold(fun (Key, Value, Acc) -> Guards ! {Key, Value}, Acc end, nil, Schedule),
    Guards ! {readout, self()},
    receive {Id, _, MostAsleep} -> [Id, MostAsleep] end.
