-module(day4).
-compile(export_all).

main([Filename]) ->
    [Guard, Minute] = analyze_schedule(file:open(Filename, [read])),
    io:format("Part 1: ~B\n", [Guard * Minute]),
    erlang:halt(0).

parse(Line) ->
    N = 18,
    Timestamp = string:trim(string:slice(Line, 0, N), both, "[]"),
    [Date, Time] = string:split(Timestamp, " "),
    Text = string:slice(Line, N + 1, infinity),
    Action = case string:prefix(Text, "Guard #") of
                 nomatch -> Text;
                 Description ->
                     {Guard, " begins shift"} = string:to_integer(Description),
                     {Guard}
             end,
    {Date, Time, Action}.

parse_schedule(File, Acc) ->
    case io:get_line(File, "") of
        eof -> Acc;
        Line -> {Date, Time, Action} = parse(string:chomp(Line)),
                parse_schedule(File, orddict:store({Date, Time}, Action, Acc))
    end.
parse_schedule(File) -> parse_schedule(File, orddict:new()).

analyze_schedule({ok, File}) ->
    Schedule = parse_schedule(File),
    Size = orddict:size(Schedule),
    [Size, Size].
