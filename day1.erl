-module(day1).
-compile(export_all).

main() ->
    io:format("Sum: ~B\n", [sum_lines(file:open("day1.txt", [read]))]),
    io:format("Calibration: ~B\n", [calibrate_lines(file:open("day1.txt", [read]))]),
    erlang:halt(0).

sum_lines(File, Acc) ->
    case io:get_line(File, "") of
        eof -> Acc;
        Line -> {X, _} = string:to_integer(Line),
                sum_lines(File, Acc + X)
    end.
sum_lines({ok, File}) ->
    sum_lines(File, 0).

calibrate_lines(File, Acc, Seen) ->
    case io:get_line(File, "") of
        eof -> file:position(File, 0),
               calibrate_lines(File, Acc, Seen);
        Line -> {X, _} = string:to_integer(Line),
                case sets:is_element(Acc, Seen) of
                    true -> Acc;
                    false -> calibrate_lines(File, Acc + X, sets:add_element(Acc, Seen))
                end
    end.
calibrate_lines({ok, File}) ->
    calibrate_lines(File, 0, sets:new()).
