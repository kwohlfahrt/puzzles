-module(day5).
-compile(export_all).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Polymer = string:chomp(binary_to_list(Data)),
    Reacted = compact_polymer(Polymer),
    io:format("Part 1: ~B\n", [length(Reacted)]),
    Improved = alter_polymer(Polymer),
    io:format("Part 2: ~B\n", [length(Improved)]),
    erlang:halt(0).
main() -> main(["day5.txt"]).

add_monomer(Monomer, []) -> [Monomer];
add_monomer(Monomer, [Last | Acc]) ->
    case string:equal([Monomer], [Last], true) and not string:equal([Monomer], [Last], false) of
        true -> Acc;
        false -> [Monomer | [Last | Acc]]
    end.
compact_polymer(Polymer) -> lists:foldl(fun add_monomer/2, "", Polymer).

test_altered_polymer(Remove, Polymer) ->
    compact_polymer(
      lists:filter(fun (Monomer) -> not string:equal(string:casefold([Monomer]), Remove) end, Polymer)).

alter_polymer(Polymer) ->
    Monomers = sets:from_list(string:casefold(Polymer)),
    Reduced = lists:map(fun (Monomer) -> test_altered_polymer([Monomer], Polymer) end,
                        sets:to_list(Monomers)),
    Best = util:min_with(Reduced, fun length/1),
    Best.
