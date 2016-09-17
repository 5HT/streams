-module(writer).
-compile(export_all).
-define(BACK,file).

main() ->
    {ok, File} = ?BACK:open("data.log", [read,binary]),
    Lines = count_lines(File, 0),
    io:format("Found ~w lines.~n", [Lines]).

count_lines(File, Count) ->
    case ?BACK:read(File,8192) of
        {ok, Line} ->
            TC = length(binary:split(Line,[<<10>>],[global])),
            count_lines(File, Count+TC);
        _ ->
            Count
    end.

open() -> {ok, F} = ?BACK:open(lists:concat([node()]), [raw, append, binary, write]), F.
append(Circ,Record) -> ecirca:push_list(Circ, binary_to_list(term_to_binary(Record))).
