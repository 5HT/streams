-module(streams_meta).
-include("streams.hrl").
-compile(export_all).

gen_api()  -> file:make_dir("src/api"), [ [ begin
                file:make_dir(lists:concat(["src/api/",V])),
                Name = lists:concat([V,"_",S]),
                File = string:join(core:split(core:to_list(element(2,
                                   file:read_file("priv/rest.dtl"))),"{{module}}"),Name),
                file:write_file(lists:concat(["src/api/",V,"/",Name,".erl"]),list_to_binary(File))
              end || S <- SS ] || {N,V,SS} <- core:api() ].

gen_meta() -> file:make_dir("src/meta"), [ begin
                Name = lists:concat(["meta_",V,".erl"]),
                File = string:join(core:split(core:to_list(element(2,
                                   file:read_file("priv/meta.dtl"))),"{{name}}"),lists:concat(["meta_",V])),
                List = "[\n" ++ [ begin
                                  lists:concat(["    #table{name=",
                                                        lists:concat([V,"_",S]),",fields=record_info(fields,",
                                                        lists:concat([V,"_",S]),")}",
                                  case I == length(SS) of false -> ",\n"; _ -> [] end])
                                  end || {S,I} <- lists:zip(SS,lists:seq(1,length(SS))) ] ++ "  ]",
                File1 = string:join(core:split(File,"{{module}}"),List),
                file:write_file(lists:concat(["src/meta/",Name]),list_to_binary(File1))
              end || {N,V,SS} <- core:api() ].

doc_api()  -> io:format(lists:flatten([ io_lib:format("<h2><a name=~s>~s</a></h2>~n<center>Picture ~s. ~s SUBJECTS<br><span><img src='img/~s.svg' width=250></span></center>~n~n",[core:to_list(V),string:to_upper(core:to_list(V)),core:to_list(N),string:to_upper(core:to_list(V)),string:to_upper(core:to_list(V))]) || {N,V,L} <- core:api() ])).
doc()      -> io:format(lists:flatten([ io_lib:format("<p>~s &mdash;<a href='#~s'>~s</a></p>~n",[core:to_list(N),core:to_list(V),string:to_upper(core:to_list(V))]) || {N,V,L} <- core:api() ])).
hrl()      -> [ [ begin io:format("~-34.s",[io_lib:format("-record(~s,",[lists:concat([V,"_",S])])]),
                        io:format("~s",[io_lib:format("{?OPS1(~s)}).~n",  [ core:to_list(V) ])])
                  end || S <- L ] || {N,V,L} <- core:api() ].
usage()    -> lists:foldl(fun({A,N,L},Acc) -> [{A,N,length(L)}|Acc] end, [], core:api()).
api()      -> ?CORE_API.
