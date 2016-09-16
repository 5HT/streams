-module(streams_endpoint).
-include("streams.hrl").
-compile(export_all).

rest_init(Req, _Opts) ->
    {Method, Reqx}    = cowboy_req:method(Req),
    {ok, Reqx,  #core { module   = i,
                        method   = name(Method),
                        req      = Reqx }}.

name(<<"POST">>)    -> post.
allowed_methods(Req, State) -> {[ <<"POST">>, <<"GET">> ], Req, State}.
content_types_accepted(Req, State) -> {[{<<"application/json">>, handle_json_data}], Req, State}.
handle_json_data(Req, #core{module = M, resource = Id} = State) ->
    {ok, Binary, Req2} = cowboy_req:body(Req),
    {Flag,Parsed} = handle_data(Binary, State),
    Req3 = cowboy_req:set_resp_body("OK\n",Req2),
    {Flag,Req3,State}.

init(_, _, _)                                               -> {upgrade, protocol, cowboy_rest}.
resource_exists(Req,  #core{module = M } = S)               -> { true, Req, S}.
handle_data(Data,   #core{module = M, method=Method} = S) -> M:Method(Data,S).

post(Data,State) ->
    io:format(Data),
    {true,[]}.
