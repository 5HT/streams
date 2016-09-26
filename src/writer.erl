-module(writer).
-compile('Abstract Stream Recorder').
-compile(export_all).
-include("streams.hrl").

amount()     -> ?LIMIT + 100.
block_size() -> 100000000.
msg_size()   -> 10000.

% gen_server ctor

init(#gen_server{app=App,parent=Parent}=Server) ->
    writer_otp:emulate_otp(Parent,App),
    loop(Parent,{local,?MODULE}, Server, ?MODULE, infinity);
init({Parent, App, N, PredN, {Len,Msg}}) ->
    writer_otp:emulate_otp(Parent,App),
    T = erlang:monotonic_time(milli_seconds),
    Server = #gen_server{file=writer:open(App),parent=Parent,app=App,
                    acc_pred=PredN,acc=N,acc_len=0,state=[],msg=Msg,
                    len=Len,circa= <<>>,time=T, init=T,sign= <<>>},
    loop(Parent, {local, ?MODULE}, Server, ?MODULE, infinity).

% gen_server bootstrap

launch(App,N,PredN,Len)                   -> spawn(?MODULE, init, [ {self(),App,N,PredN,Len} ]).
loop(Parent, Name, State, Mod, hibernate) -> erlang:hibernate(?MODULE,hibernate,[Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time)      -> server(drain(Time),        Parent, Name, State, Mod).
hibernate(Parent, Name, State, Mod)       -> server(drain(),            Parent, Name, State, Mod).
drain()                                   -> receive Input -> Input end.
drain(Timeout)                            -> receive Input -> Input after Timeout -> {timeout,[],[]} end.
reply({To,Tag},Reply)                     -> catch To ! {Tag,Reply}.

% gen_server protocol

server({Fun, Sender, Msg}, P, N, S, M) ->
     try dispatch(call(Fun,Msg,Sender,S),Sender,P,N,M)
   catch Error:Reason ->
         io:format("Exception: ~p",[{{Error,Reason},
         erlang:get_stacktrace()}]) end;

server(Msg,P,N,S,M)    ->
   server({'$gen_cast', {self(),[]}, Msg}, P, N, S, M).

call(Fun,Msg,Sender,S) ->
    case Fun of
         'init'        -> writer_otp:'init'(Msg, Sender, S);
         'EXIT'        -> writer_otp:'EXIT'(Msg, Sender, S);
         'system'      -> writer_otp:'system'   (Msg, Sender, S);
         'timeout'     -> writer_otp:'timeout'  (Msg, Sender, S);
         '$gen_call'   -> writer_otp:'$gen_call'(Msg, Sender, S);
         '$gen_cast'   -> writer_otp:'$gen_cast'(Msg, Sender, S) end.

dispatch(Call,Sender,P,N,M)   ->
    T = infinity,
    case Call of
	     {stop,R,F}    -> ok;
	     {stop,R,F,S}  -> reply(F,R), R;
         {ok,R,S}      -> reply(Sender,R),
                          loop(P,N,S,M,T);
         {ok,S}        -> loop(P,N,S,M,T) end.

% tire 0

open(App) -> {ok, F} = file:open(lists:concat([App]), [raw, binary, append, read, write]), F.
empty_append(App,X) -> ok.
pure_append(App,X) -> F = writer:open(App), file:write(F,X), file:close(F).
append(App,X) ->
    case application:get_env(streams,append,async) of
         async -> spawn(fun() -> pure_append(App,X) end);
             _ -> pure_append(App,X) end.

% on disk chunk writer

flush(Msg,Sender,#gen_server{file=F,circa=X,acc_len=AccLen,app=App,
                             acc_pred=PredN,acc=N,time=T1,init=T0}=Server) ->
    append(App,X),
    T2 = erlang:monotonic_time(milli_seconds),
    NewAccLen = round(AccLen/(T2/1000-T1/1000)),
%    NewAccLen = 100000000, % disable variator
    io:format("~p: ~p: rate ~p MB/s messages ~p in ~p/~p sec~n",
       [self(),writer_otp:name(App),round(NewAccLen/1000000),N-PredN,round(T2/1000-T1/1000),round(T2/1000-T0/1000)]),
    Server#gen_server{acc_len=0,acc_pred=N,acc=N+1,len=NewAccLen*2,time=T2,circa= <<>>}.

% server

server(Msg,Sender,#gen_server{acc=N}=Server) when N >= ?LIMIT ->
    io:format("LIMIT: ~p~n",[{Msg,Sender,N}]),
    {stop, normal, Server};

server(Msg,Sender,#gen_server{acc=N,len=C,acc_len=AccLen}=Server) when AccLen > C ->
    spawn(?MODULE, init, [ flush(Msg,Sender,Server#gen_server{acc=N+1}) ]),
    {stop, normal, Server};

server(Msg,Sender,#gen_server{acc=N,circa=X,acc_len=AccLen,sign=Sign}=Server) ->
    {Y, Len, S} = append(X,{Msg,Sender},Sign,N),
    {ok, [], Server#gen_server{acc_len=AccLen+Len,acc=N+1,circa=Y,sign=S}}.

% client

start() -> [begin writer:start(I),timer:sleep(250)end || I <- [1,2,3,4]].

start(App) ->
    file:delete(lists:concat([App])),
    start(App,1,0,{block_size(),msg_size()}).
start(App,N,PredN,{Len,Msg}) ->
    spawn_link(fun() ->
        launch(App,N,PredN,{Len,Msg}),
        io:format("whereis writer: ~p~n",[self()]),
        test_pid(App) end).

test_pid(App) ->
    {Timer,_} = timer:tc(fun() ->
                      Loop = fun L(0) -> ok;
                                 L(N) -> try
%        gen_server:call(whereis(writer_otp:name(App)),binary:copy(<<"1">>,msg_size()))
         whereis(writer_otp:name(App)) ! binary:copy(<<"1">>,?MODULE:msg_size())
                                         catch E:R-> retry_not_implemented end,
                                         L(N-1) end,
                      Loop(amount()),
                      ok end),
    T = Timer/1000000,
    io:format("~p messages sent in ~ps with ~p m/s rate~n",[amount(),round(T),trunc(amount()/T)]),
    ok.

secret()                  -> application:get_env(streams,secret,<<"ThisIsClassified">>).
append(Circ,Record,SHA,N) -> signature(SHA,term_to_binary(Record),Circ,N).
sign(SHA,Bin,Type)        -> crypto:hmac(Type,secret(),<<SHA/binary,Bin/binary>>).
signature(SHA,Bin,Circ,N) ->
    case application:get_env(streams,signature,none) of
         none -> {[Bin|Circ], size(Bin), <<>>};
         Type -> Sign = sign(SHA,Bin,Type),
                 {[<<Bin/binary,Sign/binary>>|Circ], size(Bin), Sign} end.
