-module(writer).
-compile('gen_server stream recorder').
-compile(export_all).
-record(entry, {name,id,prev,next,hdr,body}).
-record(gen_server, {circa,state,file,time,app=1,len,acc_len,size,acc,acc_pred,init,sign,parent}).
-define(LIMIT, 2 * 1000 * 1000).

name(App) -> list_to_atom(lists:concat([writer,App])).
amount() -> ?LIMIT + 100.
msg_size() -> 8048.

emulate_otp(Parent,App) ->
    catch unregister(name(App)),
    catch register(name(App),self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Parent]),
    put('$initial_call', {application_controller, start, 1}).

% gen_server ctor

init(#gen_server{app=App,parent=Parent}=Server) ->
    emulate_otp(Parent,App),
    loop(Parent,{local,?MODULE}, Server, ?MODULE, infinity);
init({Parent, App, N, PredN, Len}) ->
    emulate_otp(Parent,App),
    T = erlang:monotonic_time(milli_seconds),
    Server = #gen_server{file=writer:open(App),parent=Parent,app=App,
                    acc_pred=PredN,acc=N,acc_len=0,state=[],
                    len=Len,circa=[],time=T, init=T,sign= <<>>},
    loop(Parent, {local, ?MODULE}, Server, ?MODULE, infinity).

% gen_server bootstrap

start(App,N,PredN,Len)                    -> spawn(?MODULE, init, [ {self(), App, N,PredN,Len} ]).
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

server(Msg, P, N, S, M) ->
    server({'$gen_cast', [], Msg}, P, N, S, M).

call(Fun,Msg,Sender,S) ->
    case Fun of
         'init'      -> init       (Msg, Sender, S);
         'system'    -> system     (Msg, Sender, S);
         'timeout'   -> timeout    (Msg, Sender, S);
         'EXIT'      -> 'EXIT'     (Msg, Sender, S);
         '$gen_call' -> '$gen_call'(Msg, Sender, S);
         '$gen_cast' -> '$gen_cast'(Msg, Sender, S) end.

dispatch(Call,Sender,P,N,M)   ->
    T = infinity,
    case Call of
	     {stop,R,F}   -> ok;
	     {stop,R,F,S} -> reply(F,R), R;
         {ok,R,S}     -> reply(Sender,R), loop(P,N,S,M,T);
         {ok,S}       -> loop(P,N,S,M,T) end.

% on disk chunk writer

pure_append(App,X) -> F = writer:open(App), file:write(F,X), file:close(F).
append(App,X) ->
    case application:get_env(streams,append,async) of
         async -> spawn(fun() -> pure_append(App,X) end);
             _ -> pure_append(App,X) end.

flush(Msg,Sender,#gen_server{file=F,circa=X,acc_len=AccLen,app=App,
                             acc_pred=PredN,acc=N,time=T1,init=T0}=Server) ->
    append(App,X),
    T2 = erlang:monotonic_time(milli_seconds),
    NewAccLen = round(AccLen/(T2/1000-T1/1000)),
    io:format("Written ~p: rate ~p MB/s messages ~p in ~p sec~n",
       [name(App),round(NewAccLen/1000000),N-PredN,round(T2/1000-T1/1000)]),
    Server#gen_server{acc_len=0,acc_pred=N,acc=N+1,len=NewAccLen*2,time=T2,circa=[]}.

init(Msg,Sender,State) ->
    {ok, State}.

test(App) ->
    file:delete(lists:concat([App])),
    test(App,1,0,50000000).
test(App,N,PredN,Len) ->
    spawn_link(fun() ->
        start(App,N,PredN,Len),
        io:format("whereis writer: ~p~n",[self()]),
        test_pid(App) end).

test_pid(App) ->
    {Timer,_} = timer:tc(fun() ->
                      Loop = fun L(0) -> ok;
                                 L(N) -> try
        gen_server:call(whereis(name(App)),{<<"calahari_msg">>,binary:copy(<<"1">>,msg_size())})
                                         catch E:R-> retry_not_implemented end,
                                         L(N-1) end,
                      Loop(amount()),
                      ok end),
    T = Timer/1000000,
    io:format("~p messages sent in ~ps with ~p m/s rate~n",[amount(),round(T),trunc(amount()/T)]),
    ok.

'$gen_call'(Msg,Sender,#gen_server{state=State,acc=N}) when N >= ?LIMIT ->
    io:format("LIMIT: ~p~n",[{Msg,Sender,N}]),
    {stop, normal, State};

'$gen_call'(Msg,Sender,#gen_server{parent=Parent,len=C,acc=N,acc_len=AccLen}=Server) when AccLen > C ->
    Flush = flush(Msg,Sender,Server#gen_server{acc=N+1}),
    spawn(?MODULE, init, [ Flush ]),
    {stop, normal, Server};

'$gen_call'(Msg,Sender,#gen_server{file=F,acc=N,state=State,len=C,acc_len=AccLen,circa=X,sign=Sign}=Server) ->
    {Y, Len, S} = append(X,{Msg,Sender},Sign),
    {ok, ok, Server#gen_server{acc=N+1,acc_len=AccLen+Len,circa=Y,sign=S}}.

'$gen_cast'(Msg,Sender,#gen_server{parent=Parent,state=State,acc=N}) when N >= ?LIMIT ->
    io:format("LIMIT: ~p~n",[{Msg,Sender,N}]),
    {stop, normal, State};

'$gen_cast'(Msg,Sender,#gen_server{parent=Parent,file=F,acc=N,state=State,len=C,acc_len=AccLen,circa=X}=Server) when AccLen > C ->
    Flush = flush(Msg,Sender,Server#gen_server{acc=N+1}),
    spawn(?MODULE, init, [ Flush ]),
    {stop, normal, Server};

'$gen_cast'(Msg,Sender,#gen_server{file=F,acc=N,circa=X,acc_len=AccLen,sign=Sign}=Server) ->
    {Y, Len, S} = append(X,{Msg,Sender},Sign),
    {ok, Server#gen_server{acc_len=AccLen+Len,acc=N+1,circa=Y,sign=S}}.

'EXIT'(Msg,Sender,State) ->
    io:format("EXIT: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.

timeout(Msg,Sender,State) ->
    io:format("timeout: ~p~n",[{Msg,Sender,State}]),
    {stop, timeout, {Sender,[]}, State}.

system(Msg,Sender,State) ->
    io:format("system: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.

open(App) ->
    {ok, F} = file:open(lists:concat([App]), [raw, binary, append, read, write]),
    F.

secret()                -> application:get_env(streams,secret,<<"ThisIsClassified">>).
append(Circ,Record,SHA) -> signature(SHA,term_to_binary(Record),Circ).
sign(SHA,Bin,Type)      -> crypto:hmac(Type,secret(),<<SHA/binary,Bin/binary>>).
signature(SHA,Bin,Circ) ->
    case application:get_env(streams,signature,none) of
         none -> {[Bin|Circ], size(Bin), <<>>};
         Type -> Sign = sign(SHA,Bin,Type),
                 {[<<Bin/binary,Sign/binary>>|Circ], size(Bin), Sign} end.
