-module(writer).
-compile('Toy Gen Server').
-compile(export_all).
-record(entry, {name,id,prev,next,hdr,body}).
-record(gen_server, {circa,state,file,time,app=1,len,acc_len,size,acc,acc_pred,init,sign,parent}).

name(App) -> list_to_atom(lists:concat([writer,App])).

otp(Parent,App) ->
    catch unregister(name(App)),
    catch register(name(App),self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Parent]),
    put('$initial_call', {application_controller, start, 1}).

init(#gen_server{app=App,parent=Parent}=Server) ->
    otp(Parent,App),
    io:format("start ~p~n",[whereis(name(App))]),
    loop(Parent,{local,?MODULE}, Server, ?MODULE, infinity);
init({Parent, App, N, PredN, Len}) ->
    otp(Parent,App),
    io:format("startI ~p~n",[whereis(name(App))]),
    T = erlang:monotonic_time(milli_seconds),
    Server = #gen_server{file=writer:open(App),parent=Parent,app=App,
                    acc_pred=PredN,acc=N,acc_len=0,state=[],
                    len=Len,circa=[],time=T, init=T,sign= <<>>},
    loop(Parent, {local, ?MODULE}, Server, ?MODULE, infinity).

start(App,N,PredN,Len)                    -> spawn(?MODULE, init, [ {self(), App, N,PredN,Len} ]).
loop(Parent, Name, State, Mod, hibernate) -> erlang:hibernate(?MODULE,hibernate,[Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time)      -> server(drain(Time),        Parent, Name, State, Mod).
hibernate(Parent, Name, State, Mod)       -> server(drain(),            Parent, Name, State, Mod).
drain()                                   -> receive Input -> Input end.
drain(Timeout)                            -> receive Input -> Input after Timeout -> {timeout,[],[]} end.
reply({To,Tag},Reply)                     -> catch To ! {Tag,Reply}.

server({Fun, Sender, Msg}, P, N, S, M) ->
    try dispatch(call(Fun,Msg,Sender,S),Sender,P,N,M)
  catch Error:Reason ->
        io:format("Exception: ~p",[{{Error,Reason},
        erlang:get_stacktrace()}]) end;

server(Msg, P, N, S, M) ->
    server({'$gen_cast', [], Msg}, P, N, S, M).

call(Fun,Msg,Sender,S) ->
    ?MODULE:Fun(Msg, Sender, S).
%    case Fun of
%         'init'      -> init       (Msg, Sender, S);
%         'system'    -> system     (Msg, Sender, S);
%         'timeout'   -> timeout    (Msg, Sender, S);
%         'EXIT'      -> ?MODULE:'EXIT'     (Msg, Sender, S);
%         '$gen_call' -> '$gen_call'(Msg, Sender, S);
%         '$gen_cast' -> '$gen_cast'(Msg, Sender, S) end.

dispatch(Call,Sender,P,N,M)   ->
    T = infinity,
    case Call of
	     {stop,R,F}   -> ok;
	     {stop,R,F,S} -> reply(F,R), R;
         {ok,R,S}     -> reply(Sender,R), loop(P,N,S,M,T);
         {ok,S}       -> loop(P,N,S,M,T) end.

% gen server protocol

flush(Msg,Sender,#gen_server{file=F,circa=X,acc_len=AccLen,app=App,
                             acc_pred=PredN,acc=N,time=T1,init=T0}=Server) ->
    io:format("flush ~p: ~p~n",[name(App),Msg]),
    erlang:put(n,N),
    erlang:put(predn,PredN),
%    erlang:spawn_link(fun() ->
                          FILE = writer:open(App),
                          file:write(FILE, X),
                          file:close(FILE),
%                          ok end),
    T2 = erlang:monotonic_time(milli_seconds),
    NewAccLen = round(AccLen/(T2/1000-T1/1000)),
    erlang:put(len,NewAccLen*2),
    io:format("Written ~p: ~pMB with Rate ~pMB/s (messages=~p) in ~p~n",
       [name(App),trunc(AccLen/1000000),round(NewAccLen/1000000),N-PredN,round(T2/1000-T0/1000)]),
    Server#gen_server{acc_len=0,acc_pred=N,acc=N+1,len=NewAccLen*2,time=T2,circa=[]}.

init(Msg,Sender,State) ->
    {ok, State}.

test(App) ->
    file:delete(lists:concat([App])),
    test(App,1,0,30000000).
test(App,N,PredN,Len) ->
    spawn_link(fun() ->
        start(App,N,PredN,Len),
        io:format("whereis writer: ~p~n",[self()]),
        test_pid(App) end).

test_pid(App) ->
    {Timer,_} = timer:tc(fun() ->
                      Loop = fun L(0) -> ok;
                                 L(N) -> try gen_server:call(whereis(name(App)),{<<"calahari",1,2,3,4,5,6,7,8,1,1,1,1,1>>,erlang:monotonic_time(milli_seconds)})
                                         catch E:R->io:format("Except: ~p~n",[{E,R}]) end,
                                         L(N-1) end,
                      Loop(amount()),
%                      Pid ! {close},
                      ok end),
    T = Timer/1000000,
    io:format("~p messages sent in ~ps with ~p m/s rate~n",[amount(),round(T),trunc(amount()/T)]),
    ok.

filesize() -> {ok,{file_info,S,_,_,_,_,_,_,_,_,_,_,_,_}}=file:read_file_info(lists:concat([node()])), S.
amount() -> 2 * 1000 * 1000 + 100.

'$gen_call'(Msg,Sender,#gen_server{state=State,acc=N}) when N >= 10000000 ->
    io:format("LIMIT: ~p~n",[{Msg,Sender,N}]),
    {stop, normal, State};

'$gen_call'(Msg,Sender,#gen_server{parent=Parent,len=C,acc=N,acc_len=AccLen}=Server) when AccLen > C ->
    Flush = flush(Msg,Sender,Server#gen_server{acc=N+1}),
    spawn(?MODULE, init, [ Flush ]),
    {stop, normal, Server};
%    {ok, ok, flush(Msg,Sender,Server)};

'$gen_call'(Msg,Sender,#gen_server{file=F,acc=N,state=State,len=C,acc_len=AccLen,circa=X,sign=Sign}=Server) ->
    {Y, Len, S} = append(X,{Msg,Sender},Sign),
    {ok, ok, Server#gen_server{acc=N+1,acc_len=AccLen+Len,circa=Y,sign=S}}.

'$gen_cast'(Msg,Sender,#gen_server{parent=Parent,state=State,acc=N}) when N >= 10000000 ->
    io:format("LIMIT: ~p~n",[{Msg,Sender,N}]),
    {stop, normal, State};

'$gen_cast'(Msg,Sender,#gen_server{parent=Parent,file=F,acc=N,state=State,len=C,acc_len=AccLen,circa=X}=Server) when AccLen > C ->
    Flush = flush(Msg,Sender,Server#gen_server{acc=N+1}),
    spawn(?MODULE, init, [ Flush ]),
    {stop, normal, Server};
%    {ok, Flush};

'$gen_cast'(Msg,Sender,#gen_server{file=F,acc=N,circa=X,acc_len=AccLen,sign=Sign}=Server) ->
    {Y, Len, S} = append(X,{Msg,Sender},Sign),
    {ok, Server#gen_server{acc_len=AccLen+Len,acc=N+1,circa=Y,sign=S}}.

'EXIT'(Msg,Sender,State) ->
    io:format("EXIT: ~p~n",[{Msg,Sender,State}]),
    N = get(n),
    PredN = get(predn),
    Len = get(len),
    {stop, Msg, {Sender,[]}, State}.

timeout(Msg,Sender,State) ->
    io:format("timeout: ~p~n",[{Msg,Sender,State}]),
    {stop, timeout, {Sender,[]}, State}.

system(Msg,Sender,State) ->
    io:format("system: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.

open(App) ->
    {ok, F} = file:open(lists:concat([App]), [binary, append, read, write, raw]),
    F.

append(Circ,Record,SHA) ->
    Bin  = term_to_binary(Record),
    Size = size(Bin),
    Sign = <<1>>,%crypto:hash(md4,<<SHA/binary,Bin/binary>>),
    {[<<Bin/binary,Sign/binary>>|Circ], Size, Sign}.

