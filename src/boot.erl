-module(boot).
-compile('Toy Gen Server').
-compile(export_all).
-record(entry, {name,id,prev,next,hdr,body}).

% minimal compatibility with OTP's gen_server requires $ancestors and $initial_call

init(Parent, App) -> %process_flag(trap_exit, true),
                     file:delete(lists:concat([node()])),
                     put('$ancestors', [Parent]),
                     put('$initial_call', {application_controller, start, 1}),
                     {ok,X} = ecirca:new(100000000, last, medium, []),
                     F = {writer:open(),0,[],100000,X},
                     loop(Parent, {local, ?MODULE}, F, ?MODULE, infinity).

start(App)                                -> spawn_link(?MODULE, init, [self(), App]).
loop(Parent, Name, State, Mod, hibernate) -> erlang:hibernate(?MODULE,hibernate,[Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time)      -> server(drain(Time),        Parent, Name, State, Mod).
hibernate(Parent, Name, State, Mod)       -> server(drain(),            Parent, Name, State, Mod).
drain()                                   -> receive Input -> Input end.
drain(Timeout)                            -> receive Input -> Input after Timeout -> {timeout,[],[]} end.
reply({To,Tag},Reply)                     -> To ! {Tag,Reply}.

server({Fun, Sender, Msg}, P, N, S, M) ->
    try dispatch(call(Fun,Msg,Sender,S),Sender,P,N,M)
  catch Error:Reason -> io:format("Exception: ~p",[erlang:get_stacktrace()]) end;

server(Msg, P, N, S, M) ->
    server({'$gen_cast', [], Msg}, P, N, S, M).

call(Fun,Msg,Sender,S) ->
%    ?MODULE:Fun(Msg, Sender, S).
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
	     {stop,R,F,S} -> reply(F,R), R;
         {ok,R,S}     -> reply(Sender,R), loop(P,N,S,M,T);
         {ok,S}       -> loop(P,N,S,M,T) end.

% gen server protocol

flush(F,Msg,Sender,State,C,X) ->
    io:format("flush: ~p~n",[{Msg,X}]),
    {ok,Bin} = ecirca:save(X),
    file:write(F,Bin),
    {ok, {F,0,[],C,X}}.

init(Msg,Sender,State)              -> {ok, State}.
test(Pid) ->
   {Timer,_} = timer:tc(fun() ->
                      Loop = fun L(0) -> ok;
                                 L(N) -> Pid ! {<<"cal",1,2,3,4,5,6,7,8,1,1,1,1,1>>},
                                         L(N-1) end,
                      Loop(amount()),
                      Pid ! {close},
                      ok end),
   T = Timer/1000000,
   io:format("~p messages sent in ~ps with ~p m/s rate~n",[amount(),round(T),trunc(amount()/T)]),
   io:format("Disk write performance: ~p MB/s~n",[round(filesize()/T/1000000)]).

filesize() -> {ok,{file_info,S,_,_,_,_,_,_,_,_,_,_,_,_}}=file:read_file_info(lists:concat([node()])), S.
amount() -> 2 * 1000 * 1000 + 100.

'$gen_call'({close},Sender,{F,N,State,_,_})        -> {ok, State};
'$gen_call'(Msg,Sender,{F,N,State,C,X}) when N > C -> flush(F,Msg,Sender,State,C,X);
'$gen_call'(Msg,Sender,{F,N,State,C,X})            -> {ok, {F,N+1,writer:append(X,{call,Sender,Msg}),C,X}}.

'$gen_cast'({close},Sender,{F,N,State,_,_})        -> {ok, State};
'$gen_cast'(Msg,Sender,{F,N,State,C,X}) when N > C -> flush(F,Msg,Sender,State,C,X);
'$gen_cast'(Msg,Sender,{F,N,State,C,X})            -> {ok, {F,N+1,writer:append(X,{cast,Sender,Msg}),C,X}}.

'EXIT'(Msg,Sender,State)            -> io:format("EXIT: ~p~n",[{Msg,Sender,State}]),
                                       {stop, Msg, {Sender,[]}, State}.

timeout(Msg,Sender,State)           -> io:format("timeout: ~p~n",[{Msg,Sender,State}]),
                                       {stop, timeout, {Sender,[]}, State}.

system(Msg,Sender,State)            -> io:format("system: ~p~n",[{Msg,Sender,State}]),
                                       {stop, Msg, {Sender,[]}, State}.
