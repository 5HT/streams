-module(boot).
-compile('Toy Gen Server').
-compile(export_all).

% minimal compatibility with OTP's gen_server requires $ancestors and $initial_call

init(Parent, App) -> process_flag(trap_exit, true),
                     put('$ancestors', [Parent]),
                     put('$initial_call', {application_controller, start, 1}),
                     self() ! {init,self(),{}},
                     loop(Parent, {local, ?MODULE}, {}, ?MODULE, infinity).

start(App)                                -> spawn_link(?MODULE, init, [self(), App]).
loop(Parent, Name, State, Mod, hibernate) -> erlang:hibernate(?MODULE,hibernate,[Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time)      -> ?MODULE:server(drain(Time),        Parent, Name, State, Mod).
hibernate(Parent, Name, State, Mod)       -> ?MODULE:server(drain(),            Parent, Name, State, Mod).
drain()                                   -> receive Input -> Input end.
drain(Timeout)                            -> receive Input -> Input after Timeout -> {timeout,[],[]} end.
reply({To,Tag},Reply)                     -> To ! {Tag,Reply}.

server({Fun, Sender, Msg}, P, N, S, M) ->
    try case M:Fun(Msg, Sender, S) of
	         {stop, Status, Tag,   NS} -> reply(Tag,Status), Status;
             {ok,           Reply, NS} -> reply(Sender,Reply), ?MODULE:loop(P, N, NS, M, infinity);
             {ok,                  NS} ->                      ?MODULE:loop(P, N, NS, M, infinity) end
                    catch Error:Reason -> reply(Sender,{Error,Reason,erlang:get_stacktrace()}) end;
server(Msg, P, N, S, M)                -> server({'$gen_cast', [], Msg}, P, N, S, M).

% gen server protocol

init(Msg,Sender,State)              -> {ok, State}.

'$gen_call'(Msg,Sender,State)       -> io:format("$gen_call: ~p~n",[{Msg,Sender,State}]),
                                       {ok, Sender, State}.

'$gen_cast'(Msg,Sender,State)       -> io:format("$gen_cast: ~p~n",[{Msg,Sender,State}]),
                                       {ok, State}.

'EXIT'(Msg,Sender,State)            -> io:format("EXIT: ~p~n",[{Msg,Sender,State}]),
                                       {stop, Msg, {Sender,[]}, State}.

timeout(Msg,Sender,State)           -> io:format("timeout: ~p~n",[{Msg,Sender,State}]),
                                       {stop, timeout, {Sender,[]}, State}.

system(Msg,Sender,State)            -> io:format("system: ~p~n",[{Msg,Sender,State}]),
                                       {stop, Msg, {Sender,[]}, State}.
