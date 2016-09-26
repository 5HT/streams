-module(writer_otp).
-description('Erlang/OTP compatibility layer').
-compile(export_all).
-include("streams.hrl").

name(App) ->
    list_to_atom(lists:concat([writer,App])).

emulate_otp(Parent,App) ->
    catch unregister(name(App)),
    catch register(name(App),self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Parent]),
    put('$initial_call', {application_controller, start, 1}).

'$gen_call'(X,Y,Z) ->
    writer:server(X,Y,Z).

'$gen_cast'(X,Y,Z) ->
    case writer:server(X,Y,Z) of
         Stop = {stop,_,_} -> Stop;
         Okey = {ok,_,Sta} -> {ok,Sta} end.

'init'(Msg,Sender,State) ->
    {ok, State}.

'EXIT'(Msg,Sender,State) ->
    io:format("EXIT: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.

timeout(Msg,Sender,State) ->
    io:format("timeout: ~p~n",[{Msg,Sender,State}]),
    {stop, timeout, {Sender,[]}, State}.

system(Msg,Sender,State) ->
    io:format("system: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.
