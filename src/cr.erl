-module(cr).
-description('Distributed Transaction Coordinator').
-copyright('Maxim Sokhatsky').
-compile(export_all).
-compile({no_auto_import,[node/0]}).


encode(Msg) -> term_to_binary(Msg).
decode(Bin) -> binary_to_term(Bin).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) -> gen_fsm:send_event(Pid, {socket_ready, Socket}).
send(Pid, Message) when is_pid(Pid)  -> gen_fsm:send_event(Pid, {out, Message}).

local(Object)  -> {I,N}=lists:keyfind(cr:nodex(cr:node()),2,cr:chain(Object)),
                  {I,P,_,_}=lists:keyfind(I,1,supervisor:which_children(vnode_sup)), P.
secret()       -> application:get_env(cr,secret,<<"ThisIsClassified">>).
peers()        -> {ok,Peers}=application:get_env(cr,peers),Peers.
peers(N)       -> lists:zip(lists:seq(1,N),lists:seq(1,N)).
hash(Object)   -> hd(seq(Object)).
rep(Object)    -> roll(element(2,hash(Object))).
roll(N)        -> lists:seq(N,length(peers())) ++ lists:seq(1,N-1).
chain(Object)  -> lists:map(fun(X) -> lists:nth((X-1)*4+1,cr:seq(Object)) end,
                  cr:roll(element(2,cr:hash(Object)))).
seq(Object)    -> lists:keydelete(0,1,ring:succ(ring:key_of(Object),ring())).
peer({I,N})    -> element(1,lists:nth(N,peers())).
nodex(Node)    -> string:str(cr:peers(),[lists:keyfind(Node,1,cr:peers())]).
node()         -> list_to_atom(lists:concat([os:getenv("NAME"),'@127.0.0.1'])).
vpid({I,Node}) -> {I,P,_,_}=lists:keyfind(I,1,supervisor:which_children({vnode_sup,Node})), P.
ring()         -> ring(application:get_env(cr,shards,4)).
ring(C)        -> {Nodes,[{0,1}|Rest]} = ring:fresh(length(peers())*C,1),
                  {Nodes,[{0,0}|lists:map(fun({{I,1},X})->{I,(X-1) div C+1} end,
                                lists:zip(Rest,lists:seq(1,length(Rest))))]}.

tx(Record) when is_tuple(Record) ->
    gen_server:cast(local(Record),
        {client,{self(),os:timestamp()},
                chain(element(2,Record)),
                Record}).

string(O) ->
    lists:concat(lists:flatten([lists:map(fun(undefined) -> ''; (X) -> [X,':'] end, tuple_to_list(O))])).


latency({I,N}) -> gen_server:call(cr:vpid({I,cr:peer({I,N})}),{latency}).

rpc(undefined) -> [];
rpc({badrpc,_}) -> {error,error};
rpc(Value) -> Value.

clean() -> kvs:destroy(), kvs:join().

log_modules() -> [cr,cr_log,%cr_vnode,
                  cr_rafter,cr_heart].

sup()   -> [{T,Pid}||{T,Pid,_,_}<-supervisor:which_children(cr_sup)].
heart() -> [{_,P,_,_}]=supervisor:which_children(heart_sup), gen_server:call(P,{heart}).
local() -> [{I,P}||{I,P,_,_} <- supervisor:which_children(vnode_sup)].
