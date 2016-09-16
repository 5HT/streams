-module(streams).
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1]).
-record(core, { operation, resource, module, req, method }).
-define(POOL,1000).

tables()   -> [ cache ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              { ok, { { one_for_one, 5, 10 }, [spec(8880)] } }.

stop(_)    -> ok.
start(_,_) -> supervisor:start_link({local,streams},streams,[]).
spec(Port) -> ranch:child_spec(http, 2, ranch_tcp, port(Port), cowboy_protocol, env()).
env()      -> [ { env, [ { dispatch, points() } ] } ].
port(Port) -> [ { port, Port  } ].
points()   -> cowboy_router:compile([{'_', [ {"/capture", streams_endpoint, []} ]}]).
main(A)    -> mad:main(A).

cache(Key, undefined) -> ets:delete(cache,Key);
cache(Key, Value) -> ets:insert(cache,{Key,Value}), Value.
cache(Key) ->
    Res = ets:lookup(cache,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined;
                {_,X} -> X;
                _ -> Val end.
