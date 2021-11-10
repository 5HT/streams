-ifndef(N2O_CORE).
-define(N2O_CORE, true).

-record(ok,      { data }).
-record(error,   { data }).
-record(reply,   { msg, req, ctx } ).
-record(unknown, { msg, req, ctx } ).

-endif.
