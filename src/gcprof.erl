-module(gcprof).
-export([start/0, stop/0]).
-export([trace/2, trace_me/1, sample_me/2]).

start() -> application:start(gcprof).
stop()  -> application:stop(gcprof).

%% @doc: Starts tracing garbage collections in the given
%% process. Note: gen_server:cast is used, so at some point in the
%% future, tracing *might* start.
trace(Pid, IdentityF) ->
    gen_server:cast(gcprof_server, {start_trace, Pid, IdentityF}).

%% @doc: Starts tracing garbage collections in the calling process.
trace_me(IdentityF) ->
    trace(self(), IdentityF).

%% @doc: Traces the calling process if the second argument is
%% 0. Allows tracing if the result of a modulo operation is 0.
sample_me(IdentityF, 0) ->
    trace(self(), IdentityF);
sample_me(_, _) ->
    ok.

    
