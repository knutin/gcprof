-module(gcprof_aggregator).

-behaviour(gen_server).

%% API
-export([start_link/0, results/4, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(NEW_HISTOGRAM, basho_stats_histogram:new(0, 1000000, 1000000)).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

results(Pid, Identity, Runtime, Invocations) ->
    gen_server:cast(?MODULE, {results, Pid, Identity, Runtime, Invocations}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_stats, _From, State) ->
    {reply, {ok, do_get_stats()}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({results, _Pid, Identity, Runtime, Invocations}, State) ->
    ok = update_histogram({Identity, runtime}, Runtime),
    ok = update_histogram({Identity, invocations}, Invocations),
    %%ok = update_histogram({Pid, runtime}, Runtime),
    %%ok = update_histogram({Pid, invocations}, Invocations),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_or_create_histogram(Key) ->
    case get(Key) of
        undefined          -> ?NEW_HISTOGRAM;
        {histogram, Value} -> Value
    end.

update_histogram(Key, Value) ->
    Hist = get_or_create_histogram(Key),
    NewHist = basho_stats_histogram:update(Value, Hist),
    erlang:put(Key, {histogram, NewHist}),
    ok.


do_get_stats() ->
    Ts = lists:filter(fun ({_, {histogram, Hist}}) ->
                              basho_stats_histogram:observations(Hist) =/= 0;
                          (_) ->
                              false
                      end, get()),
    [erlang:put(Key, {histogram, ?NEW_HISTOGRAM}) || {Key, _Value}  <- Ts],
    lists:map(fun (T) -> format_timing(T) end, Ts).

format_timing({Identity, {histogram, Hist}}) ->
    {Min, Mean, Max, _, SD} =
        basho_stats_histogram:summary_stats(Hist),

    [
     {key, Identity},
     {observations, basho_stats_histogram:observations(Hist)},
     {min, Min},
     {mean, Mean},
     {max, Max},
     {sd, SD},
     {quantile_25, basho_stats_histogram:quantile(0.250, Hist)},
     {quantile_75, basho_stats_histogram:quantile(0.750, Hist)},
     {quantile_99, basho_stats_histogram:quantile(0.990, Hist)},
     {quantile_999, basho_stats_histogram:quantile(0.999, Hist)}
    ].
