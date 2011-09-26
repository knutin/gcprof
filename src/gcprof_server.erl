-module(gcprof_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tracer, identity_fs = orddict:new(), processes = ordsets:new()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{tracer = gcprof_tracer:start_link(self())}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_trace, Pid, IdentityF}, State) ->
    case catch(erlang:trace(Pid, true, ['receive', timestamp, garbage_collection])) of
        {'EXIT', {badarg, _}} ->
            %% Process died before we could start tracing
            error_logger:info_msg("badarg while setting up trace~n"),
            {noreply, State};
        1 ->
            erlang:monitor(process, Pid),
            State#state.tracer ! {start, Pid},
            NewIdentityFs = orddict:store(Pid, IdentityF, State#state.identity_fs),
            NewProcesses = ordsets:add_element(Pid, State#state.processes),
            {noreply, State#state{identity_fs = NewIdentityFs, processes = NewProcesses}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(TraceMsg, State) when element(1, TraceMsg) =:= trace_ts ->
    case State#state.tracer of
        undefined ->
            Tracer = gcprof_tracer:start_link(self()),
            Tracer ! TraceMsg,
            erlang:monitor(process, Tracer),
            {noreply, State#state{tracer = Tracer}};
        Tracer ->
            Tracer ! TraceMsg,
            {noreply, State}
    end;

handle_info({'DOWN', _, process, Pid, _}, State) ->
    case ordsets:is_element(Pid, State#state.processes) of
        true ->
            State#state.tracer ! {down, Pid},
            {noreply, State#state{processes = ordsets:del_element(Pid, State#state.processes)}};
        false ->
            {noreply, State}
    end;


handle_info({results, Pid, Message, Runtime, Invocations}, State) ->
    IdentityF = orddict:fetch(Pid, State#state.identity_fs),
    gcprof_aggregator:results(Pid, IdentityF(Message), Runtime, Invocations),
    %%NewIdentityFs = orddict:erase(Pid, State#state.identity_fs),
    {noreply, State};

handle_info(_Info, State) ->
    error_logger:info_msg("got info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
