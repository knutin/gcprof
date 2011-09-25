-module(example_server).

-behaviour(gen_server).

%% API
-export([run/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start(Id) ->
    gen_server:start(?MODULE, [Id], []).

req(Pid) ->
    gen_server:call(Pid, create_garbage).

run(Servers, Requests) ->
    RunF = fun(Id) ->
                   {ok, Pid} = start(Id),
                   timer:sleep(1),
                   [req(Pid) || _ <- lists:seq(1, Requests)],
                   exit(Pid, kill)
           end,

    lists:foreach(fun (Id) ->
                          spawn(fun () -> RunF(Id) end)
                  end, lists:seq(1, Servers)).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id]) ->
    gcprof:sample_me(fun identity/1, Id rem 10),
    {ok, #state{}}.

handle_call(create_garbage, _From, State) ->
    Pid = spawn(fun() ->
                        receive _ -> ok
                        end
                end),
    L = lists:seq(1, 100000),
    Pid ! L,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

identity({'$gen_call', {_, _}, create_garbage}) -> create_garbage;
identity(_) -> undefined.

