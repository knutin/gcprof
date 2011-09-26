-module(gcprof_server_tests).

-include_lib("eunit/include/eunit.hrl").


server_test_() ->
    {foreach,
     fun() ->
             application:start(gcprof)
     end,
     fun(_) ->
             application:stop(gcprof)
     end,
     [
      ?_test(simple_with_shutdown()),
      ?_test(garbage_in_init()),
      ?_test(example_single()),
      ?_test(broken_identity_f())
     ]
    }.


simple_with_shutdown() ->
    ?assertEqual({ok, []}, gcprof_aggregator:get_stats()),

    IdentityF = fun (garbage) ->
                        garbage;
                    (_) ->
                        undefined
                end,

    Pid = spawn(fun() ->
                        receive
                            garbage ->
                                lists:seq(1, 1000)
                        end
                end),
    gcprof:trace(Pid, IdentityF),
    %% Allow time for registering with gcprof_server
    timer:sleep(10),
    Pid ! garbage,
    %% Allow time for processing interval to trigger
    timer:sleep(200),

    {ok, Stats} = gcprof_aggregator:get_stats(),
    [Invocations, Runtime] = lists:sort(Stats),

    ?assertEqual({key, {garbage, runtime}}, lists:keyfind(key, 1, Runtime)),
    ?assertEqual({observations, 1}, lists:keyfind(observations, 1, Runtime)),

    ?assertEqual({key, {garbage, invocations}}, lists:keyfind(key, 1, Invocations)),
    ?assertEqual({observations, 1}, lists:keyfind(observations, 1, Invocations)).


garbage_in_init() ->
    ?assertEqual({ok, []}, gcprof_aggregator:get_stats()),

    IdentityF = fun (init) ->
                        init;
                    (_)    -> undefined
                end,

    Pid = spawn(fun () ->
                        gcprof:trace_me(IdentityF),
                        erlang:yield(),
                        lists:seq(1, 10000)
                end),
    Pid ! garbage,
    timer:sleep(200),
    {ok, [Invocations, Runtime]} = gcprof_aggregator:get_stats(),

    ?assertEqual({key, {init, runtime}}, lists:keyfind(key, 1, Runtime)),
    ?assertEqual({observations, 1}, lists:keyfind(observations, 1, Runtime)),

    ?assertEqual({key, {init, invocations}}, lists:keyfind(key, 1, Invocations)),
    ?assertEqual({observations, 1}, lists:keyfind(observations, 1, Invocations)).

broken_identity_f() ->
    ?assertEqual({ok, []}, gcprof_aggregator:get_stats()),

    IdentityF = fun (_) ->
                        exit(foobar),
                        baz
                end,

    Pid = spawn(fun () ->
                        gcprof:trace_me(IdentityF),
                        erlang:yield(),
                        lists:seq(1, 10000)
                end),
    Pid ! garbage,
    timer:sleep(200),
    {ok, Stats} = gcprof_aggregator:get_stats(),
    [_, Runtime] = lists:sort(Stats),

    ?assertEqual({key, {undefined, runtime}}, lists:keyfind(key, 1, Runtime)).




example_single() ->
    ?assertEqual({ok, []}, gcprof_aggregator:get_stats()),
    example_server:run(1, 2),
    timer:sleep(200).
