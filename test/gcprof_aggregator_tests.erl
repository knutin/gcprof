-module(gcprof_aggregator_tests).

-include_lib("eunit/include/eunit.hrl").

aggregator_test() ->
    {ok, Pid} = gcprof_aggregator:start_link(),
    gcprof_aggregator:results(self(), create_garbage, 1000, 100),

    {ok, [Invocations, Runtime]} = gcprof_aggregator:get_stats(),

    ?assertEqual({observations, 1}, lists:keyfind(observations, 1, Invocations)),
    ?assertEqual({key, {create_garbage, invocations}}, lists:keyfind(key, 1, Invocations)),
    ?assertEqual({mean, 100.0}, lists:keyfind(mean, 1, Invocations)),

    ?assertEqual({observations, 1}, lists:keyfind(observations, 1, Runtime)),
    ?assertEqual({key, {create_garbage, runtime}}, lists:keyfind(key, 1, Runtime)),
    ?assertEqual({mean, 1000.0}, lists:keyfind(mean, 1, Runtime)),

    unlink(Pid),
    exit(Pid, kill).
    

