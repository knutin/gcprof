-module(gcprof_tracer_tests).

-include_lib("eunit/include/eunit.hrl").


tracer_test() ->
    Tracer = gcprof_tracer:start_link(self()),
    lists:foreach(fun (M) -> Tracer ! M end, messages()),

    receive
        {results, Pid, Message, Runtime, Invocations} ->
            ?assertEqual(self(), Pid),
            ?assertEqual(1090, Runtime),
            ?assertEqual(2, Invocations),
            ?assertEqual(create_garbage, (identity_f())(Message))
    end.

process_cycle_test() ->
    Tracer = gcprof_tracer:start_link(self()),
    lists:foreach(fun (M) -> Tracer ! M end, messages_with_init_and_shutdown()),

    fun() ->
            receive
                {results, Pid, Message, Runtime, Invocations} ->
                    ?assertEqual(self(), Pid),
                    ?assertEqual(9, Runtime),
                    ?assertEqual(1, Invocations),
                    ?assertEqual(init, (identity_f())(Message))
            end
    end(),

    fun() ->
            receive
                {results, Pid, Message, Runtime, Invocations} ->
                    ?assertEqual(self(), Pid),
                    ?assertEqual(1081, Runtime),
                    ?assertEqual(1, Invocations),
                    ?assertEqual(create_garbage, (identity_f())(Message))
            end
    end(),

    fun() ->
            receive
                {results, Pid, Message, Runtime, Invocations} ->
                    ?assertEqual(self(), Pid),
                    ?assertEqual(1081, Runtime),
                    ?assertEqual(1, Invocations),
                    ?assertEqual(create_garbage, (identity_f())(Message))
            end
    end().




identity_f() ->
    fun ({'$gen_call', {_, _}, create_garbage}) ->
            create_garbage;
        (init) ->
            init;
        (_) ->
            undefined
    end.


messages() ->
    Pid = self(),
    [{trace_ts,Pid,'receive',
      {'$gen_call',{Pid,make_ref()},create_garbage},
      {1316,876550,919801}},
     {trace_ts,Pid,gc_start,
      [{old_heap_block_size,0},
       {heap_block_size,233},
       {mbuf_size,0},
       {recent_size,0},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,220},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,919843}},
     {trace_ts,Pid,gc_end,
      [{old_heap_block_size,0},
       {heap_block_size,233},
       {mbuf_size,0},
       {recent_size,141},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,141},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,919852}},
     {trace_ts,Pid,gc_start,
      [{old_heap_block_size,0},
       {heap_block_size,17711},
       {mbuf_size,0},
       {recent_size,11005},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,17693},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,920573}},
     {trace_ts,Pid,gc_end,
      [{old_heap_block_size,28657},
       {heap_block_size,17711},
       {mbuf_size,0},
       {recent_size,0},
       {stack_size,12},
       {old_heap_size,11005},
       {heap_size,6688},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,921654}},
     {trace_ts,Pid,'receive',
      {'$gen_call',{Pid,make_ref()},create_garbage},
      {1316,876550,921690}}].


messages_with_init_and_shutdown() ->
    Pid = self(),
    [
     {start, Pid},
     {trace_ts,Pid,gc_start,
      [{old_heap_block_size,0},
       {heap_block_size,233},
       {mbuf_size,0},
       {recent_size,0},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,220},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,919843}},
     {trace_ts,Pid,gc_end,
      [{old_heap_block_size,0},
       {heap_block_size,233},
       {mbuf_size,0},
       {recent_size,141},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,141},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,919852}},
     {trace_ts,Pid,'receive',
      {'$gen_call',{Pid,make_ref()},create_garbage},
      {1316,876550,919801}},
     {trace_ts,Pid,gc_start,
      [{old_heap_block_size,0},
       {heap_block_size,17711},
       {mbuf_size,0},
       {recent_size,11005},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,17693},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,920573}},
     {trace_ts,Pid,gc_end,
      [{old_heap_block_size,28657},
       {heap_block_size,17711},
       {mbuf_size,0},
       {recent_size,0},
       {stack_size,12},
       {old_heap_size,11005},
       {heap_size,6688},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,921654}},
     {trace_ts,Pid,'receive',
      {'$gen_call',{Pid,make_ref()},create_garbage},
      {1316,876550,921690}},
     {trace_ts,Pid,gc_start,
      [{old_heap_block_size,0},
       {heap_block_size,17711},
       {mbuf_size,0},
       {recent_size,11005},
       {stack_size,12},
       {old_heap_size,0},
       {heap_size,17693},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,920573}},
     {trace_ts,Pid,gc_end,
      [{old_heap_block_size,28657},
       {heap_block_size,17711},
       {mbuf_size,0},
       {recent_size,0},
       {stack_size,12},
       {old_heap_size,11005},
       {heap_size,6688},
       {bin_vheap_size,0},
       {bin_vheap_block_size,46368},
       {bin_old_vheap_size,0},
       {bin_old_vheap_block_size,46368}],
      {1316,876550,921654}},
     {down, Pid}].
