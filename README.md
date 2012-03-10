# gcprof - garbage collection profiler

gcprof is a tool to measure garbage collection in a production
system. For every message a process receives, gcprof tells you how
much time was spent in garbage collection and how many passes were
done.


## How does it work

`gcprof:trace(pid(), identity_f())` triggers tracing of garbage
collection in the specified process. Every time the process receives a
message, the resulting garbage collections is associated with that
message. The identity fun is used to identify that message.

The runtime of garbage collections is aggregated and grouped on the
identity. The results can be retrieved by calling
`gcprof_aggregator:get_stats/0`. This will reset the current stats, so
it is suitable for periodic polling.

The convenience method `gcprof:trace_me(identity_f())` can be used
from within the process you wish to trace.


## Identity

The identity fun is given the message your process received and must
produce an identity. The results will be grouped by identity, so it
makes sense to keep the number of unique ids low. The identity will be
the `key` in `gcprof_aggregator:get_stats/0`.

For example:

    fun ({'$gen_call', {_, _}, do_some_stuff}) ->
            my_identity;
        ({'$gen_call', {_, _}, {request, Arg1, Arg2}) ->
            {request, Arg1};
        (init) ->
            init;
        (_) ->
            undefined
    end

There is one special message you may also handle and that is
`init`. Any garbage collection done after triggering the trace, but
before your process received any messages is associated with this
identity. Please note that as starting a trace is asynchronous, some
information might be lost.

## Stats

This is an example of stats created by running the provided `example_server`.

    {ok,[[{key,{create_garbage,invocations}},
          {observations,20},
          {min,1},
          {mean,9.0},
          {max,17},
          {sd,8.207826816681232},
          {quantile_25,0.5},
          {quantile_75,16.5},
          {quantile_99,16.98},
          {quantile_999,16.998}],
         [{key,{create_garbage,runtime}},
          {observations,20},
          {min,690},
          {mean,4220.85},
          {max,8728},
          {sd,2898.8413706350443},
          {quantile_25,1062.0},
          {quantile_75,5974.0},
          {quantile_99,8727.8},
          {quantile_999,8727.98}]]}

## Sampling

As you may not want to trace all processes executing the code that
triggers tracing, there is a very simple way of deciding which
processes to sample.

Example:

    IdentityF = fun ...,
    Id = 1000000,
    Divisor = 100,
    gcprof:sample_me(IdentityF, Id rem Divisor)

`sample_me/2` only samples if the second argument is 0. So in the
above case, given an even distribution of ids, only 1% of the 
processes will be sampled.


## Impact on your system

`gcprof:trace/2` uses a `gen_server:cast/2` under the hood so your
process does not have to wait for gcprof. This also means that if gcprof
is not running, nothing happens.

If gcprof would crash, the processes would be restarted. When the
gcprof tracer process goes away, the VM stops whatever traces were
active.


## Ideas for the future

At the moment, there are some race conditions in some of the tests
that makes them occasionally fail. This should be fixed.

To trace code without modifying the target, a future version of gcprof
could trace a specific function call and start tracing of the process
invoking that function. Superuseful for the `init/1` gen_server
callback for example.



