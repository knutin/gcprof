-module(gcprof_tracer).

-export([start_link/1, loop/2, do_receive/1]).

-define(PROCESS_INTERVAL, 100).


start_link(Parent) ->
    spawn_link(fun() ->
                       loop(Parent, [])
          end).

loop(Parent, Acc) ->
    erlang:send_after(?PROCESS_INTERVAL, self(), process),
    Messages = lists:reverse(do_receive(lists:reverse(Acc))),

    NewMessages = case process(Messages) of
                      {GCData, NewM} ->
                          send_results(Parent, GCData),
                          NewM
                  end,
    ?MODULE:loop(Parent, NewMessages).



%% @doc: Consumes trace messages from the queue.
process(Messages) ->
    process(Messages, [], []).


process([{start, Pid} = Msg| Rest], Result, Q) ->
    %% A process just started tracing. If there are any gc messages
    %% before the first receive, these should be associated with the
    %% starting of the process to allow tracing gc in the init of a
    %% gen_server for example.

    case catch(find_gcs_until_receive(Rest, Pid)) of
        receive_not_found ->
            %% No receive yet for this process, do nothing and wait
            %% for more data
            process(Rest, Result, [Msg | Q]);
        [] ->
            %% No gcs found, but first receive found, discard the
            %% starting message
            process(Rest, Result, Q);
        GCs when is_list(GCs) ->
            %% Associated gcs with the starting message
            NewRest = delete_element(GCs, Rest),
            process(NewRest, [{Msg, GCs} | Result], Q)
    end;


process([{trace_ts, Pid, 'receive', _, _} = Msg| Rest], Result, Q) ->
    %% The process received a message. Find all gcs until the next
    %% receive.

    case catch(find_gcs_until_receive(Rest, Pid)) of
        receive_not_found ->
            %% No next receive found, do nothing and wait for more
            %% data
            process(Rest, Result, [Msg | Q]);

        GCs when is_list(GCs) ->
            NewRest = delete_element(GCs, Rest),

            %% If the process shutdown after the current receive, the
            %% last element in GCs will be the "down" message. Delete
            %% it.
            NewGCs = lists:delete({down, Pid}, GCs),

            process(NewRest, [{Msg, NewGCs} | Result], Q)
    end;

process([M | Rest], Result, Q) ->
    process(Rest, Result, [M | Q]);

process([], Result, Q) ->
    {lists:reverse(Result), lists:reverse(Q)}.



%% @doc: Deletes the given elements from the list in a single pass
%% through the list. The elements must occur in the same order as they
%% occur in the list.
delete_element([E | Es], [E | T]) ->
    delete_element(Es, T);
delete_element(Es, [H | T]) ->
    [H | delete_element(Es, T)];
delete_element([], L) ->
    L.




%% @doc: Returns a list of gc trace messages for the given pid until
%% the next receive or down message.
find_gcs_until_receive([], _Pid) ->
    throw(receive_not_found);

find_gcs_until_receive([{trace_ts, Pid, GCType, _, _} = Msg | Messages], Pid)
  when GCType =:= gc_start;
       GCType =:= gc_end ->
    [Msg | find_gcs_until_receive(Messages, Pid)];

find_gcs_until_receive([{trace_ts, Pid, 'receive', _, _} | _], Pid) ->
    [];
find_gcs_until_receive([{down, Pid} = Msg | _], Pid) ->
    [Msg];

find_gcs_until_receive([_ | Messages], Pid) ->
    find_gcs_until_receive(Messages, Pid).




%% @doc: Receive loop. When the message 'process' is received, the
%% current queue of trace messages is returned. Anything else is
%% received without filtering, to not give any preference to any type
%% of message which would mess up the ordering of trace messages.
do_receive(Messages) ->
    receive
        process ->
            Messages;
        Msg ->
            ?MODULE:do_receive([Msg | Messages])
    end.


send_results(Parent, GCData) ->
    lists:foreach(fun ({Receive, GCs}) ->
                          {Pid, Message} = case Receive of
                                               {trace_ts, P, 'receive', M, _} ->
                                                   {P, M};
                                               {start, P} ->
                                                   {P, init}
                                           end,

                          Pairs       = find_gc_pairs(GCs),
                          Runtime     = gc_time(Pairs),
                          Invocations = length(Pairs),
                          Parent ! {results, Pid, Message, Runtime, Invocations}
                  end, GCData).


gc_time(GCs) ->
    lists:foldl(fun ({StartMsg, EndMsg}, Acc) ->
                        timer:now_diff(element(5, EndMsg), element(5, StartMsg)) + Acc
                end, 0, GCs).

%% heap_reclaimed(GCs) ->
%%     lists:foldl(fun ({StartMsg, EndMsg}, Acc) ->
%%                         StartInfo = element(4, StartMsg),
%%                         EndInfo = element(4, EndMsg),

%%                         {heap_size, StartHeap} = lists:keyfind(heap_size, 1, StartInfo),
%%                         {heap_size, EndHeap} = lists:keyfind(heap_size, 1, EndInfo),

%%                         (StartHeap - EndHeap) + Acc
%%                 end, 0, GCs).


%% @doc: Returns a list of {Start, End} garbage collection
%% messages. Any excess gc_start at the end that has no gc_end is
%% discarded.
find_gc_pairs([]) ->
    [];

find_gc_pairs([Start | [End | Rest]]) ->
    [{Start, End} | find_gc_pairs(Rest)];

find_gc_pairs([_ | _Rest]) ->
    [].
