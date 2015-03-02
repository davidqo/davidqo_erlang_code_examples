-module(named_process_pid_test).

-export([t/1, receiver/0, sender/3]).

t(ProcessCount) ->
    statistics(runtime),
    statistics(wall_clock),
    Delay = 5000,
    Refs = spawner(ProcessCount, Delay),
    {_, Runtime1} = statistics(runtime),
    {_, WallClock1} = statistics(wall_clock),
    wait(Refs),
    {_, Runtime2} = statistics(runtime),
    {_, WallClock2} = statistics(wall_clock),
    io:format("Done! Runtime: creating: ~p sending: ~p WallClock: creating: ~p, sending: ~p~n", [Runtime1, Runtime2, WallClock1, WallClock2 - Delay]).

wait([]) ->
    ok;
wait(Refs) ->
    receive
        {'DOWN', Ref, _, _, _} ->
            wait(lists:delete(Ref, Refs))
    end.
%%+++++++++++++++++++++++++++++++++++++++++++++++++++

spawner(ProcessCount, Delay) ->
    spawner(ProcessCount, [], erlang:now(), Delay).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++

spawner(0, Acc, _,  _) ->
    Acc;
spawner(ProcessCount, Acc, StartedAt, Delay) ->
    Pid1 = erlang:spawn(?MODULE, receiver, []),
    Pid2 = erlang:spawn(?MODULE, sender, [Pid1, StartedAt, Delay]),
    Ref1 = erlang:monitor(process, Pid1),
    Ref2 = erlang:monitor(process, Pid2),
    spawner(ProcessCount - 1, [Ref1, Ref2 | Acc], StartedAt, Delay).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++

sender(Pid, StartedAt, Delay) ->
    Diff = timer:now_diff(now(), StartedAt),
    timer:sleep(Delay - (Diff div 1000)),
    sender(Pid, 100).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++

sender(Pid, 0) ->
    Pid ! finish;
sender(Pid, Count) ->
    Pid ! msg,
    sender(Pid, Count - 1).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++

receiver() ->
    receive
        msg ->
            receiver();
        finish ->
            ok
    end.
%%+++++++++++++++++++++++++++++++++++++++++++++++++++
