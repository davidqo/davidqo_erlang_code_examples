-module(gproc_race).

-export([t/1, f/1]).
-export([proc1/1, proc2/1]).

f(InvokeId) ->
    application:start(gproc),
    sup:start_link(InvokeId).

t(InvokeId) ->
    application:start(gproc),
    Pid1 = spawn(?MODULE, proc1, [InvokeId]),
    Pid2 = spawn(?MODULE, proc1, [InvokeId]),
    Pid3 = spawn(?MODULE, proc2, [InvokeId]),
    Pids = [{proc1_1, Pid1}, {proc1_2, Pid2}, {proc2, Pid3}],
    Refs = [{ProcType, erlang:monitor(process, Pid)} || {ProcType, Pid} <- Pids],
    wait(Refs).

wait([]) ->
    ok;
wait(Refs) ->
    receive
        {'DOWN', Ref, process, _, Reason} ->
            case lists:keytake(Ref, 2, Refs) of
                {value, {ProcType, Ref}, Rest} ->
                      io:format("Proc ~p is terminated with reason: ~p~n", [ProcType, Reason]),
                      wait(Rest);
                false ->
                      io:format("Error: unknown proc terminated~n", []),
                      wait(Refs)
            end
    end.

proc1(InvokeId) ->
    timer:sleep(1000),
    Ret = gproc:add_local_name({proc1, InvokeId}),
    io:format("Proc1 ~p Ret: ~p~n", [InvokeId, Ret]),
    receive
        msg ->
          io:format("Pid: ~p. Msg received~n", [self()])
    after 5000 ->
          io:format("Pid: ~p. Nothing to receive~n", [self()])
    end.

proc2(InvokeId) ->
    case gproc:lookup_local_name({proc1, InvokeId}) of
            Pid when is_pid(Pid) ->
                Pid ! msg;
            Ret ->
                io:format("Proc2 cannot lookup {proc1, ~p}, Ret: ~p~n", [InvokeId, Ret]),
                timer:sleep(1000),
                proc2(InvokeId)
    end.
