-module(child_sender).

-export([start_sender/1]).

-export([sender_process/1]).

start_sender(InvokeId) ->
    io:format("(Re)Start sender process for invoke id ~p~n", [InvokeId]),
    {ok, spawn_link(?MODULE, sender_process, [InvokeId])}.

sender_process(InvokeId) ->
    timer:sleep(5000),
    sender_process(InvokeId, 0).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++

sender_process(InvokeId, TickCount) ->
    timer:sleep(500),
    case gproc:lookup_local_name({proc, InvokeId}) of
        undefined ->
            io:format("Cannot get pid for the {proc, ~p}~n", [InvokeId]),
            case get({proc, InvokeId}) of
                undefined ->
                    io:format("Nothing to take from the process dictionary~n", []),
                    sender_process(InvokeId, TickCount + 1);
                Pid ->
                    io:format("Use old pid from the process dictionary: ~p~n", [Pid]),
                    Pid ! {msg, TickCount},
                    sender_process(InvokeId, TickCount + 1)
            end;
        Pid ->
            Pid ! {msg, TickCount},
            put({proc, InvokeId}, Pid),
            sender_process(InvokeId, TickCount + 1)
    end.
