-module(child_receiver).

-export([start_receiver/1]).

-export([receiver_process/1]).

start_receiver(InvokeId) ->
    io:format("(Re)Start receiver process for invoke id ~p~n", [InvokeId]),
    {ok, spawn_link(?MODULE, receiver_process, [InvokeId])}.

receiver_process(InvokeId) ->
    timer:sleep(1000),
    io:format("(Re)Started receiver process for with pid: ~p~n", [self()]),
    gproc:add_local_name({proc, InvokeId}),
    receive
        {msg, Value} ->
            io:format("Receiver process received: ~p~n", [Value]),
            exit(normal)
    after 5000 ->
            io:format("Recevier process received nothing~n", []),
            exit(normal)
    end.
