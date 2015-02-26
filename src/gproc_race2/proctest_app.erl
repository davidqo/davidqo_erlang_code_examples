-module(proctest_app).

-export([start/0]).

-export([start/2, stop/2]).

start() ->
    application:start(gproc),
    application:start(proctest).

start(_, _) ->
    sup:start_link(11).

stop(_, _) ->
    io:format("Application stop~n"),
    ok.
