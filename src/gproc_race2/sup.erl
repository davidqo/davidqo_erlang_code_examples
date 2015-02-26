-module(sup).

-export([start_link/1]).

-export([init/1]).


start_link(InvokeId) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [InvokeId]).

init([InvokeId]) ->
    Strategy = {one_for_one, 100, 100},
    ReceiverChild = {sup_receiver_child, {child_receiver, start_receiver, [InvokeId]}, permanent, 2000, worker, [child_receiver]},
    SenderChild = {sup_sender_child, {child_sender, start_sender, [InvokeId]}, permanent, 2000, worker, [child_sender]},
    {ok, {Strategy, [ReceiverChild, SenderChild]}}.
