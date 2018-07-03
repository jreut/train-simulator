-module(block).
-include("block.hrl").
-export([start/0
	,add_neighbor/2
	,get_neighbor/1
	,leave/1
	,enter/1
	]).

id(X) -> X.
void(_) -> void.

start() ->
	spawn(fun() -> loop(#block{}) end).

rpc(Pid, Request, OnSuccess, OnTimeout, Timeout) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			OnSuccess(Response)
	after Timeout ->
		OnTimeout(Timeout)
	end.

rpc_no_receive(Pid, Message) -> rpc(Pid, Message, fun void/1, fun void/1, 0).

rpc_default_timeout(Pid, Request, OnSuccess) ->
	Timeout = 1000,
	OnTimeout = fun(_) -> io:format("~p timed out after ~p ms~n", [self(), Timeout]) end,
	rpc(Pid, Request, OnSuccess, OnTimeout, Timeout).

rpc_return_response(Pid, Request) -> rpc_default_timeout(Pid, Request, fun id/1).

add_neighbor(Block, Neighbor) -> rpc_return_response(Block, {addneighbor, Neighbor}).

get_neighbor(Block) -> rpc_return_response(Block, getneighbor).

leave(Block) -> rpc_return_response(Block, leave).

enter(Block) -> rpc_return_response(Block, enter).

loop(#block{neighbor=Neighbor, occupant=Occupant} = Record) ->
	receive
		{From, {addneighbor, NewNeighbor}} ->
			rpc_no_receive(From, ok),
			loop(Record#block{neighbor=NewNeighbor});
		{From, getneighbor} ->
			From ! {self(), Neighbor},
			loop(Record);
		{From, enter} ->
			case Occupant of
				undefined ->
					From ! {self(), ok},
					loop(Record#block{occupant=From});
				_ ->
					From ! {self(),
						{error, {occupied_by, Occupant}}
						},
					loop(Record)
			end;
		{From, leave} ->
			case Occupant of
				From ->
					From ! {self(), ok},
					loop(Record#block{occupant=undefined});
				_ ->
					From ! {self(),
						{error, {occupied_by, Occupant}}
						},
					loop(Record)
			end
	end.
