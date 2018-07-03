-module(train).
-export([start/1
	,location/1
	,move/1
	]).

start(Block) ->
	spawn(fun() -> setup(Block) end).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	after 1000 ->
		exit(timeout)
	end.

location(Train) -> rpc(Train, location).
move(Train) -> rpc(Train, move).

setup(Block) ->
	case block:enter(Block) of
		ok -> loop(Block);
		{error, Message} -> exit(Message)
	end.

loop(Block) ->
	receive
		{From, location} ->
			From ! {self(), {location, Block}},
			loop(Block);
		{From, move} ->
			Neighbor = block:get_neighbor(Block),
			case block:enter(Neighbor) of
				ok ->
					case block:leave(Block) of
						ok ->
							From ! {self(), ok},
							loop(Neighbor);
						_ ->
							From ! {self(), {error, failedleave}},
							loop(Block)
					end;
				_ ->
					From ! {self(), {error, failedenter}},
					loop(Block)
			end;
		Message ->
			exit(Message)
	end.
