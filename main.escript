#!/usr/bin/env escript

main(_) ->
	process_flag(trap_exit, true),
	Block1 = block:start(),
	Block2 = block:start(),
	block:add_neighbor(Block1, Block2),
	Train = train:start(Block1),
	train:start(Block2),
	io:format("train at ~p (expect ~p)~n", [train:location(Train), Block1]),
	case train:move(Train) of
		ok -> io:format("move success~n");
		{error, Message} -> io:format("move failed: ~p~n", [Message]);
		Other -> io:format("something else ~p~n", [Other])
	end,
	io:format("train at ~p (expect ~p)~n", [train:location(Train), Block2]).
