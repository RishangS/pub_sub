%%%-------------------------------------------------------------------
%% @doc pub_sub_manager.
%% This is a pub_sub client module which sends requests to the server using tcp connection
%% @end
%%%-

-module(pub_sub_manager).

-export([
	subscribe/2,
	unsubscribe/2,
	publish/3,
	disconnect/2,
	wait_for_msg/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%% subscribe is sent when a client wants to subscribe a Topic in the server
%% Args Topic, Server Port
subscribe(Topic, Port) ->
	case validate(port,Port) of
		valid ->
			Response = gen_tcp:connect(
				"localhost", 
				Port, 
				[binary, {packet, 0}, {active, false}, {reuseaddr, true}]
			),
			case Response of
				{ok, SocketId} ->
					spawn(?MODULE, wait_for_msg, [SocketId]),
					Data = lists:flatten(io_lib:format("~p", [{self(), subscribe, Topic}])),
					gen_tcp:send(SocketId,Data),
					{ok,SocketId};
				Reason ->
					{error, Reason}
			end;
		invalid ->
			{error, invalid_param}
	end.

%% publish is sent when a client wants to publish a Message to its subscribers
%% Args Topic, Server Port, Message
publish(Topic, Port, Msg) ->
	case validate(port,Port) of
		valid ->
			Response = gen_tcp:connect(
				"localhost", 
				Port, 
				[binary, {packet, 0}, {active, false}, {reuseaddr, true}]
			),

			case Response of
				{ok, SocketId} ->
					Data = lists:flatten(io_lib:format("~p", [{self(), publish, Topic, Msg}])),
					gen_tcp:send(SocketId, Data),
					gen_tcp:close(SocketId),
					{publish, success};
				Reason ->
					{error, Reason}
			end;
		invalid ->
			{error, invalid_param}
	end.

%% unsubscribe is sent when a client wants to unsubscribe a Topic in the server
%% Args Topic, Server Port
unsubscribe(Topic,Port) ->
	case validate(port,Port) of
		valid ->
			Response = gen_tcp:connect(
				"localhost", 
				Port, 
				[binary, {packet, 0}, {active, false}, {reuseaddr, true}]
			),
			case Response of
				{ok, SocketId} ->
					Data = lists:flatten(io_lib:format("~p", [{self(), unsubscribe, Topic}])),
					gen_tcp:send(SocketId, Data),
					gen_tcp:close(SocketId),
					{unsubscribe, success};
				Reason ->
					{error, Reason}
			end;
		invalid ->
			{error, invalid_param}
	end.

%% disconnect is sent when a client wants to disconnect from the server
%% Args Topic, Server Port
disconnect(Topic, Port)->
	case validate(port,Port) of
		valid ->
			Response = gen_tcp:connect(
				"localhost", 
				Port, 
				[binary, {packet, 0}, {active, false}, {reuseaddr, true}]
			),
			case Response of
				{ok, SocketId} ->
					Data = lists:flatten(io_lib:format("~p", [{self(), disconnect, Topic}])),
					gen_tcp:send(SocketId, Data),
					gen_tcp:close(SocketId),
					{disconnect, success};
				Reason ->
					{error, Reason}
			end;
		invalid ->
			{error, invalid_param}
	end.

%% wait_for_msg is a reciving loop which waits to recieve data from the publisher
wait_for_msg(Socket) ->
	timer:sleep(50),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
        	io:format("Data recieved : ~p ~n",[Data]),	
        	Data;
        {error, Reason} ->
        	erlang:display({?MODULE, wait_for_msg,connection_closed, Reason}),
            exit(self(), kill),
            ok

    end,
    wait_for_msg(Socket).

%% Validation logic for passed parameters
validate(Parameter, Value)->
	case Parameter of
		port ->
			if (Value > 1024) andalso (Value < 65535) ->
					valid;
				true ->
					invalid
			end;
		_ ->
			invalid
	end.
