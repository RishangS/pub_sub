%%%-------------------------------------------------------------------
%% @doc pub_sub_server gen_server.
%% This is a gen_server worker which listens the the mentioned Port
%% and waits in the acceptance state till a client sends a connect request
%% @end
%%%-

-module(pub_sub_server).

-behaviour(gen_server).

%% API functions
-export([
	start_link/1,
	child_spec/1
]).

%% Call back functions
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2
]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%%===================================================================
%%% API
%%%===================================================================

%% child_spec returns the worker initiating mechanism to supervisor 
child_spec(Socket) ->
  #{
    id      => ?MODULE,
    start   => {?MODULE, start_link, [Socket]},
    restart => temporary
  }.

start_link(Socket) ->
  gen_server:start_link(?MODULE, [Socket], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([Socket]) ->
	erlang:display("received socket in init"),
	_ = gen_server:cast(self(), accept),
	{ok, #{socket => Socket}}.

%% @hidden
handle_cast(accept, State = #{socket := ListenSocket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	pub_sub_sup:start_socket(),
	erlang:display({"Acceptsocket is",AcceptSocket, State}),
	{noreply, State#{socket => AcceptSocket}};
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
handle_call(_Request, _From, State) ->
  	{reply, reply, State}.

handle_info({tcp, Socket, Msg}, State) ->
 	erlang:display({Socket, Msg, State, "handle_info in pubsub_server" }),
 	process_request(Msg, Socket),
  	% ok = gen_tcp:send(Socket, Msg),
  	% erlang:display({"sending message to", Socket}),
  	{noreply, State};
handle_info({tcp_error, Socket, _}, State) -> 
	erlang:display({Socket, tcp_error, State}),
	{stop, State};
handle_info({tcp_closed, Socket}, State) -> 
	erlang:display({Socket, tcp_closed, State}),
	{stop, normal, State};
handle_info(Exception, State) ->
	erlang:display({Exception, State}),
	{noreply, State}.


process_request(Msg, Socket) ->
	Msg_string = [X || X <- erlang:binary_to_list(Msg), X =/= $\", X=/=${, X=/=$}],
	 case erlang:list_to_tuple(string:tokens(Msg_string, ",")) of
	 	{"publisher", Topic, Message}->
	 		erlang:display({"publisher", Topic, Message, Socket}),
	 		case pub_sub_db:read_subscribers(Topic) of
				[] ->
					erlang:display("no subscribers ");
				Subscribers ->
					erlang:display({Subscribers, subscribers}),
					[gen_tcp:send(Subscriber, Message) || Subscriber <- Subscribers]
			end,
	 		ok;
	 	{"subscriber",Topic} ->
	 		erlang:display({"subscriber", Topic, Socket}),
			pub_sub_db:add_subscriber(Topic, Socket)
	 end.
