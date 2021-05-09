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
	erlang:display({ListenSocket, State}),
	{noreply, State#{socket => AcceptSocket}};
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
handle_call(_Request, _From, State) ->
  	{reply, reply, State}.

handle_info(_Request, State) ->
  	{reply, State}.


