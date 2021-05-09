%%%-------------------------------------------------------------------
%% @doc pub_sub top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pub_sub_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 	 start_socket/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_socket() ->
  supervisor:start_child(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

init([]) ->
	{ok, ListenSocket} = gen_tcp:listen(6292, [binary, {active,true}]),
	spawn_link(fun empty_listeners/0),
	Child = 
		[
	    	pub_sub_server:child_spec(ListenSocket)
	    ],
	{ok, { {simple_one_for_one, 60, 3600}, Child} }.

%%====================================================================
%% Internal functions
%%====================================================================
%% Can start N number of listeners to process huge number of request, 
%% this stays active even if the processes get killed
empty_listeners() ->
	[start_socket() || _ <- lists:seq(1,1)],
	ok.