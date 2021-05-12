-module(pub_sub_client_SUITE).
-include_lib("common_test/include/ct.hrl").

%% CT
-export([
  	all/0, 
  	init_per_suite/1, 
  	end_per_suite/1,
  	init_per_testcase/2,
	end_per_testcase/2
]).

-type config() :: proplists:proplist().

-export_type([config/0]).

-export([ 
	subscribe_server/1, 
	publish/1,
	subscribe_multi_clients/1,
	disconnect_client/1
]).

-define(PORT, 5002).

all() -> [
	subscribe_server, 
	publish,
	subscribe_multi_clients,
	disconnect_client
].


init_per_suite(Config) ->
	_ = application:ensure_all_started(pub_sub),
	Config.
end_per_suite(Config) -> 
    Config.

init_per_testcase(_Testcase, Config) ->
  	Config.

end_per_testcase(_Testcase, _Config) ->
	ok.

subscribe_server(_Config) ->
	{ok,SocketId} = pub_sub_manager:subscribe("Hello", ?PORT),
	gen_tcp:close(SocketId).

publish(_Config) ->
	{publish, success} = pub_sub_manager:publish("Hello", ?PORT, "Test_ct").

subscribe_multi_clients(_Config) ->
	NoOfClients = 1000,
	[spawn(pub_sub_manager,subscribe,["Hello", ?PORT]) || _X <- lists:seq(1,NoOfClients)].

disconnect_client(_Config) ->
	{ok,SocketId} = pub_sub_manager:subscribe("Hello", ?PORT),
	pub_sub_manager:disconnect("Hello", ?PORT).
