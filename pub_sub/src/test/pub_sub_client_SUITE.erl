-module(pub_sub_client_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([ subscribe_server_test/0, publish_test/0]).

-define (PORT , 5001).

all() -> [subscribe_server_test, publish_test].

init_per_suite(Config) ->
  _ = application:ensure_all_started(pub_sub_manager),
  _ = application:ensure_all_started(pub_sub_app),
  	erlang:display(self()),	
  Config.


subscribe_server_test()->
	{ok,SocketId} = pub_sub_manager:subscribe("Hello", ?PORT),
	gen_tcp:close(SocketId).

publish_test() ->
	pub_sub_manager:publish("Hello", ?PORT, "Test_ct").

subscribe_N_clients()->
	[]