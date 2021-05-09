-module(pub_sub_db).

-export([
	read_subscribers/1,
	add_subscriber/2,
	flush/0,
	setup/1
]).

-define(TABLE, pub_sub).

%%====================================================================
%% API functions
%%====================================================================
setup(?TABLE) ->
	case ets:whereis(?TABLE) of
		undefined ->
			ets:new(?TABLE, [set, named_table, public]);
		_Tid -> ok
	end.

flush() ->
	setup(?TABLE),
	ets:delete(?TABLE),
	ok.

add_subscriber(Topic, Subscriber) ->
	setup(?TABLE),
	case read_subscribers(Topic) of
		[] ->
			ets:insert(?TABLE, {Topic, [Subscriber]});
		SubscribersList ->
			ets:insert(?TABLE, {Topic, [Subscriber | SubscribersList]})
	end.

read_subscribers(Topic) ->
	setup(?TABLE),
	case ets:lookup(?TABLE, Topic) of
	    [{Topic, SubscribersList}] ->
	        SubscribersList;
	    [] ->
	        []
	end.
