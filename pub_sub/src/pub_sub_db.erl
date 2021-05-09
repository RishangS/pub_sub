-module(pub_sub_db).

-export([
	read_subscribers/1,
	add_subscriber/2,
	delete_subscriber/2,
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
			{Pid, _Socket} = Subscriber,
			case lists:keyfind(Pid,1,SubscribersList)of
				{_PidR, _SocR} ->
					io:format("Client already Subscribed to Topic ~p Pid ~p  ~n",[Topic, Pid]);
				_ ->
					ets:insert(?TABLE, {Topic, [Subscriber | SubscribersList]})
			end

	end.

read_subscribers(Topic) ->
	setup(?TABLE),
	case ets:lookup(?TABLE, Topic) of
	    [{Topic, SubscribersList}] ->
	        SubscribersList;
	    [] ->
	        []
	end.

delete_subscriber(Topic, Pid)->
	setup(?TABLE),
	case read_subscribers(Topic) of
		[] ->
			erlang:display({"table_empty"});
		SubscribersList ->
			case lists:keyfind(Pid,1,SubscribersList)of
				{PidR, SocR} ->
					New_SubscriberList = lists:delete({PidR, SocR},SubscribersList),
					ets:insert(?TABLE, {Topic, [New_SubscriberList]});
				_ ->
					erlang:display({"Client unknown ", Pid})
			end
	end.