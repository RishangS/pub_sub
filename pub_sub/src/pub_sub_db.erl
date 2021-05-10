%%%-------------------------------------------------------------------
%% @doc pub_sub_db ets db manager from pub_sub application.
%% @end
%%%-

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
%% setup is called to make sure that the table exits if not setup api creates one.
%% Args : Table name
setup(?TABLE) ->
	case ets:whereis(?TABLE) of
		undefined ->
			ets:new(?TABLE, [set, named_table, public]);
		_Tid -> ok
	end.

%% flush api is to delete a table
%% Args : Table name
flush() ->
	setup(?TABLE),
	ets:delete(?TABLE),
	ok.

%% add_subscriber adds a new subscriber to the db depending on the topic
%% Args (Topic, {Client_Pid, Socket_Id})
add_subscriber(Topic, Subscriber) ->
	setup(?TABLE),
	case read_subscribers(Topic) of
		[] ->
			ets:insert(?TABLE, {Topic, [Subscriber]});
		SubscribersList ->
			{Pid, _Socket} = Subscriber,
			case lists:keyfind(Pid,1,SubscribersList)of
				{_PidR, _SocR} ->
					io:format("~p CLIENT ALREADY SUBSCRIBED TO ~p Pid ~p  ~n",[?MODULE, Topic, Pid]);
				_ ->
					ets:insert(?TABLE, {Topic, [Subscriber | SubscribersList]})
			end

	end.

%% read_subscribers reads the subscriber list from DB depending on the Topic selected
%% Args Topic 
read_subscribers(Topic) ->
	setup(?TABLE),
	case ets:lookup(?TABLE, Topic) of
	    [{Topic, SubscribersList}] ->
	        SubscribersList;
	    [] ->
	        []
	end.

%% delete_subscriber deletes particular client from the topic mentioned depending on client Pid
%% Args Topic, ClientPid
delete_subscriber(Topic, Pid)->
	setup(?TABLE),
	case read_subscribers(Topic) of
		[] ->
			erlang:display({?MODULE, "TABLE EMPTY"});
		SubscribersList ->
			case lists:keyfind(Pid,1,SubscribersList)of
				{PidR, SocR} ->
					erlang:display({?MODULE, PidR, SocR, "Deleting from db"}),
					New_SubscriberList = lists:delete({PidR, SocR},SubscribersList),
					erlang:display({?MODULE, New_SubscriberList}),
					ets:insert(?TABLE, {Topic, New_SubscriberList});
				_ ->
					erlang:display({?MODULE, "CLIENT UNKNOWN ", Pid})
			end
	end.