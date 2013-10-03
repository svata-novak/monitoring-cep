-module(event).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0, create/1, store/2, store/3, fetch/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================

create() ->
	orddict:new().

store(Event, Key, Value) ->
	orddict:store(Key, Value, Event).

store(Event, KeysValues) when is_list(KeysValues) ->
	lists:foldl(fun({Key, Value}, NewEvent) -> store(NewEvent, Key, Value) end,
											   Event, KeysValues).

create(KeysValues) when is_list(KeysValues) ->
	store(create(), KeysValues).

fetch(Key, Event) ->
	orddict:fetch(Key, Event).