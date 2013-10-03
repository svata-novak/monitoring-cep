-module(predicates).

-export([main/0]).

-define(REPEAT_COUNT, 100000).

get_predicate() ->
	FirstArg = predicate:createElement(fun functions:projection/2, {event, 1}, "id"),
	SecondArg = predicate:createElement(fun functions:projection/2, {event, 2}, "id"),
	Elem = predicate:createElement(fun functions:eq/2, FirstArg, SecondArg),
	
	Disj = predicate:createDisjunction([Elem]),
	predicate:createPredicate([Disj]).

get_predicate2() ->
	Elem1 = predicate:createElement(fun functions:eq/2,
		predicate:createElement(fun functions:projection/2, {event, 1}, "id"),
		predicate:createElement(fun functions:projection/2, {event, 2}, "id")),
	Elem2 = predicate:createElement(fun functions:gt/2,
		predicate:createElement(fun functions:projection/2, {event, 1}, "attr1"),
		predicate:createElement(fun functions:projection/2, {event, 2}, "attr2")),
	predicate:createPredicate([predicate:createDisjunction([Elem1]),
							   predicate:createDisjunction([Elem2])]).

get_time_diff({BeforeMeS, BeforeS, BeforeMiS}, {AfterMeS, AfterS, AfterMiS}) ->
	{AfterMeS - BeforeMeS, AfterS - BeforeS, AfterMiS - BeforeMiS}.

main() ->
	io:format("Hello world!~n"),
	%%predicate:test(),
	%%EmptyDict = orddict:new(),
	%%Event = orddict:store("id", 1, EmptyDict),

	%%EmptyEvent = event:create(),
	%%Event1 = event:store(EmptyEvent, "id", 1),
	%%Event2 = event:store(EmptyEvent, "id", 1),
	
	%%functions:projection(Event, "id")
	%%ok.

	%%Elem = {fun functions:equals/2,
	%%		{fun functions:projection/2, {event, 1}, "id"},
	%%		{fun functions:projection/2, {event, 2}, "id"}},
	%%Disj = #disjunction{elements=[Elem]},
	%%Predicate = #predicate{inputEventCount=2, disjunctions=[Disj]},

	Event1 = event:create([{"id", 1}, {"attr1", 1}, {"attr2", 1}]),
	Event2 = event:create([{"id", 1}, {"attr1", 2}, {"attr2", 2}]),
	Event3 = event:create([{"id", 2}, {"attr1", 1}, {"attr2", 1}]),
	Event4 = event:create([{"id", 2}, {"attr1", 2}, {"attr2", 2}]),
	
	EventList = [Event1, Event2, Event3, Event4],
	
	Predicate = get_predicate2(),
	
	%%lists:foreach(fun(Event) -> io:format("~w ", [predicate:evalPredicate(Predicate, [Event, Event])]) end, EventList),

	Before = os:timestamp(),
	io:format("~n~n~n~n~n~w~n~n~n~n~n~n", [Before]),
	
	util:for(?REPEAT_COUNT, fun(_) ->
		lists:foreach(fun(Event1) ->
			lists:foreach(fun(Event2) ->
				%%io:format("~w ", [predicate:evalPredicate(Predicate, [Event1, Event2])])
				predicate:evalPredicate(Predicate, [Event1, Event2])
			end, EventList)
		end, EventList)
	end),
	
	After = os:timestamp(),
	io:format("~n~n~n~n~n~w~n~n~n~n~n~n", [After]),
	
	io:format("~n~n~n~n~nElapsed time: ~w~n~n~n~n~n~n", [get_time_diff(Before, After)]),
	
	%%predicate:evalPredicate(Predicate, [Event1, Event2]).
	
	ok.