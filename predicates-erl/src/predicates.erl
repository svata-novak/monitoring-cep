-module(predicates).

-export([main/0]).

-define(REPEAT_COUNT, 100000).

get_predicate() ->
	FirstArg = predicate:createElement(projection, {event, 1}, "id"),
	SecondArg = predicate:createElement(projection, {event, 2}, "id"),
	Elem = predicate:createElement(eq, FirstArg, SecondArg),
	
	Disj = predicate:createDisjunction([Elem]),
	predicate:createPredicate([Disj]).

get_predicate2() ->
	Elem1 = predicate:createElement(eq,
		predicate:createElement(projection, {event, 1}, "id"),
		predicate:createElement(projection, {event, 2}, "id")),
	Elem2 = predicate:createElement(gt,
		predicate:createElement(projection, {event, 1}, "attr1"),
		predicate:createElement(projection, {event, 2}, "attr2")),
	predicate:createPredicate([predicate:createDisjunction([Elem1]),
							   predicate:createDisjunction([Elem2])]).

get_time_diff({BeforeMeS, BeforeS, BeforeMiS}, {AfterMeS, AfterS, AfterMiS}) ->
	{AfterMeS - BeforeMeS, AfterS - BeforeS, AfterMiS - BeforeMiS}.

divide_time_diff({MeS, S, MiS}, Divisor) ->
	{MeS / Divisor, S / Divisor, MiS / Divisor}.

benchmark() ->
	Event1 = event:create([{"id", 1}, {"attr1", 1}, {"attr2", 1}]),
	Event2 = event:create([{"id", 1}, {"attr1", 2}, {"attr2", 2}]),
	Event3 = event:create([{"id", 2}, {"attr1", 1}, {"attr2", 1}]),
	Event4 = event:create([{"id", 2}, {"attr1", 2}, {"attr2", 2}]),
	
	EventList = [Event1, Event2, Event3, Event4],
	
	Predicate = get_predicate2(),

	Before = os:timestamp(),
	io:format("~nTimestamp before: ~w~n", [Before]),
	
	util:for(?REPEAT_COUNT, fun(_) ->
		lists:foreach(fun(Event1) ->
			lists:foreach(fun(Event2) ->
				%%io:format("~w ", [predicate:evalPredicate(Predicate, [Event1, Event2])])
				predicate:evalPredicate(Predicate, [Event1, Event2])
			end, EventList)
		end, EventList)
	end),
	
	After = os:timestamp(),
	io:format("~nTimestamp after: ~w~n", [After]),
	MatchesCount = length(EventList) * length(EventList) * ?REPEAT_COUNT,
	TimeDiff = get_time_diff(Before, After),
	io:format("~nElapsed time (~B matches): ~w~n", [MatchesCount, TimeDiff]),
	%%io:format("~nOne match: ~w~n", [divide_time_diff(TimeDiff, MatchesCount)]),
	ok.

main() ->
	benchmark().