%%%-------------------------------------------------------------------
%%% @author xtovarn
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. X 2013 14:44
%%%-------------------------------------------------------------------
-module(seal_dan_predicates).
-author("xtovarn").

-record(disjunction, {terms = []}).
-record(predicate, {disjunctions = []}).

%% API
-export([eval/2, projection/2, eq/1, neq/1, eq2/1, neq2/1, gt/1, gte/1, lt/1, lte/1]).

eval(Predicate = #predicate{}, Events) ->
	lists:all(fun(Disjunction = #disjunction{}) -> eval(Disjunction, Events) end, Predicate#predicate.disjunctions);
eval(Disjunction = #disjunction{}, Events) ->
	lists:any(
		fun(Term) ->
			{Fun, Args} = Term,
			?MODULE:Fun(args(Args, Events))
		end,
		Disjunction#disjunction.terms).

eq({Val1, Val2}) ->
	Val1 =:= Val2.

neq({Val1, Val2}) ->
	Val1 =/= Val2.

eq2({Val1, Val2}) ->
	Val1 == Val2.

neq2({Val1, Val2}) ->
	Val1 /= Val2.

gt({Val1, Val2}) ->
	Val1 > Val2.

gte({Val1, Val2}) ->
	Val1 >= Val2.

lt({Val1, Val2}) ->
	Val1 < Val2.

lte({Val1, Val2}) ->
	Val1 =< Val2.

arg({Fun, [{event, Number}, Attribute]}, Events) ->
	?MODULE:Fun(lists:nth(Number, Events), Attribute);
arg({constant, Constant}, _Events) ->
	Constant.

args([First, Second], Events) ->
	{arg(First, Events), arg(Second, Events)}.

projection(Event, Attribute) when is_list(Attribute) ->
	event:fetch(Attribute, Event).