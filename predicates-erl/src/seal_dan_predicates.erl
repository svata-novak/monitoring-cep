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

-record(clause, {literals = []}).
-record(predicate, {clauses = []}).

%% API
-export([eval/2]).

eval(Predicate = #predicate{}, Events) ->
	lists:all(fun(Clauses = #clause{}) -> eval(Clauses, Events) end, Predicate#predicate.clauses);
eval(Clause = #clause{}, Events) ->
	lists:any(
		fun(Literal) ->
			{Fun, Args} = Literal,
			seal_boolfuns:Fun(args(Args, Events))
		end,
		Clause#clause.literals).

args([First, Second], Events) ->
	{arg(First, Events), arg(Second, Events)}.

arg({projection, [{event, Number}, Attribute]}, Events) ->
	projection(lists:nth(Number, Events), Attribute);
arg({constant, Constant}, _Events) ->
	Constant.

projection(Event, Attribute) when is_list(Attribute) ->
	event:fetch(Attribute, Event).