-module(predicate).

%% ====================================================================
%% API functions
%% ====================================================================
-export([evalPredicate/2, createPredicate/1, createDisjunction/1,
		 createElement/3]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Element - a structure representing a boolean expression
%% (either a constant or a function with two arguments, where
%% each argument can be a constant or a structure representing
%% a function with two arguments...)

%% A disjunction record represents a list of elements logically joined by OR
%% A predicate record represents a list of disjunctions and is evaluated
%% as a conjunction of these disjunctions

-record(disjunction, {elements=[]}).
-record(predicate, {disjunctions=[]}).

createPredicate(Disjunctions) when is_list(Disjunctions) ->
	#predicate{disjunctions=Disjunctions}.

createDisjunction(Elements) when is_list(Elements) ->
	#disjunction{elements=Elements}.

createElement(Function, FirstArgument, SecondArgument) ->
	{Function, FirstArgument, SecondArgument}.

evalElement({event, EventNumber}, Events) ->
	lists:nth(EventNumber, Events);

evalElement(Element, Events) when is_tuple(Element) ->
	%% tuple_size(Element) should be 3
	{Function, FirstTerm, SecondTerm} = Element,

	FirstTermValue = evalElement(FirstTerm, Events),
	SecondTermValue = evalElement(SecondTerm, Events),
	functions:Function(FirstTermValue, SecondTermValue);

evalElement(Element, _) ->
	Element.

evalDisjunction(Disjunction, Events) when is_record(Disjunction, disjunction),
										  is_list(Events) ->
	%%lists:any(fun evalElement/1, Disjunction).
	ElementList = Disjunction#disjunction.elements,
	lists:any(fun(Element) -> evalElement(Element, Events) end, ElementList).

evalPredicate(Predicate, Events) when is_record(Predicate, predicate),
									  is_list(Events) ->
	DisjunctionList = Predicate#predicate.disjunctions,
	lists:all(fun(Disjunction) -> evalDisjunction(Disjunction, Events) end, DisjunctionList).