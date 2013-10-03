-module(predicate).

%% ====================================================================
%% API functions
%% ====================================================================
-export([evalPredicate/2, createPredicate/1, createDisjunction/1,
		 createElement/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Element - a structure representing an expression without any logical
%% operators
%% (either a constant or a function name with its arguments, where
%% each argument can be a constant or a structure representing
%% a function with its arguments...)

%% A disjunction record represents a list of elements logically joined by OR
%% A predicate record represents a list of disjunctions and is evaluated
%% as a conjunction of these disjunctions

-record(disjunction, {elements=[]}).
-record(predicate, {disjunctions=[]}).

createPredicate(Disjunctions) when is_list(Disjunctions) ->
	#predicate{disjunctions=Disjunctions}.

createDisjunction(Elements) when is_list(Elements) ->
	#disjunction{elements=Elements}.

createElement(Function, ArgumentList) ->
	{Function, ArgumentList}.

%% The element is a placeholder for an input event
evalElement({event, EventNumber}, Events) ->
	lists:nth(EventNumber, Events);

evalElement(Element, Events) when is_tuple(Element) ->
	{Function, ArgumentList} = Element,
	
	%%apply(functions, Function, lists:map(fun(Argument) ->
	%%	evalElement(Argument, Events) end, ArgumentList));

	%%functions:Function(lists:map(fun(Argument) ->
	%%	evalElement(Argument, Events) end, ArgumentList));

	case length(ArgumentList) of
		1 -> [Argument] = ArgumentList,
			 functions:Function(evalElement(Argument, Events));
		2 -> [Argument1, Argument2] = ArgumentList,
			 functions:Function(evalElement(Argument1, Events), evalElement(Argument2, Events));
		3 -> [Argument1, Argument2, Argument3] = ArgumentList,
			 functions:Function(evalElement(Argument1, Events), evalElement(Argument2, Events),
								evalElement(Argument3, Events));
		4 -> [Argument1, Argument2, Argument3, Argument4] = ArgumentList,
			 functions:Function(evalElement(Argument1, Events), evalElement(Argument2, Events),
								evalElement(Argument3, Events), evalElement(Argument4, Events));
		5 -> [Argument1, Argument2, Argument3, Argument4, Argument5] = ArgumentList,
			 functions:Function(evalElement(Argument1, Events), evalElement(Argument2, Events),
								evalElement(Argument3, Events), evalElement(Argument4, Events),
								evalElement(Argument5, Events))
	end;

%% The element is just a constant
evalElement(Element, _) ->
	Element.

evalDisjunction(Disjunction, Events) when is_record(Disjunction, disjunction),
										  is_list(Events) ->
	ElementList = Disjunction#disjunction.elements,
	lists:any(fun(Element) -> evalElement(Element, Events) end, ElementList).

evalPredicate(Predicate, Events) when is_record(Predicate, predicate),
									  is_list(Events) ->
	DisjunctionList = Predicate#predicate.disjunctions,
	lists:all(fun(Disjunction) -> evalDisjunction(Disjunction, Events) end, DisjunctionList).