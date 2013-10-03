-module(functions).

%% ====================================================================
%% API functions
%% ====================================================================
-export([projection/2, eq/2, neq/2, eq2/2, neq2/2, gt/2, gte/2, lt/2,
		 lte/2]).

%% ====================================================================
%% Internal functions
%% ====================================================================

eq(Val1, Val2) ->
	Val1 =:= Val2.

neq(Val1, Val2) ->
	Val1 =/= Val2.

eq2(Val1, Val2) ->
	Val1 == Val2.

neq2(Val1, Val2) ->
	Val1 /= Val2.

gt(Val1, Val2) ->
	Val1 > Val2.

gte(Val1, Val2) ->
	Val1 >= Val2.

lt(Val1, Val2) ->
	Val1 < Val2.

lte(Val1, Val2) ->
	Val1 =< Val2.

projection(Event, Attribute) when is_list(Attribute) ->
	event:fetch(Attribute, Event).

%%projection(Attribute) when is_list(Attribute) ->
