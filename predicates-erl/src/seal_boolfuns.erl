-module(seal_boolfuns).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

%% ====================================================================
%% Internal functions
%% ====================================================================

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