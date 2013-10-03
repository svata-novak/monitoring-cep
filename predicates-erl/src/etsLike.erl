%%%-------------------------------------------------------------------
%%% @author danos
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. X 2013 19:39
%%%-------------------------------------------------------------------
-module(etsLike).
-author("danos").

%% API
-export([]).

etsEval(PredEvents = {Event1, Event2}) ->
  %%   Spec = ets:fun2ms(fun({EV1 = [{_, Attr1}, {_, Attr2}, {_, Attr3}], [{_, Attr4}, {_, Attr5}, {_, Attr6}]}) when Attr1 =:= Attr4 andalso Attr2 > Attr6 -> true end),
  Compiled = ets:match_spec_compile([{
    {[{'_', '$1'}, {'_', '$2'}, {'_', '$3'}], [{'_', '$4'}, {'_', '$5'}, {'_', '$6'}]},
    [{'andalso', {'=:=', '$1', '$4'}, {'>', '$2', '$6'}}],
    [true]}]),

  ets:match_spec_run([PredEvents], Compiled).