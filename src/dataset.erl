%%%----------------------------------------------------------------------
%%% File    : dataset.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : DataSet is a list that contains key-value tuples.
%%% Created : 08 Aug 2009
%%% License : http://www.opengoss.com/license
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(dataset).

-author('ery.lee@gmail.com').

-export([get_value/2, get_value/3, 
        key_replace/3, key_delete/2]).

get_value(Key, DataSet) ->
    case lists:keysearch(Key, 1, DataSet) of
	{value, {_, Value}} -> 
	    {value, Value};
	_ ->
	    {false, Key}
    end.

get_value(Key, DataSet, Default) ->
    case lists:keysearch(Key, 1, DataSet) of
	{value, {_, Value}} -> 
	    {value, Value};
	_ -> 
	    {value, Default}
    end.

key_replace(Key, DataSet, Tuple) ->
    lists:keyreplace(Key, 1, DataSet, Tuple).

key_delete(Key, DataSet) ->
    lists:keydelete(Key, 1, DataSet).

