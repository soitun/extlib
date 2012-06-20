-module(extlib).

-compile(export_all).

slice(Len, List) ->
	slice(Len, List, []).

slice(_Len, [], Acc) ->
	lists:reverse(Acc);

slice(Len, List, Acc) when length(List) =< Len ->
	lists:reverse([List|Acc]);

slice(Len, List, Acc) when length(List) > Len ->
	{L1, L2} = lists:split(Len, List),
	slice(Len, L2, [L1|Acc]).

appvsn() ->
    {ok, App} = application:get_application(),
    case application:get_key(App, vsn) of
    {ok, Vsn} -> Vsn;
    undefined -> "unknown"
    end.

%copy from rabbitmq
module_with_attrs(App, Name) ->
	case application:get_key(App, modules) of
	{ok, Modules} ->
		lists:foldl(
		  fun (Module, Acc) ->
			  case lists:append([Atts || {N, Atts} <- module_attributes(Module),
										 N =:= Name]) of
			  []   -> Acc;
			  Atts -> [{Module, Atts} | Acc]
			  end
		  end, [], Modules);
	undefined ->
		[]
	end.

%copy from rabbitmq
module_attributes(Module) ->
    case catch Module:module_info(attributes) of
	{'EXIT', Reason} ->
		exit(Reason);
	V ->
		V
    end.

