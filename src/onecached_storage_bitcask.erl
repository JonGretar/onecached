-module(onecached_storage_bitcask).
-author('jongretar@jongretar.com').
-behaviour(onecached_storage).

-include("onecached.hrl").

-export([init/1,
	 store_item/2,
	 has_item/2,
	 get_item/2,
	 delete_item/2,
	 update_item_value/4,
	 flush_items/1]).

-record(onecached, {key, flags, exptime, data}).
	
init(_) ->
	bitcask:merge("./bc_data"),
	Handle = bitcask:open("./bc_data", [read_write]),
	Handle.


%% Store
store_item(State, #storage_command{key=Key} = Command) when is_list(Key) ->
	store_item(State, Command#storage_command{key=list_to_binary(Key)});
store_item(State, #storage_command{key=Key, flags=Flags, exptime=Exptime, data=Data}) when Exptime > 2592000 -> 
	% (Exptime > 30 days), it is an absolute Unix time
	Record = 	#onecached{key=Key, flags=Flags, exptime=Exptime, data=list_to_binary(Data)},
	bitcask:put(State, Key, term_to_binary(Record));
store_item(State, #storage_command{exptime=Exptime} = StorageCommand) -> 
	% Exptime is an offset
	{MegaSecs, Secs, _MicroSecs} = now(),
	store_item(State, StorageCommand#storage_command{exptime=Exptime + (MegaSecs*1000+Secs)}).

get_item(State, Key) when is_list(Key) ->
    get_item(State, list_to_binary(Key));
get_item(State, Key) ->
	case bitcask:get(State, Key) of
		not_found ->
			none;
		{ok, Value} ->
			{ok, check_value(State, value, binary_to_term(Value))}
	end.

% return true if an item with Key is present
has_item(State, Key) when is_list(Key) ->
	has_item(State, list_to_binary(Key));
has_item(State, Key) ->
	case get_item(State, Key) of
		{ok, _} ->
			true;
		_ ->
			false
	end.

delete_item(State, Key) when is_list(Key) ->
	delete_item(State, list_to_binary(Key));
delete_item(State, Key) ->
	case has_item(State, Key) of
		true ->
			bitcask:delete(State,Key);
		false ->
			none
	end.

flush_items(State) -> 
	F = fun(K,_V,_Acc0) ->
			bitcask:delete(State,K),
			[]
	end,
	bitcask:fold(State,F,[]),
	ok.

update_item_value(_State, _Key, _Value, _Operation) -> ok.


%% Private Functions
check_value(_State, value, #onecached{flags=Flags, data=Data, exptime=Exptime }) when Exptime =< 0 ->
	{Flags, Data};
check_value(State, value, #onecached{key=Key, flags=Flags, data=Data, exptime=Exptime }) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	case Exptime > MegaSecs*1000+Secs of
		true ->
			{Flags, Data};
		false ->
			delete_item(State, Key),
			none
	end.


