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
			{ok, item_value(value, binary_to_term(Value))}
	end.

has_item(_State, _Val) -> ok.
delete_item(_State, _Val) -> ok.
update_item_value(_State, _Key, _Value, _Operation) -> ok.
flush_items(_State) -> ok.

%% Private Functions
item_value(value, #onecached{flags = Flags, data = Data}) ->
	{Flags, Data};
item_value(record, Item) when is_record(Item, onecached) ->
	Item.