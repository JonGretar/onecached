-module(onecached_storage).

-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

-export([init/1,
	 store_item/2,
	 has_key/2,
	 get_item/2]).

-include("onecached.hrl").

-record(onecached, {key, flags, exptime, data}).

init(mnesia) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(onecached,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, onecached)}]).

store_item(mnesia, #storage_command{key=Key, flags=Flags, exptime=Exptime, data=Data})
  when Exptime > 60*60*24*30 -> % (Exptime > 30 days), it is an absolute Unix time
    case mnesia:dirty_write(
	   #onecached{key=Key,
		       flags=Flags,
		       exptime=Exptime,
		       data=Data}) of
	{'EXIT', Reason} ->
	    {error, Reason};
	 Other ->
	    Other
    end;

store_item(mnesia, #storage_command{exptime=Exptime} = StorageCommand) ->
    % Exptime is an offset
    {MegaSecs, Secs, _MicroSecs} = now(),
    store_item(mnesia, StorageCommand#storage_command{exptime=Exptime + (MegaSecs*1000+Secs)}).

% return true if a item with Key is present
has_key(Storage, Key) ->
    case get_item(Storage, Key) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

% Find the item with key Key, return
% {ok, Flags, Data}
% if found or
% none
% if not found or if the item has expired
% (in the later case, delete the item) or
% {error, Reason}
get_item(mnesia, Key) ->
    Fun = fun() ->
		  case mnesia:read({onecached, Key}) of
		      [#onecached{exptime=0, flags = Flags, data = Data}] ->
			  {ok, Flags, Data};
		      [#onecached{exptime=Exptime, flags = Flags, data = Data}] ->
			  {MegaSecs, Secs, _MicroSecs} = now(),
			  case Exptime > MegaSecs*1000+Secs of
			      true ->
				  {ok, {Flags, Data}};
			      false ->
				  mnesia:delete({onecached, Key}),
				  none
			  end;
		      [] ->
			  none;
		      {'EXIT', Reason} ->
			  {error, Reason}
		  end
	  end,
    case mnesia:transaction(Fun) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    {error, Reason}
    end.
