-module(onecached_server).
-behaviour(gen_fsm).

-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

-include("onecached.hrl").

%% External exports
-export([start/1, stop/1]).
%% internal function
-export([loop/3]).

%% gen_fsm callbacks
-export([init/1,
	 process_command/2,
	 process_data_block/2,
	 discard_data_block/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-record(state, {socket, storage, command}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Socket) ->
    gen_fsm:start(?MODULE, Socket, ?FSMOPTS).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

init(Socket) ->
    Storage = mnesia, % TODO -> get from parameters
    % run TCP server
    _Pid = proc_lib:spawn_link(?MODULE, loop, [self(), Socket, ""]),
    {ok, process_command, #state{socket=Socket, storage=Storage}}.

%%====================================================================
%% TCP server
%%====================================================================

% recieve TCP packets
loop(FSM_Pid, Socket, Data) ->
    %?DEBUG("loop ~p~n", [Socket]),
    case gen_tcp:recv(Socket, 0) of
	{ok, Packet} ->
	    %?DEBUG("Packet Received~n~p~n", [Packet]),
	    NewData = process_packet(FSM_Pid, Data++Packet),
	    loop(FSM_Pid, Socket, NewData);
	{error, closed} ->
	    ?DEBUG("closed~n", []),

	    ok;
	{error, Reason} ->
	    ?ERROR_MSG("Error receiving on socket ~p: ~p~n", [Socket, Reason]),
	    {error, Reason}
    end.

% parse TCP packet to find lines, and send them to the FSM
process_packet(FSM_Pid, Data) ->
    case read_line(Data) of
	{line, Line, NewData} ->
	    ?DEBUG("Line~n~p", [Line]),
	    gen_fsm:send_event(FSM_Pid, {line, Line}),
	    process_packet(FSM_Pid, NewData);
	 noline ->
	    Data
    end.

% Try to find the first line in the Data.
% return {line, Line, Rest_of_date} if found or
% noline
read_line(Data) ->
    read_line(Data, "").
read_line("", _Line) ->
    noline;
read_line("\r\n" ++ Data, Line) ->
    {line, lists:reverse(Line), Data};
read_line([Char|Data], Line) ->
    read_line(Data, [Char | Line]).

%%====================================================================
%% FSM Callbacks
%%====================================================================

% process a memcached storage command line
process_command({line, "set "++Line}, StateData) ->
    io:format("process_command set"),
    {next_state, process_data_block, StateData#state{command=parse_storage_command(Line)}};
process_command({line, "add "++Line}, StateData) ->
    StorageCommand = parse_storage_command(Line),
    case StorageCommand of
	#storage_command{key=Key} ->
	    NewStorageCommand = StateData#state{command=StorageCommand},
	    case onecached_storage:has_key(StateData#state.storage, Key) of
		true ->
		    {next_state, discard_data_block, NewStorageCommand};
		false ->
		    {next_state, process_data_block, NewStorageCommand}
	    end;
	_ ->
	    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~n", [Line]),
	    send_command(StateData#state.socket, "CLIENT_ERROR invalid command format "++Line),
	    {next_state, process_command, StateData}
    end;

process_command({line, "replace "++Line}, StateData) ->
    StorageCommand = parse_storage_command(Line),
    case StorageCommand of
	#storage_command{key=Key} ->
	    NewStorageCommand = StateData#state{command=StorageCommand},
	    case onecached_storage:has_key(StateData#state.storage, Key) of
		true ->
		    {next_state, process_data_block, NewStorageCommand};
		false ->
		    {next_state, discard_data_block, NewStorageCommand}
	    end;
	_ ->
	    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~n", [Line]),
	    send_command(StateData#state.socket, "CLIENT_ERROR invalid command format "++Line),
	    {next_state, process_command, StateData}
    end;

% process a memcached retrieval command line
process_command({line, "get "++Line}, #state{socket=Socket, storage=Storage}=StateData) ->
    Keys = parse_retrieval_command(Line),
    lists:foreach(fun(Key) ->
			  send_item(Socket, Storage, Key)
		  end, Keys),
    send_command(Socket, "END"),
    {next_state, process_command, StateData};

% unknown command
process_command({line, Line}, #state{socket=Socket} = StateData) ->
    ?ERROR_MSG("CLIENT_ERROR unknown command~n~p~n", [Line]),
    send_command(Socket, "CLIENT_ERROR unknown command "++Line),
    {next_state, process_command, StateData}.

% process data block that won't be stored
discard_data_block({line, Line}, #state{socket=Socket,
					command=#storage_command{bytes=Bytes}} = StateData) ->
    case length(Line) of
	Bytes ->
	    send_command(Socket, "NOT_STORED"),
	    {next_state, process_command, StateData#state{command=undefined}};
	Length ->
	    % -2 because we count the discarded "\r\n" in the Data block
	    {next_state, discard_data_block, StateData#state{command=#storage_command{bytes=Bytes-Length-2}}}
    end;
discard_data_block({line, Line}, #state{socket=Socket} = StateData) ->
    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~nState;~p~n", [Line, StateData]),
    send_command(Socket, "CLIENT_ERROR invalid command format "++Line),
    {next_state, process_command, StateData}.

% process data block that will be stored
process_data_block({line, Line}, #state{socket=Socket,
					storage=Storage,
					command=StorageCommand}=StateData)
  when is_record(StorageCommand, storage_command) ->
    Data = StorageCommand#storage_command.data,
    NewData = case Data of
		  "" ->
		      Line;
		  _ -> Data ++ "\r\n" ++ Line
	      end,
    NewStorageCommand = StorageCommand#storage_command{data=NewData},
    Bytes = StorageCommand#storage_command.bytes,
    case length(NewData) of
	Bytes ->
	    case onecached_storage:store_item(Storage, NewStorageCommand) of
		ok ->
		    send_command(Socket, "STORED");
		{error, Reason} ->
		    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Reason]),
		    send_command(Socket, "SERVER_ERROR "++Reason)
	    end,
	    {next_state, process_command, StateData#state{command=undefined}};
	_Length ->
	    {next_state, process_data_block,
	     StateData#state{command=NewStorageCommand}}
    end;
process_data_block({line, Line}, #state{socket=Socket} = StateData) ->
    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~n", [Line]),
    send_command(Socket, "CLIENT_ERROR invalid command format "++Line),
    {next_state, process_command, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, _StateData) ->
    ok.


%%====================================================================
%% Communication functions
%%====================================================================

send_command(Socket, Command) ->
    gen_tcp:send(Socket, Command++"\r\n").

send_item(Socket, Storage, Key) ->
    case onecached_storage:get_item(Storage, Key) of
	{ok, {Flags, Data}} ->
	    send_command(Socket,
			 io_lib:format("VALUE ~p ~p ~p", [Key, Flags, length(Data)])),
	    send_command(Socket, Data);
	none ->
	    ok;
	{error, Reason} ->
	    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Reason]),
	    send_command(Socket, "SERVER_ERROR "++Reason)
    end.

%%====================================================================
%% Helper functions
%%====================================================================

% Format of Line is
% <key> <flags> <exptime> <bytes>
% return #storage_command or error
parse_storage_command(Line) ->
    case string:tokens(Line, " ") of
	[Key, SFlags, SExptime, SBytes] ->
	    case {string:to_integer(SFlags),
		  string:to_integer(SExptime),
		  string:to_integer(SBytes)} of
		{{Flags, ""}, {Exptime, ""}, {Bytes, ""}} ->
		    #storage_command{key = Key,
				     flags = Flags,
				     exptime = Exptime,
				     bytes = Bytes};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

% Format of Line is
% <key>*
% return [Key] when is_list(Key)
parse_retrieval_command(Line) ->
    string:tokens(Line, " ").