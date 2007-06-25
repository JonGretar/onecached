%%%----------------------------------------------------------------------
%%% OneCached (c) 2007 Process-one (http://www.process-one.net/)
%%% $Id$
%%%----------------------------------------------------------------------

-module(onecached_listener).
-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

% Listen to clients connexions and spawn onecached_server

-include("onecached.hrl").

-export([start_link/0,
	init/0]).


start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init,
			     [])}.




init() ->
    register(?MODULE, self()),
    case gen_tcp:listen(?PORT, [list,
			       {packet, raw},
			       {active, false},
			       {reuseaddr, true},
			       {nodelay, true},
			       {keepalive, true}]) of
	{ok, LSock} ->
	    ?DEBUG("listen ~p~n", [LSock]),
	    accept(LSock);
	{error, Reason} ->
	    ?CRITICAL_MSG("Failed to open socket on port ~p: ~p",
		       [{?PORT}, Reason])
    end.

accept(ListenSocket) ->
    ?DEBUG("accept ~p~n", [ListenSocket]),
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    ?DEBUG("accepting ~p~n", [Socket]),
	    Pid = onecached_server:start(Socket),
	    %case gen_tcp:controlling_process(Socket, Pid) of
	    case ok of
		ok ->
		    ?DEBUG("Controlling process ~p", [Pid]),
		    ok;
		{error, Reason} ->
		    ?ERROR_MSG("(~w) Failed Controlling Process ~p: ~w",
			       [Socket, Pid, Reason]),
		    gen_tcp:close(Socket)
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("(~w) Failed TCP accept: ~w",
		       [ListenSocket, Reason])
    end,
    ?DEBUG("end accept~n", []),
    accept(ListenSocket).
