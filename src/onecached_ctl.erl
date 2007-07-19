%%%----------------------------------------------------------------------
%%% OneCached (c) 2007 Process-one (http://www.process-one.net/)
%%% $Id$
%%%----------------------------------------------------------------------

-module(onecached_ctl).
-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

% remote control of the server

-export([start/0, process/1]).

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = list_to_atom(SNode),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     io:format("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    io:format("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(onecached, 1, application:which_applications()) of
        false ->
            io:format("OneCached is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            io:format("OneCached is running~n", []),
            ?STATUS_SUCCESS
    end;

process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(_) ->
    print_usage(),
    ?STATUS_ERROR.


print_usage() ->
    io:format(
      "Usage: onecachedctl command~n"
      "~n"
      "Available commands:~n"
      "  start~n"
      "  stop~n"
      "  restart~n"
      "  status~n"
      "~n"
      "Example:~n"
      "  onecachedctl restart~n"
     ).
