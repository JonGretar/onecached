%%%----------------------------------------------------------------------
%%% OneCached (c) 2007 Process-one (http://www.process-one.net/)
%%% $Id$
%%%----------------------------------------------------------------------

-define(PORT, 11211).
-define(MAX_PACKET_SIZE, 300000).

-record(storage_command, {key, flags, exptime, bytes, data=""}).

-define(FSMOPTS, [{debug, [trace]}]).
%-define(FSMOPTS, []).

-define(DEBUG_MODE, true).

-ifdef(DEBUG_MODE).
-define(DEBUG(Format, Args), error_logger:info_msg("D(~p:~p:~p) : "++Format++"~n",
						   [self(),?MODULE,?LINE]++Args)).
-else.
-define(DEBUG(F,A),[]).
-endif.

-define(INFO_MSG(Format, Args),
    error_logger:info_msg(Format, Args)).

-define(WARNING_MSG(Format, Args),
    error_logger:warning_msg(Format, Args)).

-define(ERROR_MSG(Format, Args),
    error_logger:error_msg(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    error_logger:error_msg("CRITICAL:~n"++Format, Args)).
