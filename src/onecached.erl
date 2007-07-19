%%%----------------------------------------------------------------------
%%% OneCached (c) 2007 Process-one (http://www.process-one.net/)
%%% $Id$
%%%----------------------------------------------------------------------

-module(onecached).
-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

% Start all applications needed by OneCached

-export([start/0,
	 stop/0
	 ]).

start() ->
    application:start(sasl),
    application:start(mnesia),
    onecached_storage:init(mnesia),
    application:start(onecached).

stop() ->
    application:stop(onecached),
    application:stop(mnesia),
    application:stop(sasl).
