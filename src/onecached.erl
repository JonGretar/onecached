
-module(onecached).

-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

-export([start/0,
	 stop/0
	 ]).

start() ->
    application:start(sasl),
    application:start(mnesia),
    onecached_storage:init(mnesia),
    application:start(onecached).

stop() ->
    application:stop(onecached).
