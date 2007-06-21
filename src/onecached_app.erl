-module(onecached_app).
-behaviour(application).

-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

-export([start/2,
	 stop/1
	 ]).

start(normal, _Args) ->
    onecached_sup:start_link().

stop(_Args) ->
    ok.
