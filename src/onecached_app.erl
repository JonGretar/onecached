%%%----------------------------------------------------------------------
%%% OneCached (c) 2007 Process-one (http://www.process-one.net/)
%%% $Id$
%%%----------------------------------------------------------------------

-module(onecached_app).
-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

% Start the OneCached application

-behaviour(application).

-export([start/2,
	 stop/1
	 ]).

start(normal, _Args) ->
    onecached_sup:start_link().

stop(_Args) ->
    ok.
