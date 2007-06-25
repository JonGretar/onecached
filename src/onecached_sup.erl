%%%----------------------------------------------------------------------
%%% OneCached (c) 2007 Process-one (http://www.process-one.net/)
%%% $Id$
%%%----------------------------------------------------------------------

-module(onecached_sup).
-author('jerome.sautret@process-one.net').
-vsn('$Revision$ ').

% Supervisor of the OneCached listener

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 10},
          [{onecached_listener, {onecached_listener, start_link, []},
            permanent, brutal_kill, worker, [onecached_listener]}]
	 }}.
