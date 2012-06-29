%% @doc Zeta: an Erlang client for Riemann

-module(zeta).
-author('Joseph Abrahamson <me@jspha.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).
-define(SUP, zeta_sup).
-export([init/1]).


%% ---------------------
%% Application callbacks

start() ->
    _ = lists:map(fun(A) -> ok = estart(A) end,
		  [compiler, syntax_tools, lager]).

start(_StartType, _Args) ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

stop(_State) ->
    ok.


%% --------------------
%% Supervisor callbacks

init(_Args) ->
    Frontend = {zeta_frontend, 
		{zeta_frontend, start_link, []},
		permanent, brutal_kill, worker, [zeta_frontend]},
    {ok, {{one_for_one, 5, 10}, [Frontend]}}.


%% ---------
%% Utilities

estart(App) ->
    case application:start(App) of
	{error, {already_started, _}} -> ok;
	ok -> ok;
	Else -> Else
    end.
