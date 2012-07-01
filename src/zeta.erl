%% @doc Zeta: an Erlang client for Riemann

-module(zeta).
-author('Joseph Abrahamson <me@jspha.com>').

-export([ev/2, ev/3, ev/4, evh/2, evh/3, evh/4]).
-export([sv/2, sv/3, sv/4, svh/2, svh/3, svh/4]).

-behaviour(application).
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).
-define(SUP, zeta_sup).
-export([init/1]).

-include("include/zeta.hrl").

%% ----------
%% Public API

ev({Host, Service}, Metric) when is_list(Host) ->
    E = ev(Service, Metric),
    E#zeta_event{host = Host};
ev({Host, Service}, Metric) when is_atom(Host) ->
    E = ev(Service, Metric),
    E#zeta_event{host = atom_to_list(Host)};
ev(Service, Metric) when is_atom(Service) ->
    ev(atom_to_list(Service), Metric);
ev(Services = [Service | _], Metric) when is_atom(Service) ->
    ev(lists:map(fun atom_to_list/1, Services), Metric);
ev(Services = [Service | _], Metric) when is_list(Service) ->
    ev(string:join(Services, " "), Metric);
ev(Service = [Char | _], Metric) when is_integer(Char), is_number(Metric) ->
    #zeta_event{service = Service, metric_f = Metric}.

ev(Loc, Metric, State) ->
    ev(Loc, Metric, State, []).

ev(Loc, Metric, State, []) when is_atom(State) ->
    ev(Loc, Metric, atom_to_list(State), []);
ev(Loc, Metric, State, []) when is_list(State) ->
    E = ev(Loc, Metric),
    E#zeta_event{state = State};
ev(Loc, Metric, State, Opts) ->
    [Time, Description, Tags, TTL] = 
	lists:map(fun (K) -> lookup(K, Opts) end, 
		  [[t, time], [desc, description], [tag, tags], ttl]),
    E = ev(Loc, Metric, State, []),
    E#zeta_event{time = Time, 
		 description = Description, tags = process_tags(Tags, []),
		 ttl = TTL}.

process_tags([], Acc) -> Acc;
process_tags([Tag | Tags], Acc) when is_atom(Tag) ->
    process_tags(Tags, [atom_to_list(Tag) | Acc]);
process_tags([Tag | Tags], Acc) when is_list(Tag) ->
    process_tags(Tags, [Tag | Acc]).

evh(Service, Metric) ->
    ev({node(), Service}, Metric).

evh(Service, Metric, State) ->
    ev({node(), Service}, Metric, State).

evh(Service, Metric, State, Opts) ->
    ev({node(), Service}, Metric, State, Opts).

sv(Loc, Metric) -> sv(Loc, Metric, undefined).
sv(Loc, Metric, State) -> sv(Loc, Metric, State, []).
sv(Loc, Metric, State, Opts) ->
    E = ev(Loc, Metric, State, Opts),
    zeta_pb:message(E).

svh(Service, Metric) -> svh(Service, Metric, undefined).
svh(Service, Metric, State) -> svh(Service, Metric, State, []).
svh(Service, Metric, State, Opts) -> sv({node(), Service}, Metric, State, Opts).


lookup(K, List) when is_atom(K) ->
    case lists:keyfind(K, 1, List) of
	{K, V} -> V;
	false -> undefined
    end;
lookup([], _) -> undefined;
lookup([K | Ks], List) ->
    case lookup(K, List) of
	undefined -> lookup(Ks, List);
	V -> V
    end.

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
    Corral = {zeta_corral,
	      {zeta_corral, start_link, []},
	      permanent, 5000, supervisor, [zeta_corral]},
    {ok, {{one_for_one, 5, 10}, [Frontend, Corral]}}.


%% ---------
%% Utilities

estart(App) ->
    case application:start(App) of
	{error, {already_started, _}} -> ok;
	ok -> ok;
	Else -> Else
    end.
