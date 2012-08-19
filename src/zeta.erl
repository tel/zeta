%% @doc Zeta: an Erlang client for Riemann

-module(zeta).
-author('Joseph Abrahamson <me@jspha.com>').

-export([ev/2, ev/3, ev/4, evh/2, evh/3, evh/4]).
-export([sv/2, sv/3, sv/4, svh/2, svh/3, svh/4]).
-export([cv/2, cv/3, cv/4, cvh/2, cvh/3, cvh/4]).
-export([sv_batch/1, cv_batch/1]).

-behaviour(application).
-define(APP, ?MODULE).
-define(NS, zeta_ns).
-export([start/0, start/2, stop/1]).

-export_types([stringable/0]).

-behaviour(supervisor).
-define(SUP, zeta_sup).
-export([init/1]).

-include("include/zeta.hrl").

%% ----------
%% Public API

-type stringable() :: atom() | binary() | string().

-type service() :: stringable() | list(stringable()).
-type loc_spec() :: stringable() | {stringable(), service()}.
-type event_opt() :: {t | time | desc | description | tag | tags | ttl, any()}.

-spec 
%% @doc Builds an event, trying to be as "DWIM" as possible.
ev(loc_spec() | service(), number()) -> zevent().
ev({Host, Service}, Metric) when is_list(Host) ->
    #zeta_event{service = zeta_util:stringify(Service),
                metric_f = Metric, 
                host = Host};
ev({Host, Service}, Metric) when is_atom(Host) ->
    #zeta_event{service = zeta_util:stringify(Service), 
                metric_f = Metric, 
                host = atom_to_list(Host)};
ev(Service, Metric) ->
    #zeta_event{service = zeta_util:stringify(Service),
                metric_f = Metric}.

ev(Loc, Metric, State) ->
    ev(Loc, Metric, State, []).

-spec
ev(loc_spec() | service(), number(), stringable(), [event_opt()]) -> zevent().
ev(Loc, Metric, State, []) when is_atom(State) ->
    ev(Loc, Metric, atom_to_list(State), []);
ev(Loc, Metric, State, []) when is_list(State) ->
    E = ev(Loc, Metric),
    E#zeta_event{state = State};
ev(Loc, Metric, State, Opts) ->
    [Time, Description, Tags, TTL] = 
        lists:map(fun (K) -> zeta_util:lookup_alts(K, Opts) end, 
                  [[t, time], [desc, description], [tag, tags], [ttl]]),
    E = ev(Loc, Metric, State, []),
    E#zeta_event{time = Time, 
                 description = Description, 
                 tags = lists:map(fun zeta_util:ensure_string/1, Tags),
                 ttl = TTL}.

evh(Service, Metric) ->
    ev({node(), Service}, Metric).

evh(Service, Metric, State) ->
    ev({node(), Service}, Metric, State).

evh(Service, Metric, State, Opts) ->
    ev({node(), Service}, Metric, State, Opts).

%% @doc Sends an event, trying to be as "DWIM" as possible.
sv(Loc, Metric) -> sv(Loc, Metric, undefined).
sv(Loc, Metric, State) -> sv(Loc, Metric, State, []).
sv(Loc, Metric, State, Opts) ->
    E = ev(Loc, Metric, State, Opts),
    sv_batch([E]).

sv_batch(Es) ->
    M = #zeta_msg{zevents = Es},
    Data = zeta_pb:encode(M),
    Length = byte_size(Data),
    %% The `try' allows for silent failure if we want to instrument
    %% and sometimes not run a collection server.
    try gen_server:call({via, ?NS, default}, 
                        {events, <<Length:32/integer-big, Data/binary>>})
    catch exit:{noproc, _} -> {error, no_client}
    end.

svh(Service, Metric) -> svh(Service, Metric, undefined).
svh(Service, Metric, State) -> svh(Service, Metric, State, []).
svh(Service, Metric, State, Opts) -> sv({node(), Service}, Metric, State, Opts).

cv(Loc, Metric) -> cv(Loc, Metric, undefined).
cv(Loc, Metric, State) -> cv(Loc, Metric, State, []).
cv(Loc, Metric, State, Opts) ->
    E = ev(Loc, Metric, State, Opts),
    cv_batch([E]).

cv_batch(Es) ->
    M = #zeta_msg{zevents = Es},
    Data = zeta_pb:encode(M),
    %% The `try' allows for silent failure if we want to instrument
    %% and sometimes not run a collection server.
    try gen_server:call({via, ?NS, default}, 
                        {events, Data})
    catch exit:{noproc, _} -> {error, no_client}
    end.

cvh(Service, Metric) -> cvh(Service, Metric, undefined).
cvh(Service, Metric, State) -> cvh(Service, Metric, State, []).
cvh(Service, Metric, State, Opts) -> cv({node(), Service}, Metric, State, Opts).



%% ---------------------
%% Application callbacks

start() ->
    _ = lists:map(fun(A) -> ok = estart(A) end,
		  [compiler, syntax_tools, lager]),
    application:start(zeta).

start(_StartType, _Args) ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

stop(_State) ->
    ok.


%% --------------------
%% Supervisor callbacks

init(_Args) ->
    NS = {zeta_ns,
          {zeta_ns, start_link, []},
          permanent, brutal_kill, worker, [zeta_ns]},
    Corral = {zeta_corral,
              {zeta_corral, start_link, []},
              permanent, 5000, supervisor, [zeta_corral]},
    {ok, {{rest_for_one, 5, 10}, [NS, Corral]}}.


%% ---------
%% Utilities

estart(App) ->
    case application:start(App) of
	{error, {already_started, _}} -> ok;
	ok -> ok;
	Else -> Else
    end.
