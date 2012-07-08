%% @doc A corral for holding Zeta clients

%% A corral is a one_for_one supervisor which holds on to the Zeta
%% client workers. Zeta clients are not robust! They are expected to
%% die at times and for long times---such as when Riemann itself is
%% unreachable---without necessarily mandating total release shutdown
%% as the original, naive implementation did.

%% Thus, they are `transient' processes and _corral has a sensible way
%% of creating new on the fly.

-module(zeta_corral).
-compile([{parse_transform, do}]).
-author('Joseph Abrahamson <me@jspha.com>').

-export([client/0, client/1]).

-export([start_link/0]).

-behaviour(supervisor).
-define(SUP, ?MODULE).
-export([init/1]).



%% Public API
%% ----------

-spec
%% @equiv client(default).
client() -> error_m:t(pid()).
client() -> client(default).

-spec
client(Name :: atom()) -> error_m:t(pid()).
client(Name) ->
    case
	do([error_m || 
	       {Host, Port, _} <- maybe_m:justok(zeta:client_config(Name)),
	       start_client(Name, Host, Port)]) 
	of
	{error, {already_started, Pid}} -> {ok, Pid};
	{ok, Pid} -> {ok, Pid};
	{error, Reason} -> {error, Reason}
    end.

start_client(Name, Host, Port) ->
      supervisor:start_child(
	?SUP,
	{Name, {zeta_client, start_link, [Host, Port]},
	 temporary, brutal_kill, worker, [zeta_client]}).


%% Lifecycle API
%% -------------

start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

%% Supervisor API
%% --------------

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, []}}.
