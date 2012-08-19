%% @doc A corral for holding Zeta clients

%% A corral is a one_for_one supervisor which holds on to the Zeta
%% client workers. Zeta clients are not robust! They are expected to
%% die at times and for long times---such as when Riemann itself is
%% unreachable---without necessarily mandating total release shutdown
%% as the original, naive implementation did.

%% Thus, they are `transient' processes and _corral has a sensible way
%% of creating new on the fly.

-module(zeta_corral).
-author('Joseph Abrahamson <me@jspha.com>').

-export([start_client/3, start_clients/0]).

-export([start_link/0]).

-behaviour(supervisor).
-define(SUP, ?MODULE).
-export([init/1]).



%% Public API
%% ----------

start_client(Name, Host, Port) ->
    supervisor:start_child(
      ?SUP,
      {Name, {zeta_client, start_link, [Name, Host, Port]},
       temporary, brutal_kill, worker, [zeta_client]}).

start_clients() ->
    ClientConfigs =
        case application:get_env(zeta, clients) of
            {ok, Confs} -> Confs;
            _Else -> []
        end,
    lists:foreach(
      fun ({Name, {Host, Port, _}}) ->
              {ok, _} = start_client(Name, Host, Port)
      end, ClientConfigs).


%% Lifecycle API
%% -------------

start_link() ->
    Ret = supervisor:start_link({local, ?SUP}, ?MODULE, []),
    start_clients(),
    Ret.

%% Supervisor API
%% --------------

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, []}}.
