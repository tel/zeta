%% @doc A gen server
-module(zeta_ns).
-author('Joseph Abrahamson <me@jspha.com>').


%% API
-export([start_link/0]).
-export([register_name/2, unregister_name/1, 
         whereis_name/1, send/2]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

-record(st, {names = dict:new()}).

%% --------------
%% Nameserver API

-spec
register_name(Name :: term(), pid()) -> 'yes' | 'no'.
register_name(Name, Pid) -> 
    gen_server:call(?SERVER, {register_name, Name, Pid}).

-spec 
unregister_name(Name :: term()) -> any().
unregister_name(Name) -> 
    gen_server:call(?SERVER, {unregister_name, Name}).

-spec 
whereis_name(Name :: term()) -> pid() | 'undefined'.
whereis_name(Name) ->
    gen_server:call(?SERVER, {whereis_name, Name}).

-spec 
send(Name :: term(), Msg :: any()) -> pid().
send(Name, Msg) -> 
    case whereis_name(Name) of
        undefined ->
            exit({badarg, {Name, Msg}});
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid
    end.

%% -------------
%% Lifecycle API

-spec 
start_link() -> {ok, pid()} | ignore | {error, _}.
%% @doc Starts the server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% --------------------
%% Gen Server callbacks

init([]) -> {ok, #st{}}.

handle_call({register_name, Name, Pid}, _From, St = #st{names = Names}) -> 
    case dict:find(Name, Names) of
        {ok, _} ->
            {reply, no, St};
        error ->
            {reply, yes, St#st{names = dict:store(Name, Pid, Names)}}
    end;
handle_call({unregister_name, Name}, _From, St = #st{names = Names}) -> 
    {reply, ok, St#st{names = dict:erase(Name, Names)}};
handle_call({whereis_name, Name}, _From, St = #st{names = Names}) -> 
    case dict:find(Name, Names) of
        {ok, Pid} ->
            {reply, Pid, St};
        error ->
            {reply, undefined, St}
    end;
handle_call(_Message, _From, St) -> {reply, ignored, St}.
handle_cast(_Message, St) -> {noreply, St}.

terminate(_Reason, _St) -> ok.

handle_info(_Message, St) -> {noreply, St}.
code_change(_OldVsn, St, _Extra) -> {ok, St}.

%% -----------------
%% Utility functions
