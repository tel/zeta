%% @doc Client connecting to a Riemann server
-module(zeta_client).
-author('Joseph Abrahamson <me@jspha.com>').

-include("include/zeta.hrl").

-export([start/0, start_link/0]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3]).

-record(st, {tcp :: inet:socket(),
	     udp :: inet:socket()}).

%% -------------
%% Lifecycle API

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% -----------------------
%% Gen server callback API

init(_Args) ->
    {ok, {Host, Port}} = application:get_env(client),
    %% Open UDP on a random port
    {ok, UDPSock} = gen_udp:open(0, [binary, {active,false}]),
    %% Try to make a TCP connection
    {ok, TCPSock} = gen_tcp:connect(Host, Port, 
				    [binary, {active, false}],
				    5000),
    {ok, #st{udp = UDPSock, tcp = TCPSock}}.

terminate(_Reason, #st{udp = UDPSock, tcp = TCPSock}) ->
    case UDPSock of
	undefined -> ok;
	USock -> gen_udp:close(USock)
    end,
    case TCPSock of
	undefined -> ok;
	TSock -> gen_udp:close(TSock)
    end.
    
handle_call({events, Msg}, _From, St = #st{tcp = TCP}) ->
    ok = gen_tcp:send(TCP, Msg),
    case gen_tcp:recv(TCP, 0, 2000) of
	{ok, Resp} -> 
	    case zeta_pb:pop(Resp) of
		{#zeta_msg{ok = true}, _} -> {reply, ok, St};
		{#zeta_msg{error = Error}, _} -> {error, {riemann, Error}};
		{none, _} -> {error, noparse}
	    end
    end;
handle_call(_Message, _From, State) -> {reply, ignored, State}.

handle_cast({events, Msg}, St = #st{udp = UDP}) -> 
    gen_udp:send(UDP, Msg),
    {noreply, St}.

handle_info(_Message, State) -> {ok, State}.
code_change(_Vsn, State, _Extra) -> {ok, State}.
