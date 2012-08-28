%% @doc Client connecting to a Riemann server
-module(zeta_client).
-author('Joseph Abrahamson <me@jspha.com>').

-include("include/zeta.hrl").

-export([start/2, start_link/2]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3]).

-record(st, {tcp :: inet:socket(),
             udp :: inet:socket(),
             host :: inet:ip_address() | inet:hostname(),
             port :: integer()}).

%% -------------
%% Lifecycle API

start(Host, Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Host, Port], []).

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).


%% -----------------------
%% Gen server callback API

init([Host, Port]) ->
    %% Open UDP on a random port
    {ok, UDPSock} = gen_udp:open(0, [binary, {active,false}]),
    %% Try to make a TCP connection
    case gen_tcp:connect(Host, Port,
                         [binary, {active, false}],
                         5000) of
        {ok, TCPSock} ->
            {ok, #st{udp = UDPSock, tcp = TCPSock, host = Host, port = Port}};
        {error, econnrefused} ->
            error_logger:info_msg("zeta_client connection refused"),
            {stop, {shutdown, econnrefused}}
    end.

terminate(_Reason, #st{udp = UDPSock, tcp = TCPSock}) ->
    case UDPSock of
	undefined -> ok;
	USock -> gen_udp:close(USock)
    end,
    case TCPSock of
	undefined -> ok;
	TSock -> gen_tcp:close(TSock)
    end.
    
handle_call({events, Msg}, _From, St = #st{tcp = TCP}) ->
    case gen_tcp:send(TCP, Msg) of
        ok ->
            case gen_tcp:recv(TCP, 0, 2000) of
                {ok, Resp} ->
                    case zeta_pb:pop(Resp) of
                        {#zeta_msg{ok = true}, _} ->
                            {reply, ok, St};
                        {#zeta_msg{error = Error}, _} ->
                            {error, {riemann, Error}};
                        {none, _} -> {error, noparse}
                    end
            end;
        {error, closed} ->
            error_logger:info_msg("zeta_client disconnected"),
            {stop, {shutdown, connection_closed}, St}
    end;
handle_call(_Message, _From, State) -> {reply, ignored, State}.

handle_cast({events, Msg}, St = #st{udp = UDP, host = Host, port = Port}) ->
    gen_udp:send(UDP, Host, Port, Msg),
    {noreply, St}.

handle_info(_Message, State) -> {ok, State}.
code_change(_Vsn, State, _Extra) -> {ok, State}.
