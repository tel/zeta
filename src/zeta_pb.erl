%% @doc Zeta Protobuf support
-module(zeta_pb).
-author('Joseph Abrahamson <me@jspha.com>').

-export([encode/1, decode/1]).

-include("include/zeta.hrl").

-spec
encode(zstate() | zevent() | zquery() | zmsg()) -> binary().
encode(undefined) -> <<>>;
encode(
  #zeta_msg{ok = Ok, error = Error, 
	    zstates = States, zquery = Query, zevents = Events}
 ) ->
    BOk     = maybe_enc(?MSG_OK, Ok, ?MSG_OK_T),
    BError  = maybe_enc(?MSG_ERROR, Error, ?MSG_ERROR_T),
    BStates = lists:map(fun (State) ->
				maybe_enc(?MSG_ZSTATE, encode(State), ?MSG_ZSTATE_T)
			end, States),
    BQuery  = maybe_enc(?MSG_ZQUERY, encode(Query), ?MSG_ZQUERY_T),
    BEvents = lists:map(fun (Event) -> 
				maybe_enc(?MSG_ZEVENT, encode(Event), ?MSG_ZEVENT_T)
			end, Events),
    erlang:iolist_to_binary(
      [BOk, BError, BStates, BQuery, BEvents]);
encode(
  #zeta_state{time = Time, state = State, service = Service, host = Host,
	      description = Desc, once = Once, tags = Tags, ttl = TTL, metric_f = MetricF}
 ) ->
    erlang:iolist_to_binary(
      [
       maybe_enc(?STATE_TIME, Time, ?STATE_TIME_T),
       maybe_enc(?STATE_STATE, State, ?STATE_STATE_T),
       maybe_enc(?STATE_SERVICE, Service, ?STATE_SERVICE_T),
       maybe_enc(?STATE_HOST, Host, ?STATE_HOST_T),
       maybe_enc(?STATE_DESCRIPTION, Desc, ?STATE_DESCRIPTION_T),
       maybe_enc(?STATE_ONCE, Once, ?STATE_ONCE_T),
       lists:map(fun(Tag) ->
			 maybe_enc(?STATE_TAG, Tag, ?STATE_TAG_T)
		 end, Tags),
       maybe_enc(?STATE_TTL, TTL, ?STATE_TTL_T),
       maybe_enc(?STATE_METRICF, MetricF, ?STATE_METRICF_T)
      ]);
encode(
  #zeta_event{time = Time, state = State, service = Service, host = Host, 
	      description = Desc, tags = Tags, ttl = TTL, metric_f = MetricF}
 ) ->
    erlang:iolist_to_binary(
      [
       maybe_enc(?EVENT_TIME, Time, ?EVENT_TIME_T),
       maybe_enc(?EVENT_STATE, State, ?EVENT_STATE_T),
       maybe_enc(?EVENT_SERVICE, Service, ?EVENT_SERVICE_T),
       maybe_enc(?EVENT_HOST, Host, ?EVENT_HOST_T),
       maybe_enc(?EVENT_DESCRIPTION, Desc, ?EVENT_DESCRIPTION_T),
       lists:map(fun(Tag) ->
			 maybe_enc(?STATE_TAG, Tag, ?STATE_TAG_T)
		 end, Tags),
       maybe_enc(?EVENT_TTL, TTL, ?EVENT_TTL_T),
       maybe_enc(?EVENT_METRICF, MetricF, ?EVENT_METRICF_T)
      ]);
encode(
  #zeta_query{string = String}
 ) ->
    erlang:iolist_to_binary(
      [
       maybe_enc(?QUERY_STRING, String, ?QUERY_STRING_T)
      ]).

-spec
decode(binary()) -> {zmsg(), binary()}.
decode(_Bin) ->
    ok.

%% Utilities

maybe_enc(_, undefined, _) -> <<>>;
maybe_enc(Key, Val, Type) -> protobuffs:encode(Key, Val, Type).
