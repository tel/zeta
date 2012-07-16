%% @doc Zeta Protobuf support
-module(zeta_pb).
-compile([export_all]).
-author('Joseph Abrahamson <me@jspha.com>').

-export([pop/1, stream/1, encode/1, decode/1]).

-include("include/zeta.hrl").
-include("include/zeta_extra.hrl").

-spec
pop(binary()) -> {zmsg(), binary()}.
pop(<<Length:32/integer-big, Rest/binary>>) ->
    case Rest of
	<<Msg:Length/binary, Leftover/binary>> ->
	    {decode(Msg), Leftover};
	_ -> 
	    lager:warning("Failed to parse a zeta:zmsg()"),
	    {none, Rest}
    end;
pop(Else) -> {none, Else}.

-spec
stream(binary()) -> [zmsg()].
stream(Bin) ->
    stream(Bin, []).

stream(<<>>, Acc) -> lists:reverse(Acc);
stream(Bin, Acc) ->
    case pop(Bin) of
	{none, _} -> lists:reverse(Acc);
        {Msg, Rest} -> stream(Rest, [Msg | Acc])
    end.
    

-spec
encode(zstate() | zevent() | zquery() | zmsg()) -> binary().
encode(undefined) -> <<>>;
encode(
  #zeta_msg{ok = Ok, error = Error, 
	    zstates = States, zquery = Query, zevents = Events}
 ) ->
    BOk     = maybe_enc(?MSG_OK, Ok, msg_t(?MSG_OK)),
    BError  = maybe_enc(?MSG_ERROR, Error, msg_t(?MSG_ERROR)),
    BStates = lists:map(fun (State) ->
				maybe_enc(?MSG_ZSTATE, encode(State), msg_t(?MSG_ZSTATE))
			end, States),
    BQuery  = case Query of
		  undefined -> <<>>;
		  _ -> maybe_enc(?MSG_ZQUERY, encode(Query), msg_t(?MSG_ZQUERY))
	      end,
    BEvents = lists:map(fun (Event) -> 
				maybe_enc(?MSG_ZEVENT, encode(Event), msg_t(?MSG_ZEVENT))
			end, Events),
    erlang:iolist_to_binary(
      [BOk, BError, BStates, BQuery, BEvents]);
encode(
  #zeta_state{time = Time, state = State, service = Service, host = Host,
	      description = Desc, once = Once, tags = Tags, ttl = TTL, metric_f = MetricF}
 ) ->
    erlang:iolist_to_binary(
      [
       maybe_enc(?STATE_TIME, Time, state_t(?STATE_TIME)),
       maybe_enc(?STATE_STATE, State, state_t(?STATE_STATE)),
       maybe_enc(?STATE_SERVICE, Service, state_t(?STATE_SERVICE)),
       maybe_enc(?STATE_HOST, Host, state_t(?STATE_HOST)),
       maybe_enc(?STATE_DESCRIPTION, Desc, state_t(?STATE_DESCRIPTION)),
       maybe_enc(?STATE_ONCE, Once, state_t(?STATE_ONCE)),
       lists:map(fun(Tag) ->
			 maybe_enc(?STATE_TAG, Tag, state_t(?STATE_TAG))
		 end, Tags),
       maybe_enc(?STATE_TTL, TTL, state_t(?STATE_TTL)),
       maybe_enc(?STATE_METRICF, MetricF, state_t(?STATE_METRICF))
      ]);
encode(
  #zeta_event{time = Time, state = State, service = Service, host = Host, 
	      description = Desc, tags = Tags, ttl = TTL, metric_f = MetricF}
 ) ->
    erlang:iolist_to_binary(
      [
       maybe_enc(?EVENT_TIME, Time, event_t(?EVENT_TIME)),
       maybe_enc(?EVENT_STATE, State, event_t(?EVENT_STATE)),
       maybe_enc(?EVENT_SERVICE, Service, event_t(?EVENT_SERVICE)),
       maybe_enc(?EVENT_HOST, Host, event_t(?EVENT_HOST)),
       maybe_enc(?EVENT_DESCRIPTION, Desc, event_t(?EVENT_DESCRIPTION)),
       lists:map(fun(Tag) ->
			 maybe_enc(?STATE_TAG, Tag, event_t(?STATE_TAG))
		 end, Tags),
       maybe_enc(?EVENT_TTL, TTL, event_t(?EVENT_TTL)),
       maybe_enc(?EVENT_METRICF, MetricF, event_t(?EVENT_METRICF))
      ]);
encode(#zeta_query{string = String}) ->
    erlang:iolist_to_binary(
      [
       maybe_enc(?QUERY_STRING, String, query_t(?QUERY_STRING))
      ]).


decode(Bin) ->
    decode(Bin, #zeta_msg{}).

decode(<<>>, Result = #zeta_state{tags = Tags}) -> 
    Result#zeta_state{tags = lists:reverse(Tags)};
decode(<<>>, Result = #zeta_event{tags = Tags}) -> 
    Result#zeta_event{tags = lists:reverse(Tags)};
decode(<<>>, Result = #zeta_msg{zstates = States, zevents = Events}) -> 
    Result#zeta_msg{zstates = lists:reverse(States),
		    zevents = lists:reverse(Events)};
decode(<<>>, Result) -> Result;
decode(Bin, Msg = #zeta_msg{zstates = States, zevents = Events}) ->
    try protobuffs:read_field_num_and_wire_type(Bin) of
        {{Slot, _}, _} -> 
	    try protobuffs:decode(Bin, msg_t(Slot)) of
		{{?MSG_OK, 1}, Rest} ->
		    decode(Rest, Msg#zeta_msg{ok = true});
		{{?MSG_OK, 0}, Rest} ->
		    decode(Rest, Msg#zeta_msg{ok = false});
		{{?MSG_ERROR, Value}, Rest} ->
		    decode(Rest, Msg#zeta_msg{error = Value});
		{{?MSG_ZSTATE, Value}, Rest} ->
		    case decode(Value, #zeta_state{}) of
			{error, R, _} -> {error, {state_failed, R}};
			State -> 
                            decode(Rest, Msg#zeta_msg{zstates = [State | States]})
		    end;
		{{?MSG_ZEVENT, Value}, Rest} ->
		    case decode(Value, #zeta_event{}) of
			{error, R, _} -> {error, {event_failed, R}};
			Event ->
			    decode(Rest, Msg#zeta_msg{zevents = [Event | Events]})
		    end;
		{{?MSG_ZQUERY, Value}, Rest} ->
		    case decode(Value, #zeta_query{}) of
			{error, R, _} -> {error, {query_failed, R}};
			Query ->
			    decode(Rest, Msg#zeta_msg{zquery = Query})
		    end
	    catch
		error:function_clause -> {error, {noparse, zmsg}};
				   E:V -> {E, V}
	    end
    catch
	error:function_clause -> {error, {noparse, field, zmsg}}
    end;
decode(Bin, ZState = #zeta_state{tags = Tags}) ->
    try protobuffs:read_field_num_and_wire_type(Bin) of
        {{Slot, _}, _} -> 
	    try protobuffs:decode(Bin, state_t(Slot)) of
		{{?STATE_TIME, Time}, Rest} ->
		    decode(Rest, ZState#zeta_state{time = Time});
		{{?STATE_STATE, State}, Rest} ->
		    decode(Rest, ZState#zeta_state{state = State});
		{{?STATE_SERVICE, Service}, Rest} ->
		    decode(Rest, ZState#zeta_state{service = Service});
		{{?STATE_HOST, Host}, Rest} ->
		    decode(Rest, ZState#zeta_state{host = Host});
		{{?STATE_DESCRIPTION, Desc}, Rest} ->
		    decode(Rest, ZState#zeta_state{description = Desc});
		{{?STATE_ONCE, Once}, Rest} ->
		    case Once of
			1 -> decode(Rest, ZState#zeta_state{once = true});
			0 -> decode(Rest, ZState#zeta_state{once = false})
		    end;
		{{?STATE_TAG, Tag}, Rest} ->
		    decode(Rest, ZState#zeta_state{tags = [Tag | Tags]});
		{{?STATE_TTL, TTL}, Rest} ->
		    decode(Rest, ZState#zeta_state{ttl = TTL});
		{{?STATE_METRICF, MetricF}, Rest} ->
		    decode(Rest, ZState#zeta_state{metric_f = MetricF})
	    catch
		error:function_clause -> {error, {noparse, zstate}}
	    end
    catch
	error:function_clause -> {error, {noparse, field, zstate}}
    end;
decode(Bin, ZEvent = #zeta_event{tags = Tags}) ->
    try protobuffs:read_field_num_and_wire_type(Bin) of
        {{Slot, _}, _} -> 
	    try protobuffs:decode(Bin, event_t(Slot)) of
		{{?EVENT_TIME, Time}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{time = Time});
		{{?EVENT_STATE, State}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{state = State});
		{{?EVENT_SERVICE, Service}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{service = Service});
		{{?EVENT_HOST, Host}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{host = Host});
		{{?EVENT_DESCRIPTION, Desc}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{description = Desc});
		{{?EVENT_TAG, Tag}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{tags = [Tag | Tags]});
		{{?EVENT_TTL, TTL}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{ttl = TTL});
		{{?EVENT_METRICF, MetricF}, Rest} ->
		    decode(Rest, ZEvent#zeta_event{metric_f = MetricF})
	    catch
		error:function_clause -> {error, {noparse, zevent}}
	    end
    catch
	error:function_clause -> {error, {noparse, field, zevent}}
    end;
decode(Bin, ZQuery = #zeta_query{}) ->
    try protobuffs:read_field_num_and_wire_type(Bin) of
        {{Slot, _}, _} -> 
	    try protobuffs:decode(Bin, query_t(Slot)) of
		{{?QUERY_STRING, String}, Rest} ->
		    decode(Rest, ZQuery#zeta_query{string = String})
	    catch
		error:function_clause -> {error, {noparse, zquery}}
	    end
    catch
	error:function_clause -> {error, {noparse, field, zquery}}
    end.

%% Utilities

keyfindor(Match, PList, Default) ->
    case lists:keyfind(Match, 1, PList) of
	{Match, Val} -> Val;
	_ -> Default
    end.

keyfindall(Match, PList) -> 
    lists:foldl(
      fun ({K, V}, Acc) -> 
	      case Match =:= K of
		  true  -> [V | Acc];
		  false -> Acc
	      end
      end, [], PList).
			  

maybe_enc(_, undefined, _) -> <<>>;
maybe_enc(Key, Val, Type) -> protobuffs:encode(Key, Val, Type).
