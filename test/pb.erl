-module(pb).
-author('Joseph Abrahamson <me@jspha.com>').

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("include/zeta.hrl").

n(G) -> union([undefined, G]).
bstring() -> binary().
nstring() -> n(bstring()).
nfloat() -> n(?LET(Int, integer(), float(Int))).
nbool() -> n(boolean()).
zeta_state() ->
    #zeta_state{time = pos_integer(), 
		state = nstring(), 
		service = nstring(),
		host = nstring(), 
		description = nstring(), 
		once = nbool(),
		tags = list(bstring()),
		ttl = nfloat(),
		metric_f = nfloat()}.

zeta_event() ->
    #zeta_event{time = pos_integer(), 
		state = nstring(), 
		service = nstring(),
		host = nstring(), 
		description = nstring(), 
		tags = list(bstring()),
		ttl = nfloat(),
		metric_f = nfloat()}.

zeta_query() -> #zeta_query{string = nstring()}.

zeta_msg() ->
    #zeta_msg{ok = nbool(),
	      error = nstring(),
	      zstates = list(zeta_state()),
	      zquery = zeta_query(),
	      zevents = list(zeta_event())}.

% prop_encdec_state() ->
%     ?FORALL(State, zeta_state(), 
% 	    equals(zeta_pb:decode(zeta_pb:encode(State), #zeta_state{}),
% 		   State)).

% prop_encdec_event() ->
%     ?FORALL(Event, zeta_event(), 
% 	    equals(zeta_pb:decode(zeta_pb:encode(Event), #zeta_event{}),
% 		   Event)).

% prop_encdec_q() ->
%     ?FORALL(Q, zeta_query(), 
% 	    equals(zeta_pb:decode(zeta_pb:encode(Q), #zeta_query{}),
% 		   Q)).

prop_encdec_msg() ->
    ?FORALL(Msg, zeta_msg(), 
	    equals(zeta_pb:decode(zeta_pb:encode(Msg)),
		     Msg)).

proper_test_() ->
    [fun () -> [] = proper:module(?MODULE, [verbose, {numtests, 100}]) end].
