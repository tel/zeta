-module(pb).
-author('Joseph Abrahamson <me@jspha.com>').

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([prop_encdec_msg/0]).

-include("include/zeta.hrl").

n(G) -> elements([undefined, G]).
bstring() -> binary().
nstring() -> n(bstring()).
nfloat() -> n(?LET(Int, int(), float(Int))).
nbool() -> n(bool()).
zeta_state() ->
  ?LET({Time, State, Service, Host,
        Once,
        Desc, Tags, TTL, Metric},
       {largeint(), nstring(), nstring(), nstring(),
        nbool(),
        nstring(), list(bstring()), nfloat(), nfloat()},
       #zeta_state{time = Time,
                   state = State,
                   service = Service,
                   host = Host,
                   description = Desc,
                   once = Once,
                   tags = Tags,
                   ttl = TTL,
                   metric_f = Metric}).

zeta_event() ->
  ?LET({Time, State, Service, Host,
        Desc, Tags, TTL, Metric},
       {largeint(), nstring(), nstring(), nstring(),
        nstring(), list(bstring()), nfloat(), nfloat()},
       #zeta_event{time = Time,
                   state = State,
                   service = Service,
                   host = Host,
                   description = Desc,
                   tags = Tags,
                   ttl = TTL,
                   metric_f = Metric}).

zeta_query() -> ?LET(String, nstring(), #zeta_query{string = String}).

zeta_msg() ->
  ?LET({Ok, Error, ZStates, ZQuery, ZEvents},
       {bool(), nstring(), list(zeta_state()), zeta_query(), list(zeta_event())},
       #zeta_msg{ok = Ok,
                 error = Error,
                 zstates = ZStates,
                 zquery = ZQuery,
                 zevents = ZEvents}).
prop_encdec_msg() ->
  ?FORALL(Msg, zeta_msg(), 
          equals(zeta_pb:decode(zeta_pb:encode(Msg)),
                 Msg)).

proper_test_() ->
  [fun () -> [] = eqc:module(?MODULE) end].
