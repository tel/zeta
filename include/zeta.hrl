%%-*- mode: erlang -*-
-record(zeta_state,
	{
	  time :: undefined | integer(),	% 1  int64
	  state :: undefined | string(),	% 2  string
	  service :: undefined | string(),	% 3  string
	  host :: undefined | string(),		% 4  string
	  description :: undefined | string(),	% 5  string
	  once :: undefined | boolean(),	% 6  bool
	  tags = [] ::  [string(), ...],	% 7  string repeated
	  ttl :: undefined | float(),		% 8  float
	  metric_f :: undefined | float()	% 15 float
	 }).

-define(STATE_TIME, 1).
-define(STATE_STATE, 2).
-define(STATE_SERVICE, 3).
-define(STATE_HOST, 4).
-define(STATE_DESCRIPTION, 5).
-define(STATE_ONCE, 6).
-define(STATE_TAG, 7).
-define(STATE_TTL, 8).
-define(STATE_METRICF, 15).	

-record(zeta_event,
	{
	  time :: undefined | integer(),	% 1  int64
	  state :: undefined | string(),	% 2  string
	  service :: undefined | string(),	% 3  string
	  host :: undefined | string(),		% 4  string
	  description :: undefined | string(),	% 5  string
	  tags = [] :: [string(), ...],		% 7  string repeated
	  ttl :: undefined | float(),		% 8  float
	  metric_f :: undefined | float() 	% 15  float
	 }).

-define(EVENT_TIME, 1).
-define(EVENT_STATE, 2).
-define(EVENT_SERVICE, 3).
-define(EVENT_HOST, 4).
-define(EVENT_DESCRIPTION, 5).
-define(EVENT_ONCE, 6).
-define(EVENT_TAG, 7).
-define(EVENT_TTL, 8).
-define(EVENT_METRICF, 15).

-record(zeta_query,
	{
	  string :: undefined | string()	% 1 string
	 }).
-define(QUERY_STRING, 1).	

-record(zeta_msg,
	{
	  ok :: undefined | boolean(),		% 2 bool
	  error :: undefined | string(),	% 3 string
	  zstates = [] :: [zstate(), ...],	% 4 zeta_state repeated
	  zquery :: undefined |	zquery(),	% 5 zeta_query
	  zevents = [] :: [zevent(), ...]	% 6 zeta_event repeated
	 }).

-define(MSG_OK, 2).
-define(MSG_ERROR, 3).
-define(MSG_ZSTATE, 4).
-define(MSG_ZQUERY, 5).
-define(MSG_ZEVENT, 6).

-type zstate() :: #zeta_state{}.
-type zevent() :: #zeta_event{}.
-type zquery() :: #zeta_query{}.
-type zmsg()   :: #zeta_msg{}.
