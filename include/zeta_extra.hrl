%%-*- mode: erlang -*-

state_t(?STATE_TIME) -> int64;
state_t(?STATE_STATE) -> string;
state_t(?STATE_SERVICE) -> string;
state_t(?STATE_HOST) -> string;
state_t(?STATE_DESCRIPTION) -> string;
state_t(?STATE_ONCE) -> bool;
state_t(?STATE_TAG) -> string;
state_t(?STATE_TTL) -> float;
state_t(?STATE_METRICF) -> float.

event_t(?EVENT_TIME) -> int64;
event_t(?EVENT_STATE) -> string;
event_t(?EVENT_SERVICE) -> string;
event_t(?EVENT_HOST) -> string;
event_t(?EVENT_DESCRIPTION) -> string;
event_t(?EVENT_ONCE) -> string;
event_t(?EVENT_TAG) -> string;
event_t(?EVENT_TTL) -> float;
event_t(?EVENT_METRICF) -> float.

query_t(?QUERY_STRING) -> string.

msg_t(?MSG_OK) -> bool;
msg_t(?MSG_ERROR) -> string;
msg_t(?MSG_ZSTATE) -> bytes;
msg_t(?MSG_ZQUERY) -> bytes;
msg_t(?MSG_ZEVENT) -> bytes.
