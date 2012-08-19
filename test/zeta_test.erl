-module(zeta_test).
-author('Joseph Abrahamson <me@jspha.com>').

-include_lib("eunit/include/eunit.hrl").

-include("include/zeta.hrl").

-import(zeta_util, [stringify/1]).

stringify_test_() ->
    [
     {"inputs: just atoms", 
      [
       {"single",
        [
         ?_assertEqual("foo", stringify(foo)),
         ?_assertEqual("foo", stringify([foo]))
        ]},
       {"multiple",
        [
         ?_assertEqual("foo bar", stringify([foo, bar])),
         ?_assertEqual("foo bar baz", stringify([foo, bar, baz])),
         ?_assertEqual("foo bar baz quux", stringify([foo, bar, baz, quux]))
        ]},
       {"tree'd",
        [
         ?_assertEqual("foo bar", stringify([foo, [bar]])),
         ?_assertEqual("foo bar baz", stringify([foo, [bar, baz]])),
         ?_assertEqual("foo bar baz quux", stringify([foo, [bar, [baz, quux]]]))
        ]}
      ]},
     {"inputs: atoms and strings",
      [
       {"lists of",
        [
         ?_assertEqual("foo bar", stringify([foo, "bar"])),
         ?_assertEqual("foo bar baz", stringify([foo, "bar", baz])),
         ?_assertEqual("baz foo bar", stringify(["baz", foo, "bar"])),
         ?_assertEqual("baz bux foo bar", stringify(["baz bux", foo, "bar"])),
         ?_assertEqual("baz foo bux bar", stringify(["baz", foo, "bux bar"])),
         ?_assertEqual("baz bux foo bux bar", stringify(["baz bux", foo, "bux bar"]))
        ]},
       {"trees of",
        [
         ?_assertEqual("foo bar", stringify([foo, "bar"])),
         ?_assertEqual("foo bar baz", stringify([foo, ["bar", baz]])),
         ?_assertEqual("baz foo bar", stringify([["baz", foo, "bar"]])),
         ?_assertEqual("baz bux foo bar", stringify([["baz bux", foo], "bar"])),
         ?_assertEqual("baz foo bux bar", stringify([["baz", foo], "bux bar"])),
         ?_assertEqual("baz bux foo bux bar", stringify([["baz bux", [foo]], "bux bar"]))
        ]}
      ]},
     {"inputs: atoms and binaries",
      [
       {"lists of",
        [
         ?_assertEqual("foo bar", stringify([foo, <<"bar">>])),
         ?_assertEqual("foo bar baz", stringify([foo, <<"bar">>, baz])),
         ?_assertEqual("baz foo bar", stringify([<<"baz">>, foo, <<"bar">>])),
         ?_assertEqual("baz bux foo bar", stringify([<<"baz bux">>, foo, <<"bar">>])),
         ?_assertEqual("baz foo bux bar", stringify([<<"baz">>, foo, <<"bux bar">>])),
         ?_assertEqual("baz bux foo bux bar", stringify([<<"baz bux">>, foo, <<"bux bar">>]))
        ]},
       {"trees of",
        [
         ?_assertEqual("foo bar", stringify([foo, <<"bar">>])),
         ?_assertEqual("foo bar baz", stringify([foo, [<<"bar">>, baz]])),
         ?_assertEqual("baz foo bar", stringify([[<<"baz">>, foo, <<"bar">>]])),
         ?_assertEqual("baz bux foo bar", stringify([[<<"baz bux">>, foo], <<"bar">>])),
         ?_assertEqual("baz foo bux bar", stringify([[<<"baz">>, foo], <<"bux bar">>])),
         ?_assertEqual("baz bux foo bux bar", stringify([[<<"baz bux">>, [foo]], <<"bux bar">>]))
        ]}
      ]}
    ].
