-module(zeta_util).
-author('Joseph Abrahamson <me@jspha.com>').

-export([stringify/1, ensure_string/1, lookup_alts/2]).

%% @doc Converts a list or tree of atoms/binaries/strings to a series
%% of "words". See the tests at the end of this source file for more
%% details.
-spec
stringify(zeta:stringable() | list(zeta:stringable())) -> string().
stringify(It) -> string:join(stringify_in(It), " ").

%% @doc Takes in whatever `stringify/1' hands it and returns a list of
%% string tokens (a list of lists).
stringify_in(B) when is_binary(B) -> [binary_to_list(B)];
stringify_in(A) when is_atom(A) -> [atom_to_list(A)];
stringify_in(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> [L];
        false -> lists:flatmap(fun stringify_in/1, L)
    end.

ensure_string(A) when is_atom(A) -> atom_to_list(A);
ensure_string(B) when is_binary(B) -> binary_to_list(B);
ensure_string(I) when is_integer(I) -> integer_to_list(I);
ensure_string(F) when is_float(F) -> float_to_list(F);
ensure_string(Else) -> Else.

lookup_alts([], _) -> undefined;
lookup_alts([K | Ks], List) ->
    case lists:keyfind(K, 1, List) of
        {K, V} -> V;
        false -> lookup_alts(Ks, List)
    end.
