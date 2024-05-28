-module(bravo).

-export([try_insert/3]).
-export([try_new/2]).
-export([try_lookup/2]).
-export([try_delete/1]).

try_insert(Name, Keypos, Objects) ->
    case lists:all(fun(Elem) -> tuple_size(Elem) >= Keypos end, Objects) of
        true -> ets:insert(Name, Objects);
        false -> false
    end.

try_new(Name, Options) ->
    case (catch (ets:new(Name, Options))) of
        {'EXIT', {Reason, _}} -> {error, Reason};
        Other -> {ok, Other}
    end.

try_lookup(Name, Key) ->
    ets:lookup(Name, Key).


try_delete(Name) ->
    case (catch ets:delete(Name)) of
        {'EXIT', _} -> false;
        _ -> true
    end.
