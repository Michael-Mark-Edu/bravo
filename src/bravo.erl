-module(bravo).

-export([try_insert/2]).
-export([try_new/2]).
-export([try_lookup/2]).
-export([try_delete/1]).

-import(bravo@object, [object/1]).
-import(bravo@table, [table/0]).

try_insert(Table, Objects) ->
    {_, Name, Size, _} = Table,
    case lists:all(fun(Elem) -> tuple_size(Elem) >= Size end, Objects) of
        true -> ets:insert(Name, Objects);
        false -> false
    end.

try_new(Name, Options) ->
    case (catch (ets:new(Name, Options))) of
        {'EXIT', {Reason, _}} -> {error, Reason};
        Other -> {ok, Other}
    end.

try_lookup(Table, Key) ->
    {_, Name, _, _} = Table,
    ets:lookup(Name, Key).


try_delete(Table) ->
    {_, Name, _, _} = Table,
    case (catch ets:delete(Name)) of
        {'EXIT', _} -> false;
        _ -> true
    end.
