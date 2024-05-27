-module(bravo).

-export([try_insert/2]).
-export([try_new/2]).
-export([set_lookup/2]).

-import(bravo@object, [object/1]).
-import(bravo@table, [table/0]).

% -spec set_insert(Set, Objects) -> {ok, nil} | {error, nil} when Set :: bravo@table:set(), Objects :: [bravo@object:object(term())].
try_insert(Set, Objects) ->
    {_, Table, _, _, _} = Set,
    NewObjs = lists:map(fun decapitate/1, Objects),
    ets:insert(Table, NewObjs).

try_new(Name, Options) ->
    case (catch (ets:new(Name, Options))) of
        {'EXIT', {Reason, _}} -> {error, Reason};
        Other -> {ok, Other}
    end.

set_lookup(Set, Key) ->
    {_, Table, Size, _, Stringed} = Set,
    List = ets:lookup(Table, Key),
    case length(List) of
        0 ->
            none;
        1 ->
            {some, set_lookup_b(List, Size, Stringed)}
    end.

set_lookup_b(List, Size, Stringed) ->
    S = lists:nth(1, List),
    case Size of
        2 when Stringed ->
            {s2, element(1, S), element(2, S)};
        2 ->
            {o2, element(1, S), element(2, S)};
        3 when Stringed ->
            {s3, element(1, S), element(2, S), element(3, S)};
        3 ->
            {o3, element(1, S), element(2, S), element(3, S)};
        4 when Stringed ->
            {s4, element(1, S), element(2, S), element(3, S), element(4, S)};
        4 ->
            {o4, element(1, S), element(2, S), element(3, S), element(4, S)};
        5 when Stringed ->
            {s5, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S)};
        5 ->
            {o5, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S)};
        6 when Stringed ->
            {s6, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S)};
        6 ->
            {o6, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S)};
        7 when Stringed ->
            {s7, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S), element(6, S)};
        7 ->
            {o7, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S), element(6, S)};
        8 when Stringed ->
            {s8, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S), element(7, S), element(8, S)};
        8 ->
            {o8, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S), element(7, S), element(8, S)};
        9 when Stringed ->
            {s9, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S), element(7, S), element(8, S), element(9, S)};
        9 ->
            {o9, element(1, S), element(2, S), element(3, S), element(4, S), element(5, S), element(6, S), element(7, S), element(8, S), element(9, S)}
    end.

decapitate(Object) ->
    List = tuple_to_list(Object),
    Decap =
        lists:delete(
            lists:nth(1, List), List),
    list_to_tuple(Decap).
