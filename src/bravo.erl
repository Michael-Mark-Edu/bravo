-module(bravo).

-export([try_insert/3]).
-export([try_new/2]).
-export([try_lookup/2]).
-export([try_delete/1]).

try_insert(Name, Keypos, Objects) ->
    Condition = case is_tuple(lists:nth(1, Objects)) of
                    true -> not is_atom(element(1, lists:nth(1, Objects)));
                    false -> false
                end,
    case Condition of
        true ->
            case lists:all(fun(Elem) -> tuple_size(Elem) >= Keypos end, Objects) of
                true -> ets:insert(Name, Objects);
                false -> false
            end;
        false -> 
            case Keypos == 1 of
                true -> ets:insert(Name, lists:map(fun(Elem) -> {Elem, '$BRAVO_SINGLETON'} end, Objects));
                false -> false
            end
    end.

try_new(Name, Options) ->
    case (catch (ets:new(Name, Options))) of
        {'EXIT', {Reason, _}} -> {error, Reason};
        Other -> {ok, Other}
    end.

try_lookup(Name, Key) ->
    case lists:map(fun get_singleton/1, ets:lookup(Name, Key)) of
        [] -> 
            Lookup = ets:lookup(Name, {Key, '$BRAVO_SINGLETON'}),
            Output = lists:map(fun get_singleton/1, Lookup),
            Output;
        Other -> Other 
    end.

get_singleton(Elem) ->
    case Elem of
        {Res, '$BRAVO_SINGLETON'} -> Res;
        Other -> Other
    end.

try_delete(Name) ->
    case (catch ets:delete(Name)) of
        {'EXIT', _} -> false;
        _ -> true
    end.
