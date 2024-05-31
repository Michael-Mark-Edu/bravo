-module(bravo).

-export([try_insert/3]).
-export([try_new/2]).
-export([try_lookup/2]).
-export([try_delete/1]).
-export([try_delete_key/2]).
-export([try_delete_all_objects/1]).
-export([try_delete_object/2]).
-export([try_tab2file/5]).
-export([try_file2tab/2]).
-export([inform/2]).

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

try_delete_key(Name, Key) ->
    ets:delete(Name, Key).

try_delete_all_objects(Name) ->
    ets:delete_all_objects(Name).

try_delete_object(Name, Object) ->
    ets:delete_object(Name, Object).

try_tab2file(Name, Filename, ObjectCount, Md5sum, Sync) ->
    A = if ObjectCount -> [object_count]; true -> [] end,
    B = if Md5sum -> [md5sum]; true -> [] end,
    C = [{extended_info, lists:append(A, B)}],
    D = [{sync, Sync}],
    Options = lists:append(C, D),
    ets:tab2file(Name, Filename, Options).

try_file2tab(Filename, Verify) ->
    ets:file2tab(Filename, [{verify, Verify}]).

inform(Name, Key) ->
    Info = ets:info(Name),
    {_, Target} = lists:keyfind(Key, 1, Info),
    Target.
