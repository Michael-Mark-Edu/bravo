-module(bravo_ffi).

-export([inform/2, try_delete/1, try_delete_all_objects/1, try_delete_key/2,
         try_delete_object/2, try_file2tab/2, try_insert/3, try_insert_new/3, try_lookup/2,
         try_member/2, try_new/2, try_tab2file/5, try_tab2list/1, try_take/2]).

inform(Name, Key) ->
  Info = catch ets:info(Name),
  {_, Target} = lists:keyfind(Key, 1, Info),
  Target.

get_singleton(Elem) ->
  case Elem of
    {Res, '$BRAVO_SINGLETON'} ->
      Res;
    Other ->
      Other
  end.

try_insert(Name, Keypos, Objects) ->
  Condition =
    case is_tuple(lists:nth(1, Objects)) of
      true ->
        not is_atom(element(1, lists:nth(1, Objects)));
      false ->
        false
    end,
  case Condition of
    true ->
      case lists:all(fun(Elem) -> tuple_size(Elem) >= Keypos end, Objects) of
        true ->
          ets:insert(Name, Objects);
        false ->
          false
      end;
    false ->
      case Keypos == 1 of
        true ->
          ets:insert(Name, lists:map(fun(Elem) -> {Elem, '$BRAVO_SINGLETON'} end, Objects));
        false ->
          false
      end
  end.

try_new(Name, Options) ->
  case catch ets:new(Name, Options) of
    {'EXIT', {Reason, _}} ->
      {error, {erlang_error, atom_to_binary(Reason, utf8)}};
    Other ->
      {ok, Other}
  end.

try_delete(Name) ->
  case catch ets:delete(Name) of
    {'EXIT', _} ->
      false;
    _ ->
      true
  end.

try_tab2file(Name, Filename, ObjectCount, Md5sum, Sync) ->
  A = if ObjectCount ->
           [object_count];
         true ->
           []
      end,
  B = if Md5sum ->
           [md5sum];
         true ->
           []
      end,
  C = [{extended_info, lists:append(A, B)}],
  D = [{sync, Sync}],
  Options = lists:append(C, D),
  case catch ets:tab2file(Name, Filename, Options) of
    ok ->
      {ok, nil};
    {'EXIT', {Reason, _}} ->
      {error, {erlang_error, atom_to_binary(Reason, utf8)}}
  end.

try_file2tab(Filename, Verify) ->
  case catch ets:file2tab(Filename, [{verify, Verify}]) of
    {'EXIT', {Reason, _}} ->
      {error, {erlang_error, atom_to_binary(Reason, utf8)}};
    Other ->
      Other
  end.

try_insert_new(Name, Keypos, Objects) ->
  Condition =
    case is_tuple(lists:nth(1, Objects)) of
      true ->
        not is_atom(element(1, lists:nth(1, Objects)));
      false ->
        false
    end,
  case Condition of
    true ->
      case lists:all(fun(Elem) -> tuple_size(Elem) >= Keypos end, Objects) of
        true ->
          ets:insert_new(Name, Objects);
        false ->
          false
      end;
    false ->
      case Keypos == 1 of
        true ->
          ets:insert_new(Name, lists:map(fun(Elem) -> {Elem, '$BRAVO_SINGLETON'} end, Objects));
        false ->
          false
      end
  end.

try_lookup(Name, Key) ->
  lists:map(fun get_singleton/1, ets:lookup(Name, Key)).

try_delete_key(Name, Key) ->
  ets:delete(Name, Key).

try_delete_all_objects(Name) ->
  ets:delete_all_objects(Name).

try_delete_object(Name, Object) ->
  ets:delete_object(Name, Object).

try_tab2list(Name) ->
  ets:tab2list(Name).

try_take(Name, Key) ->
  lists:map(fun get_singleton/1, ets:take(Name, Key)).

try_member(Name, Key) ->
  ets:member(Name, Key).
