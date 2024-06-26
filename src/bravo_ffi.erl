-module(bravo_ffi).

-export([inform/2, try_delete/1, try_delete_all_objects/1, try_delete_key/2,
         try_delete_object/2, try_file2tab/2, try_first/1, try_insert/3,
         try_insert_list/2, try_insert_new/3, try_insert_new_list/2, try_last/1,
         try_lookup/2, try_member/2, try_new/2, try_next/2, try_prev/2,
         try_tab2file/5, try_tab2list/1, try_take/2, try_whereis/1]).

inform(Name, Key) ->
  Info = catch ets:info(Name),
  {_, Target} = lists:keyfind(Key, 1, Info),
  Target.

get_error_type(Reason) ->
  case Reason of
    {badarg, Trace} ->
      case map_get(cause, element(2, lists:nth(1, element(4, lists:nth(1, Trace))))) of
        id -> {error, table_does_not_exist};
        access -> {error, access_denied};
        _ -> {error, {erlang_error, badarg}}
      end;
    _ -> {error, {erlang_error, atom_to_binary(Reason)}}
  end.

try_new(Name, Options) ->
  case catch ets:new(Name, Options) of
    {'EXIT', {badarg, _}} ->
      case ets:whereis(Name) of
        undefined -> {error, {erlang_error, atom_to_binary(badarg, utf8)}};
        _ -> {error, table_already_exists}
      end;
    {'EXIT', {Reason, _}} ->
      {error, {erlang_error, atom_to_binary(Reason, utf8)}};
    Other ->
      {ok, Other}
  end.

try_insert(Name, Key, Value) ->
  case catch ets:insert(Name, {Key, Value}) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
  end.

try_insert_list(Name, Objects) ->
  case catch ets:insert(Name, Objects) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
  end.

try_insert_new(Name, Key, Value) ->
  case catch ets:insert_new(Name, {Key, Value}) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
  end.

try_insert_new_list(Name, Object) ->
  todo.

try_lookup(Name, Key) ->
  case catch ets:lookup(Name, Key) of
    {'EXIT', Reason} -> get_error_type(Reason);
    Other ->
      case lists:map(fun(Elem) -> element(2, Elem) end, Other) of
        [] -> {error, empty};
        Value -> {ok, Value}
      end
  end.

try_take(Name, Key) ->
  case catch ets:take(Name, Key) of
    {'EXIT', Reason} -> get_error_type(Reason);
    Other ->
      case lists:map(fun(Elem) -> element(2, Elem) end, Other) of
        [] -> {error, empty};
        Value -> {ok, Value}
      end
  end.

try_member(Name, Key) ->
  case catch ets:member(Name, Key) of
    {'EXIT', Reason} -> get_error_type(Reason);
    Other -> {ok, Other}
  end.

try_delete(Name) ->
  case catch ets:delete(Name) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
  end.

try_delete_key(Name, Key) ->
  case catch ets:delete(Name, Key) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
  end.

try_delete_object(Name, Object) ->
  case catch ets:delete_object(Name, Object) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
  end.

try_delete_all_objects(Name) ->
  case catch ets:delete_all_objects(Name) of
    {'EXIT', Reason} -> get_error_type(Reason);
    _ -> {ok, nil}
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
    Other -> Other
  end.

try_tab2list(Name) ->
  case catch ets:tab2list(Name) of
    {'EXIT', Reason} -> get_error_type(Reason);
    Other -> {ok, Other}
  end.

try_first(Name) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:first(Name))) of
    {'EXIT', Reason} -> get_error_type(Reason);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.

try_last(Name) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:last(Name))) of
    {'EXIT', Reason} -> get_error_type(Reason);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.


try_next(Name, Key) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:next(Name, Key))) of
    {'EXIT', Reason} -> get_error_type(Reason);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.

try_prev(Name, Key) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:next(Name, Key))) of
    {'EXIT', Reason} -> get_error_type(Reason);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.

try_whereis(Atom) ->
  case ets:whereis(Atom) of
    undefined -> {error, nil};
    Other -> {ok, Other}
  end.
