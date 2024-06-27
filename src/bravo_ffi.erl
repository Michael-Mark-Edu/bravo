-module(bravo_ffi).

-export([try_delete/2, try_delete_all_objects/2, try_delete_key/3,
         try_delete_object/3, try_file2tab/2, try_first/2, try_insert/4,
         try_insert_list/3, try_insert_new/4, try_insert_new_list/3, try_last/2,
         try_lookup/3, try_member/3, try_new/2, try_next/3, try_prev/3,
         try_tab2file/5, try_tab2list/2, try_take/3, try_whereis/1]).

get_error_type(Reason, Tid, Atom) ->
  case Reason of
    {badarg, _} ->
      case ets:whereis(Atom) == Tid of
        true -> {error, access_denied};
        false -> {error, table_does_not_exist}
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

try_insert(Tid, Atom, Key, Value) ->
  case catch ets:insert(Tid, {Key, Value}) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_insert_list(Tid, Atom, Objects) ->
  case catch ets:insert(Tid, Objects) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_insert_new(Tid, Atom, Key, Value) ->
  case catch ets:insert_new(Tid, {Key, Value}) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_insert_new_list(Tid, Atom, Object) ->
  todo.

try_lookup(Tid, Atom, Key) ->
  case catch ets:lookup(Tid, Key) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    Other ->
      case lists:map(fun(Elem) -> element(2, Elem) end, Other) of
        [] -> {error, empty};
        Value -> {ok, Value}
      end
  end.

try_take(Tid, Atom, Key) ->
  case catch ets:take(Tid, Key) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    Other ->
      case lists:map(fun(Elem) -> element(2, Elem) end, Other) of
        [] -> {error, empty};
        Value -> {ok, Value}
      end
  end.

try_member(Tid, Atom, Key) ->
  case catch ets:member(Tid, Key) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    Other -> {ok, Other}
  end.

try_delete(Tid, Atom) ->
  case catch ets:delete(Tid) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_delete_key(Tid, Atom, Key) ->
  case catch ets:delete(Tid, Key) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_delete_object(Tid, Atom, Object) ->
  case catch ets:delete_object(Tid, Object) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_delete_all_objects(Tid, Atom) ->
  case catch ets:delete_all_objects(Tid) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    _ -> {ok, nil}
  end.

try_tab2file(Tid, Filename, ObjectCount, Md5sum, Sync) ->
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
  case catch ets:tab2file(Tid, Filename, Options) of
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

try_tab2list(Tid, Atom) ->
  case catch ets:tab2list(Tid) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    Other -> {ok, Other}
  end.

try_first(Tid, Atom) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:first(Tid))) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.

try_last(Tid, Atom) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:last(Tid))) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.


try_next(Tid, Atom, Key) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:next(Tid, Key))) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.

try_prev(Tid, Atom, Key) ->
  case catch(lists:map(fun(Elem) -> element(1, Elem) end, ets:next(Tid, Key))) of
    {'EXIT', Reason} -> get_error_type(Reason, Tid, Atom);
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  end.

try_whereis(Atom) ->
  case ets:whereis(Atom) of
    undefined -> {error, nil};
    Other -> {ok, Other}
  end.
