-module(bravo_ffi).

-export([try_delete/2, try_delete_all_objects/2, try_delete_key/3,
         try_delete_object/3, try_file2tab/2, try_first/2, try_insert/4,
         try_insert_list/3, try_insert_new/4, try_insert_new_list/3, try_last/2,
         try_lookup/3, try_member/3, try_new/2, try_next/3, try_prev/3,
         try_tab2file/5, try_tab2list/2, try_tabfile_info/1, try_take/3,
         try_whereis/1]).

get_error_type(Reason, Tid, Atom) ->
  case Reason of
    badarg ->
      case ets:whereis(Atom) == Tid of
        true -> {error, access_denied};
        false -> {error, table_does_not_exist}
      end;
    _ -> {error, {erlang_error, term_to_binary(Reason)}}
  end.

try_new(Name, Options) ->
  try {ok, ets:new(Name, Options)}
  catch
    _:badarg ->
      case ets:whereis(Name) of
        undefined -> {error, {erlang_error, atom_to_binary(badarg, utf8)}};
        _ -> {error, table_already_exists}
      end;
    _:Reason ->
      {error, {erlang_error, atom_to_binary(Reason, utf8)}}
  end.

try_insert(Tid, Atom, Key, Value) ->
  try ets:insert(Tid, {Key, Value}) of _ -> {ok, nil}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_insert_list(Tid, Atom, Objects) ->
  try ets:insert(Tid, Objects) of _ -> {ok, nil}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_insert_new(Tid, Atom, Key, Value) ->
  try {ok, ets:insert_new(Tid, {Key, Value})}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_insert_new_list(Tid, Atom, Objects) ->
  try {ok, ets:insert_new(Tid, Objects)}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_lookup(Tid, Atom, Key) ->
  try ets:lookup(Tid, Key) of
    Other ->
      case lists:map(fun(Elem) -> element(2, Elem) end, Other) of
        [] -> {error, empty};
        Value -> {ok, Value}
      end
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_take(Tid, Atom, Key) ->
  try ets:take(Tid, Key) of
    Other ->
      case lists:map(fun(Elem) -> element(2, Elem) end, Other) of
        [] -> {error, empty};
        Value -> {ok, Value}
      end
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_member(Tid, Atom, Key) ->
  try {ok, ets:member(Tid, Key)}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_delete(Tid, Atom) ->
  try ets:delete(Tid) of _ -> {ok, nil}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_delete_key(Tid, Atom, Key) ->
  try ets:delete(Tid, Key) of _ -> {ok, nil}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_delete_object(Tid, Atom, Object) ->
  try ets:delete_object(Tid, Object) of _ -> {ok, nil}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_delete_all_objects(Tid, Atom) ->
  try ets:delete_all_objects(Tid) of _ -> {ok, nil}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
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
  try ets:tab2file(Tid, Filename, Options) of
    ok -> {ok, nil};
    {error, eaccess} -> {error, no_file_permissions};
    {error, {file_error, _, eacces}} -> {error, no_file_permissions};
    {error, {file_error, _, enoent}} -> {error, invalid_path};
    {error, badtab} -> {error, table_does_not_exist};
    {error, Reason} -> term_to_binary(Reason)
  catch
    _:badarg -> {error, access_denied}; % this assumes the only possible cause of badarg is if table is private
    _:Reason -> {error, {erlang_error, atom_to_binary(Reason, utf8)}}
  end.

try_file2tab(Filename, Verify) ->
  try ets:file2tab(Filename, [{verify, Verify}]) of
    {ok, Atom} -> {ok, Atom};
    {error, {read_error, {file_error, _, enoent}}} -> {error, file_does_not_exist};
    {error, {read_error, {not_a_log_file, _}}} -> {error, file_is_not_table};
    {error, checksum_error} -> {error, checksum_error};
    {error, invalid_object_count} -> {error, invalid_object_count};
    {error, cannot_create_table} -> {error, table_already_exists};
    {error, Reason} -> {error, {erlang_error, atom_to_binary(Reason, utf8)}}
  catch _:Reason -> {error, {erlang_error, atom_to_binary(Reason, utf8)}}
  end.

try_tab2list(Tid, Atom) ->
  try {ok, ets:tab2list(Tid)}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_first(Tid, Atom) ->
  try ets:first(Tid) of
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_last(Tid, Atom) ->
  try ets:last(Tid) of
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_next(Tid, Atom, Key) ->
  try ets:next(Tid, Key) of
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_prev(Tid, Atom, Key) ->
  try ets:prev(Tid, Key) of
    '$end_of_table' -> {error, end_of_table};
    Other -> {ok, Other}
  catch _:Reason -> get_error_type(Reason, Tid, Atom)
  end.

try_whereis(Atom) ->
  case ets:whereis(Atom) of
    undefined -> {error, nil};
    Tid -> {ok, Tid}
  end.

try_tabfile_info(Filename) ->
  try ets:tabfile_info(Filename)
  catch _:Reason -> {error, {erlang_error, atom_to_binary(Reason, utf8)}}
  end.
