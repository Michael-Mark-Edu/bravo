-module(bravo_ffi).

-export([inform/2, try_delete/1, try_delete_all_objects/1, try_delete_key/2,
         try_delete_object/2, try_file2tab/2, try_first/1, try_insert/3,
         try_insert_new/3, try_last/1, try_lookup/2, try_member/2, try_new/2,
         try_next/2, try_prev/2, try_tab2file/5, try_tab2list/1, try_take/2,
         try_whereis/1]).

inform(Name, Key) ->
  Info = catch ets:info(Name),
  {_, Target} = lists:keyfind(Key, 1, Info),
  Target.

try_insert(Name, Keypos, Objects) ->
  Condition =
    case is_tuple(lists:nth(1, Objects)) of
      true ->
        not is_atom(element(1, lists:nth(1, Objects)));
      false ->
        false
    end,
  case catch(case Condition of
    true ->
      ets:insert('$BRAVOMETA', {Name, tuple}),
      case lists:all(fun(Elem) -> tuple_size(Elem) >= Keypos end, Objects) of
        true ->
          ets:insert(Name, Objects);
        false ->
          false
      end;
    false ->
      ets:insert('$BRAVOMETA', {Name, non_tuple}),
      case Keypos == 1 of
        true ->
          ets:insert(Name, lists:map(fun(Elem) -> {Elem} end, Objects));
        false ->
          false
      end
  end) of
    {'EXIT', _} -> false;
    Other -> Other
  end.

try_new(Name, Options) ->
  case ets:whereis('$BRAVOMETA') of
    undefined ->
      Meta = ets:new('$BRAVOMETA', [set, public, named_table, {keypos, 1}, {heir, none}]),
      ets:insert(Meta, {Name, unknown});
    Table ->
      ets:insert(Table, {Name, unknown})
  end,
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
      ets:insert('$BRAVOMETA', {Name, deleted}),
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
      Name = element(2, Other),
      ets:insert('$BRAVOMETA', {Name, case is_tuple(ets:lookup(Name, ets:first(Name))) of
                                        true -> non_tuple;
                                        false -> tuple
                                      end}),
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
  case catch(case Condition of
    true ->
      ets:insert('$BRAVOMETA', {Name, tuple}),
      case lists:all(fun(Elem) -> tuple_size(Elem) >= Keypos end, Objects) of
        true ->
          ets:insert_new(Name, Objects);
        false ->
          false
      end;
    false ->
      ets:insert('$BRAVOMETA', {Name, non_tuple}),
      case Keypos == 1 of
        true ->
          ets:insert_new(Name, lists:map(fun(Elem) -> {Elem} end, Objects));
        false ->
          false
      end
  end) of
    {'EXIT', _} -> false;
    Other -> Other
  end.

try_lookup(Name, Key) ->
  case catch(case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      ets:lookup(Name, Key);
    {_, non_tuple} ->
      Res = ets:lookup(Name, Key),
      lists:map(fun(Elem) -> element(1, Elem) end, Res)
  end) of
    {'EXIT', _} -> [];
    Other -> Other
  end.


try_delete_key(Name, Key) ->
  ets:delete(Name, Key).

try_delete_all_objects(Name) ->
  ets:delete_all_objects(Name).

try_delete_object(Name, Object) ->
  ets:delete_object(Name, Object).

try_tab2list(Name) ->
  case catch(case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      ets:tab2list(Name);
    {_, non_tuple} ->
      Res = ets:tab2list(Name),
      lists:map(fun(Elem) -> element(1, Elem) end, Res)
  end) of
    {'EXIT', _} -> [];
    Other -> Other
  end.

try_take(Name, Key) ->
  case catch(case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      ets:take(Name, Key);
    {_, non_tuple} ->
      {Res} = ets:take(Name, {Key}),
      Res
  end) of
    {'EXIT', _} -> [];
    Other -> Other
  end.

try_member(Name, Key) ->
  ets:member(Name, Key).

try_first(Name) ->
  case catch(case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      case ets:first(Name) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end;
    {_, non_tuple} ->
      case ets:first(Name) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end
  end) of
    {'EXIT', _} -> {error, nil};
    Other -> Other
  end.

try_last(Name) ->
  case catch(case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      case ets:last(Name) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end;
    {_, non_tuple} ->
      case ets:last(Name) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end
  end) of
    {'EXIT', _} -> {error, nil};
    Other -> Other
  end.


try_next(Name, Key) ->
  case catch(case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      case ets:next(Name, Key) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end;
    {_, non_tuple} ->
      case ets:next(Name, Key) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end
  end) of
    {'EXIT', _} -> {error, nil};
    Other -> Other
  end.

try_prev(Name, Key) ->
  case catch (case lists:nth(1, ets:lookup('$BRAVOMETA', Name)) of
    {_, unknown} ->
      [];
    {_, deleted} ->
      [];
    {_, tuple} ->
      case ets:prev(Name, Key) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end;
    {_, non_tuple} ->
      case ets:prev(Name, Key) of
        '$end_of_table' -> {error, nil};
        Val -> {ok, Val}
      end
  end) of
    {'EXIT', _} -> {error, nil};
    Other -> Other
  end.

try_whereis(Atom) ->
  case ets:whereis(Atom) of
    undefined -> {error, nil};
    Other -> {ok, Other}
  end.
