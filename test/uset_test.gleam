import bravo
import bravo/uset
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/otp/task
import gleeunit/should
import shellout
import simplifile

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn uset_insert_lookup_delete_test() {
  let assert Ok(table) = uset.new("uset1", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert(table, "a", 1)
  |> should.be_ok
  uset.lookup(table, "a")
  |> should.equal(Ok(1))
  uset.lookup(table, "b")
  |> should.equal(Error(bravo.Empty))
  uset.insert_list(table, [#("b", 2), #("c", 3)])
  |> should.be_ok
  uset.lookup(table, "b")
  |> should.equal(Ok(2))
  uset.lookup(table, "c")
  |> should.equal(Ok(3))
}

pub fn uset_dupe_test() {
  let assert Ok(table) = uset.new("uset2", bravo.Public)
  uset.new("uset2", bravo.Public)
  |> should.equal(Error(bravo.TableAlreadyExists))
  uset.delete(table)
  |> should.be_ok
  let assert Ok(table2) = uset.new("uset2", bravo.Public)
  uset.insert(table, "Goodbye", "World")
  |> should.equal(Error(bravo.TableDoesNotExist))
  uset.insert(table2, "Goodbye", "World")
  |> should.be_ok
  uset.delete(table2)
  |> should.be_ok
}

pub fn uset_multi_insert_test() {
  let assert Ok(table) = uset.new("uset3", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert(table, 100, 200)
  |> should.be_ok
  uset.lookup(table, 100)
  |> should.equal(Ok(200))
  uset.insert(table, 100, 300)
  |> should.be_ok
  uset.lookup(table, 100)
  |> should.equal(Ok(300))
}

pub fn uset_large_multitype_test() {
  let assert Ok(table) = uset.new("uset4", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert(table, 0, #(
    "String",
    5,
    10.0,
    [15, 20],
    #(25, 30),
    dict.from_list([#(35, 40)]),
    Ok(45),
    Ok(50),
  ))
  |> should.be_ok
  uset.lookup(table, 0)
  |> should.equal(
    Ok(#(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Ok(50),
    )),
  )
}

pub fn uset_delete_key_test() {
  let assert Ok(table) = uset.new("uset5", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  uset.lookup(table, "Bye")
  |> should.equal(Ok("World"))
  uset.delete_key(table, "Bye")
  |> should.be_ok
  uset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
  uset.lookup(table, "Hello")
  |> should.equal(Ok("World"))
}

pub fn uset_delete_all_objects_test() {
  let assert Ok(table) = uset.new("uset6", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  uset.delete_all_objects(table)
  |> should.be_ok
  uset.lookup(table, "Hello")
  |> should.equal(Error(bravo.Empty))
  uset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn uset_delete_object_test() {
  let assert Ok(table) = uset.new("uset7", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  uset.delete_object(table, "Bye", "World")
  |> should.be_ok
  uset.lookup(table, "Hello")
  |> should.equal(Ok("World"))
  uset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn uset_delete_table_test() {
  let assert Ok(table) = uset.new("uset8", bravo.Public)
  uset.delete(table)
  |> should.be_ok
  uset.insert(table, "Hello", "World")
  |> should.equal(Error(bravo.TableDoesNotExist))
}

pub fn uset_tab2file_test() {
  let assert Ok(table) = uset.new("uset9", bravo.Public)
  uset.insert(table, "Hello", "World")
  |> should.be_ok
  uset.tab2file(table, "uset9", True, True, True)
  |> should.be_ok
  uset.delete(table)
  |> should.be_ok
  shellout.command("mkdir", ["no_access", "-p", "-m", "555"], ".", [])
  |> should.be_ok
  uset.tab2file(table, "no_access/uset9", True, True, True)
  |> should.equal(Error(bravo.NoFilePermissions))
}

pub fn uset_file2tab_test() {
  let assert Ok(new_table) =
    uset.file2tab("uset9", True, dynamic.string, dynamic.string)
  uset.lookup(new_table, "Hello")
  |> should.equal(Ok("World"))
  uset.delete(new_table)
  |> should.be_ok
  uset.file2tab("uset9", True, dynamic.int, dynamic.int)
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("uset9")
  |> should.be_ok
  uset.file2tab("no_access/uset9", True, dynamic.string, dynamic.string)
  |> should.equal(Error(bravo.FileDoesNotExist))
}

pub fn uset_access_test() {
  let assert Ok(table) = uset.new("uset10a", bravo.Protected)
  uset.insert(table, "Hello", "World")
  |> should.be_ok
  {
    use <- task.async
    uset.lookup(table, "Hello")
    |> should.equal(Ok("World"))
    uset.insert(table, "Goodbye", "World")
    |> should.equal(Error(bravo.AccessDenied))
  }
  |> task.await_forever
}

pub fn uset_tab2list_orderedness_test() {
  let assert Ok(table) = uset.new("uset11", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  let control = [
    #("A", "B"),
    #("Q", "S"),
    #("C", "F"),
    #("R", "Da"),
    #("Z", "DA"),
    #("B", "Db"),
    #("S", "a"),
    #("F", "A"),
    #("Da", "Q"),
    #("DA", "C"),
    #("Db", "R"),
    #("a", "Z"),
  ]
  uset.insert_list(table, control)
  |> should.be_ok
  let assert Ok(list) = uset.tab2list(table)
  list
  |> should.not_equal([
    #("A", "B"),
    #("B", "Db"),
    #("C", "F"),
    #("DA", "C"),
    #("Da", "Q"),
    #("Db", "R"),
    #("F", "A"),
    #("Q", "S"),
    #("R", "Da"),
    #("S", "a"),
    #("Z", "DA"),
    #("a", "Z"),
  ])
  list
  |> list.length
  |> should.equal(12)
  use elem <- list.each(control)
  list.contains(list, elem)
  |> should.be_true
}

pub fn uset_insert_new_test() {
  let assert Ok(table) = uset.new("uset12", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert_new(table, 1, 2)
  |> should.be_ok
  uset.insert_new_list(table, [#(1, 3), #(2, 4)])
  |> should.equal(Error(bravo.KeyAlreadyPresent))
  uset.lookup(table, 1)
  |> should.equal(Ok(2))
  uset.lookup(table, 2)
  |> should.equal(Error(bravo.Empty))
}

pub fn uset_take_test() {
  let assert Ok(table) = uset.new("uset13", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert_list(table, [#(1, 2), #(3, 4)])
  |> should.be_ok
  uset.lookup(table, 1)
  |> should.equal(Ok(2))
  uset.take(table, 1)
  |> should.equal(Ok(2))
  uset.take(table, 1)
  |> should.equal(Error(bravo.Empty))
  uset.lookup(table, 1)
  |> should.equal(Error(bravo.Empty))
  uset.lookup(table, 3)
  |> should.equal(Ok(4))
}

pub fn uset_member_test() {
  let assert Ok(table) = uset.new("uset14", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert_list(table, [#(1, 2), #(3, 4)])
  |> should.be_ok
  uset.member(table, 1)
  |> should.equal(Ok(True))
  uset.member(table, 2)
  |> should.equal(Ok(False))
  uset.delete_key(table, 1)
  |> should.be_ok
  uset.member(table, 1)
  |> should.equal(Ok(False))
  uset.member(table, 3)
  |> should.equal(Ok(True))
  uset.member(table, 4)
  |> should.equal(Ok(False))
}

pub fn uset_fn_test() {
  let assert Ok(table) = uset.new("uset15", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  let control = [
    #("A", "B"),
    #("Q", "S"),
    #("C", "F"),
    #("R", "Da"),
    #("Z", "DA"),
    #("B", "Db"),
    #("S", "a"),
    #("F", "A"),
    #("Da", "Q"),
    #("DA", "C"),
    #("Db", "R"),
    #("a", "Z"),
  ]
  uset.insert_list(table, control)
  |> should.be_ok
  let assert Ok(key) = table |> uset.first
  let assert Ok(a) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(b) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(c) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(d) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(e) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(f) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(g) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(h) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(i) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(j) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(k) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.next(key)
  let assert Ok(l) = uset.lookup(table, key)
  let list = []
  let list = list.append([a], list)
  let list = list.append([b], list)
  let list = list.append([c], list)
  let list = list.append([d], list)
  let list = list.append([e], list)
  let list = list.append([f], list)
  let list = list.append([g], list)
  let list = list.append([h], list)
  let list = list.append([i], list)
  let list = list.append([j], list)
  let list = list.append([k], list)
  let list = list.append([l], list)
  list.map(control, fn(elem) {
    list.contains(list, elem.1)
    |> should.equal(True)
  })
  table |> uset.next(key) |> should.equal(Error(bravo.EndOfTable))
}

pub fn uset_lp_test() {
  let assert Ok(table) = uset.new("uset16", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  let control = [
    #("A", "B"),
    #("Q", "S"),
    #("C", "F"),
    #("R", "Da"),
    #("Z", "DA"),
    #("B", "Db"),
    #("S", "a"),
    #("F", "A"),
    #("Da", "Q"),
    #("DA", "C"),
    #("Db", "R"),
    #("a", "Z"),
  ]
  uset.insert_list(table, control)
  |> should.be_ok
  let assert Ok(key) = table |> uset.last
  let assert Ok(a) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(b) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(c) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(d) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(e) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(f) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(g) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(h) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(i) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(j) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(k) = uset.lookup(table, key)
  let assert Ok(key) = table |> uset.prev(key)
  let assert Ok(l) = uset.lookup(table, key)
  let list = []
  let list = list.append([a], list)
  let list = list.append([b], list)
  let list = list.append([c], list)
  let list = list.append([d], list)
  let list = list.append([e], list)
  let list = list.append([f], list)
  let list = list.append([g], list)
  let list = list.append([h], list)
  let list = list.append([i], list)
  let list = list.append([j], list)
  let list = list.append([k], list)
  let list = list.append([l], list)
  list.map(control, fn(elem) {
    list.contains(list, elem.1)
    |> should.equal(True)
  })
  table |> uset.prev(key) |> should.equal(Error(bravo.EndOfTable))
}

pub fn uset_spec_test() {
  let assert Ok(table1) =
    bravo.spec("uset17a")
    |> bravo.access(bravo.Public)
    |> bravo.write_concurrency(bravo.Off)
    |> bravo.read_concurrency(False)
    |> bravo.decentralized_counters(False)
    |> bravo.compressed
    |> uset.from_spec
  uset.insert(table1, "Hello", "World")
  |> should.be_ok
  uset.lookup(table1, "Hello")
  |> should.equal(Ok("World"))
  let assert Ok(table2) =
    bravo.spec("uset17b")
    |> bravo.access(bravo.Private)
    |> bravo.write_concurrency(bravo.On)
    |> bravo.read_concurrency(True)
    |> bravo.decentralized_counters(True)
    |> uset.from_spec
  uset.insert(table2, "Hello", "World")
  |> should.be_ok
  uset.lookup(table2, "Hello")
  |> should.equal(Ok("World"))
  let assert Ok(table3) =
    bravo.spec("uset17c")
    |> bravo.write_concurrency(bravo.Auto)
    |> uset.from_spec
  uset.insert(table3, "Hello", "World")
  |> should.be_ok
  uset.lookup(table3, "Hello")
  |> should.equal(Ok("World"))
}

pub fn uset_tabfile_info_test() {
  let assert Ok(table) = uset.new("uset18", bravo.Public)
  use <- defer(fn() { uset.delete(table) |> should.be_ok })
  uset.insert(table, "Hello", "World")
  |> should.be_ok
  uset.tab2file(table, "uset18", False, True, True)
  |> should.be_ok
  use <- defer(fn() { simplifile.delete("uset18") |> should.be_ok })
  let assert Ok(info) = bravo.tabfile_info("uset18")
  info.name |> should.equal("uset18")
  info.table_type |> should.equal(bravo.Set)
  info.protection |> should.equal(bravo.Public)
  info.named_table |> should.equal(True)
  info.keypos |> should.equal(1)
  info.size |> should.equal(1)
  info.md5sum |> should.equal(True)
  info.object_count |> should.equal(False)
}
