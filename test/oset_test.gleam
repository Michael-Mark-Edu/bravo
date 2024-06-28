import bravo
import bravo/bravo_options
import bravo/oset
import gleam/dict
import gleam/dynamic
import gleam/otp/task
import gleeunit/should
import shellout
import simplifile

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn oset_insert_lookup_delete_test() {
  let assert Ok(table) = oset.new("oset1", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert(table, "a", 1)
  |> should.be_ok
  oset.lookup(table, "a")
  |> should.equal(Ok(1))
  oset.lookup(table, "b")
  |> should.equal(Error(bravo.Empty))
  oset.insert_list(table, [#("b", 2), #("c", 3)])
  |> should.be_ok
  oset.lookup(table, "b")
  |> should.equal(Ok(2))
  oset.lookup(table, "c")
  |> should.equal(Ok(3))
}

pub fn oset_dupe_test() {
  let assert Ok(table) = oset.new("oset2", bravo.Public)
  oset.new("oset2", bravo.Public)
  |> should.equal(Error(bravo.TableAlreadyExists))
  oset.delete(table)
  |> should.be_ok
  let assert Ok(table2) = oset.new("oset2", bravo.Public)
  oset.insert(table, "Goodbye", "World")
  |> should.equal(Error(bravo.TableDoesNotExist))
  oset.insert(table2, "Goodbye", "World")
  |> should.be_ok
  oset.delete(table2)
  |> should.be_ok
}

pub fn oset_multi_insert_test() {
  let assert Ok(table) = oset.new("oset3", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert(table, 100, 200)
  |> should.be_ok
  oset.lookup(table, 100)
  |> should.equal(Ok(200))
  oset.insert(table, 100, 300)
  |> should.be_ok
  oset.lookup(table, 100)
  |> should.equal(Ok(300))
}

pub fn oset_large_multitype_test() {
  let assert Ok(table) = oset.new("oset4", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert(table, 0, #(
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
  oset.lookup(table, 0)
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

pub fn oset_delete_key_test() {
  let assert Ok(table) = oset.new("oset5", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  oset.lookup(table, "Bye")
  |> should.equal(Ok("World"))
  oset.delete_key(table, "Bye")
  |> should.be_ok
  oset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
  oset.lookup(table, "Hello")
  |> should.equal(Ok("World"))
}

pub fn oset_delete_all_objects_test() {
  let assert Ok(table) = oset.new("oset6", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  oset.delete_all_objects(table)
  |> should.be_ok
  oset.lookup(table, "Hello")
  |> should.equal(Error(bravo.Empty))
  oset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn oset_delete_object_test() {
  let assert Ok(table) = oset.new("oset7", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert_list(table, [#("Hello", "World"), #("Bye", "World")])
  |> should.be_ok
  oset.delete_object(table, "Bye", "World")
  |> should.be_ok
  oset.lookup(table, "Hello")
  |> should.equal(Ok("World"))
  oset.lookup(table, "Bye")
  |> should.equal(Error(bravo.Empty))
}

pub fn oset_delete_table_test() {
  let assert Ok(table) = oset.new("oset8", bravo.Public)
  oset.delete(table)
  |> should.be_ok
  oset.insert(table, "Hello", "World")
  |> should.equal(Error(bravo.TableDoesNotExist))
}

pub fn oset_tab2file_test() {
  let assert Ok(table) = oset.new("oset9", bravo.Public)
  oset.insert(table, "Hello", "World")
  |> should.be_ok
  oset.tab2file(table, "oset9", True, True, True)
  |> should.be_ok
  oset.delete(table)
  |> should.be_ok
  shellout.command("mkdir", ["no_access", "-p", "-m", "555"], ".", [])
  |> should.be_ok
  oset.tab2file(table, "no_access/oset9", True, True, True)
  |> should.equal(Error(bravo.NoFilePermissions))
}

pub fn oset_file2tab_test() {
  let assert Ok(new_table) =
    oset.file2tab("oset9", True, dynamic.string, dynamic.string)
  oset.lookup(new_table, "Hello")
  |> should.equal(Ok("World"))
  oset.delete(new_table)
  |> should.be_ok
  oset.file2tab("oset9", True, dynamic.int, dynamic.int)
  |> should.equal(Error(bravo.DecodeFailure))
  simplifile.delete("oset9")
  |> should.be_ok
  oset.file2tab("no_access/oset9", True, dynamic.string, dynamic.string)
  |> should.equal(Error(bravo.FileDoesNotExist))
}

pub fn oset_access_test() {
  let assert Ok(table) = oset.new("oset10", bravo.Protected)
  oset.insert(table, "Hello", "World")
  |> should.be_ok
  {
    use <- task.async
    oset.lookup(table, "Hello")
    |> should.equal(Ok("World"))
    oset.insert(table, "Goodbye", "World")
    |> should.equal(Error(bravo.AccessDenied))
  }
  |> task.await_forever
}

pub fn oset_tab2list_orderedness_test() {
  let assert Ok(table) = oset.new("oset11", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
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
  oset.insert_list(table, control)
  |> should.be_ok
  oset.tab2list(table)
  |> should.equal(
    Ok([
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
    ]),
  )
}

pub fn oset_insert_new_test() {
  let assert Ok(table) = oset.new("oset12", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert_new(table, 1, 2)
  |> should.be_ok
  oset.insert_new_list(table, [#(1, 3), #(2, 4)])
  |> should.equal(Error(bravo.KeyAlreadyPresent))
  oset.lookup(table, 1)
  |> should.equal(Ok(2))
  oset.lookup(table, 2)
  |> should.equal(Error(bravo.Empty))
}

pub fn oset_take_test() {
  let assert Ok(table) = oset.new("oset13", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert_list(table, [#(1, 2), #(3, 4)])
  |> should.be_ok
  oset.lookup(table, 1)
  |> should.equal(Ok(2))
  oset.take(table, 1)
  |> should.equal(Ok(2))
  oset.take(table, 1)
  |> should.equal(Error(bravo.Empty))
  oset.lookup(table, 1)
  |> should.equal(Error(bravo.Empty))
  oset.lookup(table, 3)
  |> should.equal(Ok(4))
}

pub fn oset_member_test() {
  let assert Ok(table) = oset.new("oset14", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
  oset.insert_list(table, [#(1, 2), #(3, 4)])
  |> should.be_ok
  oset.member(table, 1)
  |> should.equal(Ok(True))
  oset.member(table, 2)
  |> should.equal(Ok(False))
  oset.delete_key(table, 1)
  |> should.be_ok
  oset.member(table, 1)
  |> should.equal(Ok(False))
  oset.member(table, 3)
  |> should.equal(Ok(True))
  oset.member(table, 4)
  |> should.equal(Ok(False))
}

pub fn oset_fn_test() {
  let assert Ok(table) = oset.new("oset15", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
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
  oset.insert_list(table, control)
  |> should.be_ok
  let assert Ok(key) = table |> oset.first
  oset.lookup(table, key)
  |> should.equal(Ok("B"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Db"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("F"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("C"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Q"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("R"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("A"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("S"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Da"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("a"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("DA"))
  let assert Ok(key) = table |> oset.next(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Z"))
  table |> oset.next(key) |> should.equal(Error(bravo.EndOfTable))
}

pub fn oset_lp_test() {
  let assert Ok(table) = oset.new("oset16", bravo.Public)
  use <- defer(fn() { oset.delete(table) |> should.be_ok })
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
  oset.insert_list(table, control)
  |> should.be_ok
  let assert Ok(key) = table |> oset.last
  oset.lookup(table, key)
  |> should.equal(Ok("Z"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("DA"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("a"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Da"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("S"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("A"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("R"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Q"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("C"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("F"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("Db"))
  let assert Ok(key) = table |> oset.prev(key)
  oset.lookup(table, key)
  |> should.equal(Ok("B"))
  table |> oset.prev(key) |> should.equal(Error(bravo.EndOfTable))
}

pub fn oset_spec_test() {
  let assert Ok(table1) =
    bravo.spec("oset17a")
    |> bravo.access(bravo.Public)
    |> bravo.write_concurrency(bravo_options.Off)
    |> bravo.read_concurrency(False)
    |> bravo.decentralized_counters(False)
    |> bravo.compressed
    |> oset.from_spec
  oset.insert(table1, "Hello", "World")
  |> should.be_ok
  oset.lookup(table1, "Hello")
  |> should.equal(Ok("World"))
  let assert Ok(table2) =
    bravo.spec("oset17b")
    |> bravo.access(bravo.Private)
    |> bravo.write_concurrency(bravo_options.On)
    |> bravo.read_concurrency(True)
    |> bravo.decentralized_counters(True)
    |> oset.from_spec
  oset.insert(table2, "Hello", "World")
  |> should.be_ok
  oset.lookup(table2, "Hello")
  |> should.equal(Ok("World"))
  let assert Ok(table3) =
    bravo.spec("oset17c")
    |> bravo.write_concurrency(bravo_options.Auto)
    |> oset.from_spec
  oset.insert(table3, "Hello", "World")
  |> should.be_ok
  oset.lookup(table3, "Hello")
  |> should.equal(Ok("World"))
}
