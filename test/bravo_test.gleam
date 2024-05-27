import bravo/error
import bravo/object
import bravo/set
import bravo/table
import gleam/erlang/atom
import gleam/io
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn o2_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable1"), 2, table.Public, 1, False)
  set.insert(table, [object.O2(100, 200), object.O2(300, 500)])
  |> should.equal(True)
  set.lookup(table, 100)
  |> should.equal(Some(object.O2(100, 200)))
  set.lookup(table, 300)
  |> should.equal(Some(object.O2(300, 500)))
  set.lookup(table, 600)
  |> should.equal(None)
}

pub fn o3_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable2"), 3, table.Public, 1, False)
  set.insert(table, [object.O3(100, 200, 300), object.O3(400, 500, 600)])
  |> should.equal(True)
  set.lookup(table, 100)
  |> should.equal(Some(object.O3(100, 200, 300)))
  set.lookup(table, 400)
  |> should.equal(Some(object.O3(400, 500, 600)))
  set.lookup(table, 700)
  |> should.equal(None)
}

pub fn s2_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable3"), 2, table.Public, 1, True)
  set.insert(table, [object.S2("a", 1), object.S2("b", 2)])
  |> should.equal(True)
  set.lookup(table, "a")
  |> should.equal(Some(object.S2("a", 1)))
  set.lookup(table, "b")
  |> should.equal(Some(object.S2("b", 2)))
  set.lookup(table, "c")
  |> should.equal(None)
}

pub fn o9_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable4"), 9, table.Public, 1, False)
  set.insert(table, [
    object.O9(1, 2, 3, 4, 5, 6, 7, 8, 9),
    object.O9(11, 12, 13, 14, 15, 16, 17, 18, 19),
  ])
  |> should.equal(True)
  set.lookup(table, 1)
  |> should.equal(Some(object.O9(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  set.lookup(table, 11)
  |> should.equal(Some(object.O9(11, 12, 13, 14, 15, 16, 17, 18, 19)))
  set.lookup(table, 21)
  |> should.equal(None)
}

pub fn keypos_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable5"), 2, table.Public, 2, False)
  set.insert(table, [object.O2(100, 200), object.O2(300, 500)])
  |> should.equal(True)
  set.lookup(table, 200)
  |> should.equal(Some(object.O2(100, 200)))
  set.lookup(table, 500)
  |> should.equal(Some(object.O2(300, 500)))
  set.lookup(table, 100)
  |> should.equal(None)
}

pub fn bad_new_test() {
  set.new(atom.create_from_string("MyTable6"), 1, table.Public, 1, False)
  |> should.equal(Error(None))
  set.new(atom.create_from_string("MyTable7"), 2, table.Public, 3, False)
  |> should.equal(Error(None))
  set.new(atom.create_from_string("MyTable8"), 10, table.Public, 1, False)
  |> should.equal(Error(None))
  set.new(atom.create_from_string("MyTable9"), 2, table.Public, 1, False)
  |> should.equal(
    Ok(table.Set(atom.create_from_string("MyTable9"), 2, 1, False)),
  )
  set.new(atom.create_from_string("MyTable9"), 2, table.Public, 1, False)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn bad_insert_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable10"), 2, table.Public, 1, False)
  set.insert(table, [object.O2(100, 200), object.S2("a", 1)])
  |> should.equal(False)
  set.insert(table, [object.O2(100, 200), object.O3(300, 400, 500)])
  |> should.equal(False)
}

pub fn set_multi_insert_test() {
  let assert Ok(table) =
    set.new(atom.create_from_string("MyTable11"), 2, table.Public, 1, False)
  set.insert(table, [object.O2(100, 200)])
  |> should.equal(True)
  set.lookup(table, 100)
  |> should.equal(Some(object.O2(100, 200)))
  set.insert(table, [object.O2(100, 300), object.O2(100, 400)])
  |> should.equal(True)
  set.lookup(table, 100)
  |> should.equal(Some(object.O2(100, 400)))
}
