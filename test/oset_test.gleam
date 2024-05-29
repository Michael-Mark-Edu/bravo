import bravo/error
import bravo/etc
import bravo/oset
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn oset_insert_lookup_delete_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(#(100, 200)))
  oset.lookup(table, 300)
  |> should.equal(Some(#(300, 500)))
  oset.lookup(table, 600)
  |> should.equal(None)
}

pub fn oset_multitype_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  oset.lookup(table, "a")
  |> should.equal(Some(#("a", 1)))
  oset.lookup(table, "b")
  |> should.equal(Some(#("b", 2)))
  oset.lookup(table, "c")
  |> should.equal(None)
}

pub fn oset_large_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [
    #(
      900,
      800,
      700,
      600,
      500,
      400,
      300,
      200,
      100,
      0,
      -100,
      -200,
      -300,
      -400,
      -500,
      -600,
      -700,
      -800,
      -900,
    ),
  ])
  |> should.equal(True)
  oset.lookup(table, 900)
  |> should.equal(
    Some(#(
      900,
      800,
      700,
      600,
      500,
      400,
      300,
      200,
      100,
      0,
      -100,
      -200,
      -300,
      -400,
      -500,
      -600,
      -700,
      -800,
      -900,
    )),
  )
}

pub fn oset_keypos_test() {
  let assert Ok(table) = oset.new("MyTable", 2, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  oset.lookup(table, 200)
  |> should.equal(Some(#(100, 200)))
  oset.lookup(table, 500)
  |> should.equal(Some(#(300, 500)))
  oset.lookup(table, 100)
  |> should.equal(None)
}

pub fn oset_bad_new_test() {
  let assert Ok(table) = oset.new("table", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.new("table", 1, etc.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn oset_bad_insert_test() {
  let assert Ok(table) = oset.new("MyTable", 3, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("a", 1)])
  |> should.equal(False)
}

pub fn oset_multi_insert_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(#(100, 200)))
  oset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(#(100, 400)))
}

pub fn oset_large_multitype_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [
    #(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Some(50),
    ),
  ])
  |> should.equal(True)
  oset.lookup(table, "String")
  |> should.equal(
    Some(#(
      "String",
      5,
      10.0,
      [15, 20],
      #(25, 30),
      dict.from_list([#(35, 40)]),
      Ok(45),
      Some(50),
    )),
  )
}

pub fn oset_delete_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  oset.delete(table)
  |> should.equal(True)
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  oset.delete(table)
  |> should.equal(True)
  oset.delete(table)
  |> should.equal(False)
}

pub fn oset_singleton_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(1), #(2)])
  |> should.equal(True)
  oset.lookup(table, 1)
  |> should.equal(Some(#(1)))
  oset.lookup(table, 2)
  |> should.equal(Some(#(2)))
}

pub fn oset_nontuple_test() {
  let assert Ok(table) = oset.new("MyTable2", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [5])
  |> should.equal(True)
  oset.lookup(table, 5)
  |> should.equal(Some(5))
}

pub fn oset_nontuple_record_test() {
  let assert Ok(table) = oset.new("MyTable1", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [Ok(5)])
  |> should.equal(True)
  oset.lookup(table, Ok(5))
  |> should.equal(Some(Ok(5)))
}

type Multirecord {
  A(Int)
  B(Int, Int)
  C
}

pub fn oset_nontuple_multirecord_test() {
  let assert Ok(table) = oset.new("oset", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [A(1), B(2, 3), C])
  |> should.equal(True)
  oset.lookup(table, A(1))
  |> should.equal(Some(A(1)))
  oset.lookup(table, B(2, 3))
  |> should.equal(Some(B(2, 3)))
  oset.lookup(table, C)
  |> should.equal(Some(C))
}
