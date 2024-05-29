import bravo/error
import bravo/etc
import bravo/uset
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn uset_insert_lookup_delete_test() {
  let assert Ok(table) = uset.new("uset1", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(#(100, 200)))
  uset.lookup(table, 300)
  |> should.equal(Some(#(300, 500)))
  uset.lookup(table, 600)
  |> should.equal(None)
}

pub fn uset_multitype_test() {
  let assert Ok(table) = uset.new("uset2", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  uset.lookup(table, "a")
  |> should.equal(Some(#("a", 1)))
  uset.lookup(table, "b")
  |> should.equal(Some(#("b", 2)))
  uset.lookup(table, "c")
  |> should.equal(None)
}

pub fn uset_large_test() {
  let assert Ok(table) = uset.new("uset3", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [
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
  uset.lookup(table, 900)
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

pub fn uset_keypos_test() {
  let assert Ok(table) = uset.new("uset4", 2, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 200)
  |> should.equal(Some(#(100, 200)))
  uset.lookup(table, 500)
  |> should.equal(Some(#(300, 500)))
  uset.lookup(table, 100)
  |> should.equal(None)
}

pub fn uset_bad_new_test() {
  let assert Ok(table) = uset.new("uset5", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.new("uset5", 1, etc.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn uset_bad_insert_test() {
  let assert Ok(table) = uset.new("uset6", 3, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#("a", 1)])
  |> should.equal(False)
}

pub fn uset_multi_insert_test() {
  let assert Ok(table) = uset.new("uset7", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(#(100, 200)))
  uset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(#(100, 400)))
}

pub fn uset_large_multitype_test() {
  let assert Ok(table) = uset.new("uset8", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [
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
  uset.lookup(table, "String")
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

pub fn uset_delete_test() {
  let assert Ok(table) = uset.new("uset9", 1, etc.Public)
  uset.delete(table)
  |> should.equal(True)
  let assert Ok(table) = uset.new("uset9", 1, etc.Public)
  uset.delete(table)
  |> should.equal(True)
  uset.delete(table)
  |> should.equal(False)
}

pub fn uset_singleton_test() {
  let assert Ok(table) = uset.new("uset10", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(1), #(2)])
  |> should.equal(True)
  uset.lookup(table, 1)
  |> should.equal(Some(#(1)))
  uset.lookup(table, 2)
  |> should.equal(Some(#(2)))
}

pub fn uset_nontuple_test() {
  let assert Ok(table) = uset.new("uset11", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [5])
  |> should.equal(True)
  uset.lookup(table, 5)
  |> should.equal(Some(5))
}

pub fn uset_nontuple_record_test() {
  let assert Ok(table) = uset.new("uset12", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [Ok(5)])
  |> should.equal(True)
  uset.lookup(table, Ok(5))
  |> should.equal(Some(Ok(5)))
}

type Multirecord {
  A(Int)
  B(Int, Int)
  C
}

pub fn uset_nontuple_multirecord_test() {
  let assert Ok(table) = uset.new("uset13", 1, etc.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [A(1), B(2, 3), C])
  |> should.equal(True)
  uset.lookup(table, A(1))
  |> should.equal(Some(A(1)))
  uset.lookup(table, B(2, 3))
  |> should.equal(Some(B(2, 3)))
  uset.lookup(table, C)
  |> should.equal(Some(C))
}
