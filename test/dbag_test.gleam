import bravo/dbag
import bravo/error
import bravo/object
import bravo/table
import gleam/dict
import gleam/dynamic
import gleam/option.{Some}
import gleeunit/should

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn dbag_insert_lookup_delete_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  dbag.lookup(table, 300)
  |> should.equal([object.new(dynamic.from(#(300, 500)))])
  dbag.lookup(table, 600)
  |> should.equal([])
}

pub fn dbag_insert_obj_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert_obj(table, [object.new(#(100, 200)), object.new(#(300, 500))])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  dbag.lookup(table, 300)
  |> should.equal([object.new(dynamic.from(#(300, 500)))])
  dbag.lookup(table, 600)
  |> should.equal([])
}

pub fn dbag_multisize_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200, 300)])
  |> should.equal(True)
  dbag.insert_obj(table, [object.new(#(400, 300, 200, 100))])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200, 300)))])
  dbag.lookup(table, 400)
  |> should.equal([object.new(dynamic.from(#(400, 300, 200, 100)))])
}

pub fn dbag_multitype_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  dbag.lookup(table, "a")
  |> should.equal([object.new(dynamic.from(#("a", 1)))])
  dbag.lookup(table, "b")
  |> should.equal([object.new(dynamic.from(#("b", 2)))])
  dbag.lookup(table, "c")
  |> should.equal([])
}

pub fn dbag_large_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
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
  dbag.lookup(table, 900)
  |> should.equal([
    object.new(
      dynamic.from(#(
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
    ),
  ])
}

pub fn dbag_keypos_test() {
  let assert Ok(table) = dbag.new("MyTable", 2, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  dbag.lookup(table, 200)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  dbag.lookup(table, 500)
  |> should.equal([object.new(dynamic.from(#(300, 500)))])
  dbag.lookup(table, 100)
  |> should.equal([])
}

pub fn dbag_bad_new_test() {
  let assert Ok(table) = dbag.new("table", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.new("table", 1, table.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn dbag_bad_insert_test() {
  let assert Ok(table) = dbag.new("MyTable", 3, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#("a", 1)])
  |> should.equal(False)
  dbag.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn dbag_multi_insert_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(100, 200)])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  dbag.insert(table, [#(100, 300), #(100, 400), #(100, 400)])
  |> should.equal(True)
  dbag.lookup(table, 100)
  |> should.equal([
    object.new(dynamic.from(#(100, 200))),
    object.new(dynamic.from(#(100, 300))),
    object.new(dynamic.from(#(100, 400))),
    object.new(dynamic.from(#(100, 400))),
  ])
}

pub fn dbag_large_multitype_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [
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
  dbag.lookup(table, "String")
  |> should.equal([
    object.new(
      dynamic.from(#(
        "String",
        5,
        10.0,
        [15, 20],
        #(25, 30),
        dict.from_list([#(35, 40)]),
        Ok(45),
        Some(50),
      )),
    ),
  ])
}

pub fn dbag_delete_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  dbag.delete(table)
  |> should.equal(True)
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  dbag.delete(table)
  |> should.equal(True)
  dbag.delete(table)
  |> should.equal(False)
}

pub fn dbag_singleton_test() {
  let assert Ok(table) = dbag.new("MyTable", 1, table.Public)
  use <- defer(fn() { dbag.delete(table) |> should.equal(True) })
  dbag.insert(table, [#(1), #(2)])
  |> should.equal(True)
  dbag.lookup(table, 1)
  |> should.equal([object.new(dynamic.from(#(1)))])
  dbag.lookup(table, 2)
  |> should.equal([object.new(dynamic.from(#(2)))])
}
