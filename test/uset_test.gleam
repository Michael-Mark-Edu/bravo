import bravo/error
import bravo/object
import bravo/table
import bravo/uset
import gleam/dict
import gleam/dynamic
import gleam/option.{None, Some}
import gleeunit/should

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn uset_insert_lookup_delete_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 600)
  |> should.equal(None)
}

pub fn uset_insert_obj_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert_obj(table, [object.new(#(100, 200)), object.new(#(300, 500))])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 600)
  |> should.equal(None)
}

pub fn uset_multisize_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200, 300)])
  |> should.equal(True)
  uset.insert_obj(table, [object.new(#(400, 300, 200, 100))])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200, 300)))))
  uset.lookup(table, 400)
  |> should.equal(Some(object.new(dynamic.from(#(400, 300, 200, 100)))))
}

pub fn uset_multitype_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  uset.lookup(table, "a")
  |> should.equal(Some(object.new(dynamic.from(#("a", 1)))))
  uset.lookup(table, "b")
  |> should.equal(Some(object.new(dynamic.from(#("b", 2)))))
  uset.lookup(table, "c")
  |> should.equal(None)
}

pub fn uset_large_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
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
    Some(
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
    ),
  )
}

pub fn uset_keypos_test() {
  let assert Ok(table) = uset.new("MyTable", 2, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 200)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 500)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 100)
  |> should.equal(None)
}

pub fn uset_bad_new_test() {
  let assert Ok(table) = uset.new("table", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.new("table", 1, table.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn uset_bad_insert_test() {
  let assert Ok(table) = uset.new("MyTable", 3, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#("a", 1)])
  |> should.equal(False)
  uset.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn uset_multi_insert_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(100, 200)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 400)))))
}

pub fn uset_large_multitype_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
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
    Some(
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
    ),
  )
}

pub fn uset_delete_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  uset.delete(table)
  |> should.equal(True)
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  uset.delete(table)
  |> should.equal(True)
  uset.delete(table)
  |> should.equal(False)
}

pub fn uset_singleton_test() {
  let assert Ok(table) = uset.new("MyTable", 1, table.Public)
  use <- defer(fn() { uset.delete(table) |> should.equal(True) })
  uset.insert(table, [#(1), #(2)])
  |> should.equal(True)
  uset.lookup(table, 1)
  |> should.equal(Some(object.new(dynamic.from(#(1)))))
  uset.lookup(table, 2)
  |> should.equal(Some(object.new(dynamic.from(#(2)))))
}
