import bravo/error
import bravo/etc
import bravo/object
import bravo/oset
import gleam/dict
import gleam/dynamic
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
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  oset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  oset.lookup(table, 600)
  |> should.equal(None)
}

pub fn oset_insert_obj_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert_obj(table, [object.new(#(100, 200)), object.new(#(300, 500))])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  oset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  oset.lookup(table, 600)
  |> should.equal(None)
}

pub fn oset_multisize_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200, 300)])
  |> should.equal(True)
  oset.insert_obj(table, [object.new(#(400, 300, 200, 100))])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200, 300)))))
  oset.lookup(table, 400)
  |> should.equal(Some(object.new(dynamic.from(#(400, 300, 200, 100)))))
}

pub fn oset_multitype_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  oset.lookup(table, "a")
  |> should.equal(Some(object.new(dynamic.from(#("a", 1)))))
  oset.lookup(table, "b")
  |> should.equal(Some(object.new(dynamic.from(#("b", 2)))))
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

pub fn oset_keypos_test() {
  let assert Ok(table) = oset.new("MyTable", 2, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  oset.lookup(table, 200)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  oset.lookup(table, 500)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
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
  oset.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn oset_multi_insert_test() {
  let assert Ok(table) = oset.new("MyTable", 1, etc.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(100, 200)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  oset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  oset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 400)))))
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
  |> should.equal(Some(object.new(dynamic.from(#(1)))))
  oset.lookup(table, 2)
  |> should.equal(Some(object.new(dynamic.from(#(2)))))
}
