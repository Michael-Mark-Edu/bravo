import bravo/bag
import bravo/error
import bravo/etc
import bravo/object
import gleam/dict
import gleam/dynamic
import gleam/option.{Some}
import gleeunit/should

fn defer(defer: fn() -> a, block: fn() -> b) -> b {
  let b = block()
  defer()
  b
}

pub fn bag_insert_lookup_delete_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  bag.lookup(table, 300)
  |> should.equal([object.new(dynamic.from(#(300, 500)))])
  bag.lookup(table, 600)
  |> should.equal([])
}

pub fn bag_insert_obj_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert_obj(table, [object.new(#(100, 200)), object.new(#(300, 500))])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  bag.lookup(table, 300)
  |> should.equal([object.new(dynamic.from(#(300, 500)))])
  bag.lookup(table, 600)
  |> should.equal([])
}

pub fn bag_multisize_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200, 300)])
  |> should.equal(True)
  bag.insert_obj(table, [object.new(#(400, 300, 200, 100))])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200, 300)))])
  bag.lookup(table, 400)
  |> should.equal([object.new(dynamic.from(#(400, 300, 200, 100)))])
}

pub fn bag_multitype_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  bag.lookup(table, "a")
  |> should.equal([object.new(dynamic.from(#("a", 1)))])
  bag.lookup(table, "b")
  |> should.equal([object.new(dynamic.from(#("b", 2)))])
  bag.lookup(table, "c")
  |> should.equal([])
}

pub fn bag_large_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [
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
  bag.lookup(table, 900)
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

pub fn bag_keypos_test() {
  let assert Ok(table) = bag.new("MyTable", 2, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  bag.lookup(table, 200)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  bag.lookup(table, 500)
  |> should.equal([object.new(dynamic.from(#(300, 500)))])
  bag.lookup(table, 100)
  |> should.equal([])
}

pub fn bag_bad_new_test() {
  let assert Ok(table) = bag.new("table", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.new("table", 1, etc.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn bag_bad_insert_test() {
  let assert Ok(table) = bag.new("MyTable", 3, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("a", 1)])
  |> should.equal(False)
  bag.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn bag_multi_insert_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(100, 200)])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([object.new(dynamic.from(#(100, 200)))])
  bag.insert(table, [#(100, 300), #(100, 400), #(100, 400)])
  |> should.equal(True)
  bag.lookup(table, 100)
  |> should.equal([
    object.new(dynamic.from(#(100, 200))),
    object.new(dynamic.from(#(100, 300))),
    object.new(dynamic.from(#(100, 400))),
  ])
}

pub fn bag_large_multitype_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [
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
  bag.lookup(table, "String")
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

pub fn bag_delete_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  bag.delete(table)
  |> should.equal(True)
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  bag.delete(table)
  |> should.equal(True)
  bag.delete(table)
  |> should.equal(False)
}

pub fn bag_singleton_test() {
  let assert Ok(table) = bag.new("MyTable", 1, etc.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(1), #(2)])
  |> should.equal(True)
  bag.lookup(table, 1)
  |> should.equal([object.new(dynamic.from(#(1)))])
  bag.lookup(table, 2)
  |> should.equal([object.new(dynamic.from(#(2)))])
}
