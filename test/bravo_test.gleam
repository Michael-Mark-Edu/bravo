import bravo/bag
import bravo/dbag
import bravo/error
import bravo/object
import bravo/oset
import bravo/table
import bravo/uset
import gleam/dict
import gleam/dynamic
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

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

pub fn oset_insert_lookup_delete_test() {
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 2, table.Public)
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
  let assert Ok(table) = oset.new("table", 1, table.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.new("table", 1, table.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn oset_bad_insert_test() {
  let assert Ok(table) = oset.new("MyTable", 3, table.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#("a", 1)])
  |> should.equal(False)
  oset.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn oset_multi_insert_test() {
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
  oset.delete(table)
  |> should.equal(True)
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
  oset.delete(table)
  |> should.equal(True)
  oset.delete(table)
  |> should.equal(False)
}

pub fn oset_singleton_test() {
  let assert Ok(table) = oset.new("MyTable", 1, table.Public)
  use <- defer(fn() { oset.delete(table) |> should.equal(True) })
  oset.insert(table, [#(1), #(2)])
  |> should.equal(True)
  oset.lookup(table, 1)
  |> should.equal(Some(object.new(dynamic.from(#(1)))))
  oset.lookup(table, 2)
  |> should.equal(Some(object.new(dynamic.from(#(2)))))
}

pub fn bag_insert_lookup_delete_test() {
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 2, table.Public)
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
  let assert Ok(table) = bag.new("table", 1, table.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.new("table", 1, table.Public)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn bag_bad_insert_test() {
  let assert Ok(table) = bag.new("MyTable", 3, table.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#("a", 1)])
  |> should.equal(False)
  bag.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn bag_multi_insert_test() {
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
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
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
  bag.delete(table)
  |> should.equal(True)
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
  bag.delete(table)
  |> should.equal(True)
  bag.delete(table)
  |> should.equal(False)
}

pub fn bag_singleton_test() {
  let assert Ok(table) = bag.new("MyTable", 1, table.Public)
  use <- defer(fn() { bag.delete(table) |> should.equal(True) })
  bag.insert(table, [#(1), #(2)])
  |> should.equal(True)
  bag.lookup(table, 1)
  |> should.equal([object.new(dynamic.from(#(1)))])
  bag.lookup(table, 2)
  |> should.equal([object.new(dynamic.from(#(2)))])
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
