import bravo/error
import bravo/object
import bravo/table
import bravo/uset
import gleam/dict
import gleam/dynamic
import gleam/erlang/atom
import gleam/io
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn insert_lookup_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable1"), 2, table.Public, 1)
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 600)
  |> should.equal(None)
}

pub fn insert_obj_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable2"), 2, table.Public, 1)
  uset.insert_obj(table, [object.new(#(100, 200)), object.new(#(300, 500))])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 600)
  |> should.equal(None)
}

pub fn multisize_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable3"), 3, table.Public, 1)
  uset.insert(table, [#(100, 200, 300)])
  |> should.equal(True)
  uset.insert_obj(table, [object.new(#(400, 300, 200, 100))])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200, 300)))))
  uset.lookup(table, 400)
  |> should.equal(Some(object.new(dynamic.from(#(400, 300, 200, 100)))))
}

pub fn multitype_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable4"), 2, table.Public, 1)
  uset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  uset.lookup(table, "a")
  |> should.equal(Some(object.new(dynamic.from(#("a", 1)))))
  uset.lookup(table, "b")
  |> should.equal(Some(object.new(dynamic.from(#("b", 2)))))
  uset.lookup(table, "c")
  |> should.equal(None)
}

pub fn large_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable5"), 9, table.Public, 1)
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

pub fn keypos_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable6"), 2, table.Public, 2)
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 200)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 500)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 100)
  |> should.equal(None)
}

pub fn bad_new_test() {
  uset.new(atom.create_from_string("MyTable7"), 1, table.Public, 1)
  |> should.equal(Error(None))
  uset.new(atom.create_from_string("MyTable8"), 2, table.Public, 3)
  |> should.equal(Error(None))
  uset.new(atom.create_from_string("table()"), 2, table.Public, 3)
  |> should.equal(Error(None))
  uset.new(atom.create_from_string("MyTable10"), 2, table.Public, 1)
  |> should.equal(Ok(table.USet(atom.create_from_string("MyTable10"), 2, 1)))
  uset.new(atom.create_from_string("MyTable10"), 2, table.Public, 1)
  |> should.equal(Error(Some(error.Badarg)))
}

pub fn bad_insert_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable11"), 3, table.Public, 1)
  uset.insert(table, [#("a", 1)])
  |> should.equal(False)
  uset.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
}

pub fn uset_multi_insert_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable12"), 2, table.Public, 1)
  uset.insert(table, [#(100, 200)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 400)))))
}

pub fn large_multitype_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable13"), 8, table.Public, 1)
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
