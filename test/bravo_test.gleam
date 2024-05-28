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

pub fn insert_lookup_delete_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 1)
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 600)
  |> should.equal(None)
  uset.delete(table)
  |> should.equal(True)
}

pub fn insert_obj_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 1)
  uset.insert_obj(table, [object.new(#(100, 200)), object.new(#(300, 500))])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 300)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 600)
  |> should.equal(None)
  uset.delete(table)
  |> should.equal(True)
}

pub fn multisize_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 3, table.Public, 1)
  uset.insert(table, [#(100, 200, 300)])
  |> should.equal(True)
  uset.insert_obj(table, [object.new(#(400, 300, 200, 100))])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200, 300)))))
  uset.lookup(table, 400)
  |> should.equal(Some(object.new(dynamic.from(#(400, 300, 200, 100)))))
  uset.delete(table)
  |> should.equal(True)
}

pub fn multitype_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 1)
  uset.insert(table, [#("a", 1), #("b", 2)])
  |> should.equal(True)
  uset.lookup(table, "a")
  |> should.equal(Some(object.new(dynamic.from(#("a", 1)))))
  uset.lookup(table, "b")
  |> should.equal(Some(object.new(dynamic.from(#("b", 2)))))
  uset.lookup(table, "c")
  |> should.equal(None)
  uset.delete(table)
  |> should.equal(True)
}

pub fn large_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 9, table.Public, 1)
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
  uset.delete(table)
  |> should.equal(True)
}

pub fn keypos_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 2)
  uset.insert(table, [#(100, 200), #(300, 500)])
  |> should.equal(True)
  uset.lookup(table, 200)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.lookup(table, 500)
  |> should.equal(Some(object.new(dynamic.from(#(300, 500)))))
  uset.lookup(table, 100)
  |> should.equal(None)
  uset.delete(table)
  |> should.equal(True)
}

pub fn bad_new_test() {
  uset.new(atom.create_from_string("table1"), 1, table.Public, 1)
  |> should.equal(Error(None))
  uset.new(atom.create_from_string("table2"), 2, table.Public, 3)
  |> should.equal(Error(None))
  uset.new(atom.create_from_string("table()"), 2, table.Public, 3)
  |> should.equal(Error(None))
  let assert Ok(table) =
    uset.new(atom.create_from_string("table3"), 2, table.Public, 1)
  uset.new(atom.create_from_string("table3"), 2, table.Public, 1)
  |> should.equal(Error(Some(error.Badarg)))
  uset.delete(table)
  |> should.equal(True)
}

pub fn bad_insert_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 3, table.Public, 1)
  uset.insert(table, [#("a", 1)])
  |> should.equal(False)
  uset.insert(table, [#(300, 400, 500)])
  |> should.equal(True)
  uset.delete(table)
  |> should.equal(True)
}

pub fn uset_multi_insert_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 1)
  uset.insert(table, [#(100, 200)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 200)))))
  uset.insert(table, [#(100, 300), #(100, 400)])
  |> should.equal(True)
  uset.lookup(table, 100)
  |> should.equal(Some(object.new(dynamic.from(#(100, 400)))))
  uset.delete(table)
  |> should.equal(True)
}

pub fn large_multitype_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 8, table.Public, 1)
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
  uset.delete(table)
  |> should.equal(True)
}

pub fn delete_test() {
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 1)
  uset.delete(table)
  |> should.equal(True)
  let assert Ok(table) =
    uset.new(atom.create_from_string("MyTable"), 2, table.Public, 1)
  uset.delete(table)
  |> should.equal(True)
  uset.delete(table)
  |> should.equal(False)
}
