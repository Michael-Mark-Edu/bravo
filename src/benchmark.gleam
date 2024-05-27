import bravo/object
import bravo/set
import bravo/table
import gleam/dict
import gleam/erlang/atom
import gleam/list
import glychee/benchmark
import glychee/configuration

pub fn main() {
  // Configuration is optional
  configuration.initialize()
  configuration.set_pair(configuration.Warmup, 2)
  // configuration.set_pair(configuration.Parallel, 10)

  // Run the benchmarks
  benchmark.run(
    [
      benchmark.Function(
        label: "set.insert() & set.lookup()",
        callable: fn(test_data) {
          fn() {
            let assert Ok(table) = test_data
            let _ =
              list.map(list.range(0, 1_000_000), fn(a) {
                set.insert(table, [object.O2(a, a)])
              })
          }
        },
        // let _ =
      //   list.map(list.range(0, 2_000_000), fn(a) { set.lookup(table, a) })
      ),
    ],
    [
      benchmark.Data(
        label: "set",
        data: set.new(
          atom.create_from_string("Table1"),
          2,
          table.Public,
          1,
          False,
        ),
      ),
    ],
  )
  benchmark.run(
    [
      benchmark.Function(
        label: "dict.insert() & dict.get()",
        callable: fn(test_data) {
          fn() {
            let _ =
              list.map(list.range(0, 1_000_000), fn(a) {
                dict.insert(test_data, a, a)
              })
          }
        },
        // let _ =
      //   list.map(list.range(0, 2_000_000), fn(a) {
      //     dict.get(test_data, a)
      //   })
      ),
    ],
    [benchmark.Data(label: "dict", data: dict.new())],
  )
}
