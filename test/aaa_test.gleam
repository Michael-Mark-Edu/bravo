// import bravo
// import bravo/bag
// import gleam/erlang/process
// import gleam/io
// import gleam/otp/actor
// import gleam/otp/task
// import gleeunit/should
//
// pub type Message {
//   Start
//   Kill
// }
//
// fn defer(defer: fn() -> a, block: fn() -> b) -> b {
//   let b = block()
//   defer()
//   b
// }
//
// // This test must be the first test ran
// pub fn bravo_meta_survival_test() {
//   let assert Ok(actor) = {
//     use message, state <- actor.start(Error(Nil))
//     case message {
//       Start -> {
//         let assert Ok(table) = bag.new("first_table", bravo.Public)
//         let _ = bag.insert(table, "Hello", "World")
//         actor.continue(Ok(table))
//       }
//       Kill -> {
//         let assert Ok(table) = state
//         bag.delete(table)
//         actor.Stop(process.Normal)
//       }
//     }
//   }
//   let task = {
//     use <- task.async
//     actor.send(actor, Start)
//   }
//   task.await_forever(task)
//   let _task = {
//     use <- task.async
//     process.sleep(25)
//     actor.send(actor, Kill)
//   }
//   let assert Ok(table) = bag.new("bag0_5", bravo.Public)
//   use <- defer(fn() { bag.delete(table) |> should.equal(True) })
//   bag.insert(table, [#(100, 200), #(300, 500)])
//   |> should.equal(Ok(Nil))
//   process.sleep(50)
//   bag.insert(table, [#(200, 400)])
//   |> should.equal(Ok(Nil))
//   bag.lookup(table, 100)
//   |> should.equal(Ok([#(100, 200)]))
//   bag.lookup(table, 300)
//   |> should.equal(Ok([#(300, 500)]))
//   bag.lookup(table, 200)
//   |> should.equal(Ok([#(200, 400)]))
// }
