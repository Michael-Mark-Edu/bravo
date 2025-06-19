import gleam/erlang/process

pub fn run_async(f: fn() -> a) -> a {
  let subject = process.new_subject()
  let _pid =
    process.spawn(fn() {
      let result = f()
      process.send(subject, result)
    })
  let assert Ok(result) = process.receive(subject, 5000)
  result
}
