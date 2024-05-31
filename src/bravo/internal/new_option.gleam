import bravo/etc.{type Term}

pub type NewOption {
  Set
  OrderedSet
  Bag
  DuplicateBag
  Public
  Protected
  Private
  NamedTable
  Keypos(Int)
  Heir(Pid, Term)
  WriteConcurrency(Auto)
  ReadConcurrency(Bool)
  DecentralizedCounters(Bool)
  Compressed
}

pub type Pid

pub type Auto {
  Auto
}

pub type EmptyResult(error) {
  Ok
  Error(error)
}
