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
  Tweaks(Tweak)
}

pub type Tweak {
  WriteConcurrency(Bool)
  ReadConcurrency(Bool)
  DecentralizedCounters(Bool)
  Compressed
}

pub type Pid
