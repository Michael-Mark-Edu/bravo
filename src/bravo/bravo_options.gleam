import bravo/internal/write_concurrency_internal

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
  WriteConcurrency(write_concurrency_internal.WriteConcurrencyInternal)
  ReadConcurrency(Bool)
  DecentralizedCounters(Bool)
  Compressed
}

pub type WriteConcurrency {
  On
  Off
  Auto
}
