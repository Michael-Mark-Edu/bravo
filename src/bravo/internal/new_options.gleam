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
  WriteConcurrency(WriteConcurrencyInternal)
  ReadConcurrency(Bool)
  DecentralizedCounters(Bool)
  Compressed
}

pub type WriteConcurrencyInternal {
  True
  False
  Auto
}
