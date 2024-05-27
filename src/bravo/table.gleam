import gleam/erlang/atom.{type Atom}

pub type Set {
  Set(table: Atom, size: Int, keypos: Int, stringed: Bool)
}

pub type OrderedSet {
  OrderedSet(Atom, Int, Int, Bool)
}

pub type Bag {
  Bag(Atom, Int, Int, Bool)
}

pub type DuplicateBag {
  DuplicateBag(Atom, Int, Int, Bool)
}

pub type Access {
  Public
  Protected
  Private
}
