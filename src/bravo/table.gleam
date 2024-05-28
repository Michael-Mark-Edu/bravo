import gleam/erlang/atom.{type Atom}

pub type USet {
  USet(table: Atom, keypos: Int)
}

pub type OSet {
  OSet(Atom, Int, Bool)
}

pub type Bag {
  Bag(Atom, Int, Bool)
}

pub type DBag {
  DBag(Atom, Int, Bool)
}

pub type Access {
  Public
  Protected
  Private
}
