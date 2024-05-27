import gleam/erlang/atom.{type Atom}

pub type USet {
  USet(table: Atom, size: Int, keypos: Int)
}

pub type OSet {
  OSet(Atom, Int, Int, Bool)
}

pub type Bag {
  Bag(Atom, Int, Int, Bool)
}

pub type DBag {
  DBag(Atom, Int, Int, Bool)
}

pub type Access {
  Public
  Protected
  Private
}
