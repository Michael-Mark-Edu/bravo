import bravo/etc.{type Term}

pub type ErlangError {
  Badarg
  Badarith
  Badmatch(Term)
  FunctionClause
  CaseClause(Term)
  IfClause
  TryClause(Term)
  Undef
  Badfun(Term)
  Badarity(#(Term, Term))
  TimeoutValue
  Noproc
  Noconnection
  Nocatch(Term)
  SystemLimit
}
