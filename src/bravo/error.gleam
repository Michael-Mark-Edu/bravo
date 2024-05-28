import bravo/etc.{type Term}

/// Represents any of Erlang's runtime errors. These errors are often vague and unclear.
///
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
