val do_primitive_test : string
  -> NuprlTerms.nuprl_term
    -> bool

val is_primitive_value : NuprlTerms.nuprl_term -> bool

val is_complete_primitive_value : NuprlTerms.nuprl_term -> bool

val compare_atomn : int
  -> NuprlTerms.nuprl_term
    -> NuprlTerms.nuprl_term
      -> bool
