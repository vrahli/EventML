val is_eval_all : string -> bool

val num_principals    : string -> int
val num_principal_all : string -> int

val mk_ilibrary : NuprlTerms.nuprl_term
  -> NuprlTerms.nuprl_term
    -> NuprlTerms.nuprl_term

val get_found_user : NuprlTerms.nuprl_term
  -> NuprlTerms.nuprl_term

val strip_ilib : NuprlTerms.nuprl_term
  -> NuprlTerms.nuprl_term

val is_termof_term : NuprlTerms.lib * NuprlTerms.nuprl_term
  -> NuprlTerms.nuprl_term
    -> bool

val is_abstraction_term : NuprlTerms.lib * NuprlTerms.nuprl_term
  -> NuprlTerms.nuprl_term
    -> bool

val unfold_tof : NuprlTerms.nuprl_term
  -> NuprlTerms.nuprl_term

val unfold_abs : NuprlTerms.nuprl_term option
  -> NuprlTerms.nuprl_term
    -> NuprlTerms.nuprl_term

val ct_unfold_abs : NuprlTerms.env option
  -> NuprlTerms.nuprl_term
    -> NuprlTerms.nuprl_term
