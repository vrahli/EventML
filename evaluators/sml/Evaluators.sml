(* Copyright 2011 Cornell University
 * Copyright 2012 Cornell University
 * Copyright 2013 Cornell University
 *
 *
 * This file is part of EventML - a tool aiming at specifying
 * distributed protocols in an ML like language.  It is an interface
 * to the logic of events and is compiled into Nuprl.  It is written
 * by the NUPRL group of Cornell University, Ithaca, NY.
 *
 * EventML is a free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * EventML is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with EventML.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Cornell University, NUPRL group
 *  o Date:        20 May 2011
 *  o File name:   Evaluators.sml
 *  o Description: .
 *)


structure Evaluators :> EVALUATOR = struct

structure T    = NuprlTerms
structure P    = ParserNuprlAscii
structure EV1  = Evaluator1
structure EV2  = Evaluator2
structure EV3  = Evaluator3
structure EV4  = Evaluator4
structure EV5  = Evaluator5
structure EVE2 = EvaluatorEnv2
(*structure EVR2 = EvaluatorRef2*)

type eval_sig = int -> T.nuprl_term -> T.nuprl_term * int

val mapref = ref (T.emlib ())

val Evaluator1 = EV1.Evaluator1 (T.emlib ())

val eval1 = EV1.Evaluator1
val eval2 = EV2.Evaluator2
(*val evalR = EVR2.Evaluator2*)
val evalE = EVE2.Evaluator2
val eval3 = EV3.Evaluator3
val eval4 = EV4.Evaluator4

fun run_ev1 prt (SOME lib_file) steps term =
    let val light = false
	val split = false
	val terms = P.parse prt light [] lib_file split
	val map   = T.terms2map 4 terms
    in EV1.Evaluator1 map steps term
    end
  | run_ev1 prt NONE steps term =
    EV1.Evaluator1 (T.emlib ()) steps term

fun run_ev2b prt (SOME lib_file) steps term =
    let val light = false
	val split = false
	val terms = P.parse prt light [] lib_file split
	val map   = T.terms2map 5 terms
    in eval2 true true map steps term
    end
  | run_ev2b prt NONE steps term =
    eval2 true true (T.emlib ()) steps term

(* evaluator -- recursive descent *)
fun run_ev1_map steps term = EV1.Evaluator1 (!mapref) steps term

(* evaluator with closures *)
fun run_ev2_map steps term = eval2 true false (!mapref) steps term

(* evaluator with closures
 * + closures instead of subst when unfolding
 * + optimization: call-by-need instead of call-by-name *)
fun run_ev2b_map steps term = eval2 true true (!mapref) steps term

(* evaluator with closures
 * + closures instead of subst when unfolding
 * + regular call-by-name evaluation *)
fun run_ev2bn_map steps term = eval2 false true (!mapref) steps term

fun run_ev2e_map steps term = evalE true (!mapref) steps term

(* evaluator with closures
 * + closures instead of subst when unfolding
 * + CBV *)
fun run_ev2d_map steps term = eval2 true true (!mapref) steps term

(* evaluator with continuations *)
fun run_ev3_map steps term = EV3.Evaluator3 false (!mapref) steps term

(* evaluator with continuations
 * + closures instead of subst when unfolding *)
fun run_ev3b_map steps term = EV3.Evaluator3 true (!mapref) steps term

(* evaluator with continuations
 * + datatype for the different sub-eval functions *)
fun run_ev3c_map steps term = EV3.EvaluatorD3 false (!mapref) steps term

(* evaluator with continuations
 * + closures instead of subst when unfolding
 * + datatype for the different sub-eval functions *)
fun run_ev3d_map steps term = EV3.EvaluatorD3 true (!mapref) steps term

(* evaluator with defunctionalization *)
fun run_ev4_map steps term = EV4.Evaluator4 false (!mapref) steps term

(* evaluator with defunctionalization
 * + closures instead of subst when unfolding *)
fun run_ev4b_map steps term = EV4.Evaluator4 true (!mapref) steps term

(* evaluator -- state machine *)
fun run_ev5_map steps term = EV5.Evaluator5 false (!mapref) steps term

(* evaluator -- state machine
 * + closures instead of subst when unfolding *)
fun run_ev5b_map steps term = EV5.Evaluator5 true (!mapref) steps term

fun start_session prt (SOME lib) =
    let val light = false
	val split = false
	val terms = P.parse prt light [] lib split
	val map   = T.terms2map 6 terms
	val _     = mapref := map
    in ()
    end
  | start_session prt NONE = ()

fun start_session_lib lib = mapref := lib

fun add_to_session terms =
    let val map =
	    foldl (fn (term, map) =>
		      if T.is_nuprl_iabstraction_term term
		      then let val (_, rlhs, rrhs) = T.dest_iabstraction term
			       val lhs  = T.rterm2term rlhs
			       val sign = T.getSignature lhs (*([], [])*)
			       val id   = T.opid_of_term lhs
			       val obid = ""
			       val opid = id
			       (*val _    = print ("[adding to library: " ^ id ^ "]\n")*)
			       val item = T.mk_item id sign obid rlhs rrhs []
			   in T.insert_abs map opid item
			   end
		      else map)
		  (!mapref)
		  terms
    in mapref := map
    end

fun get_lib () = !mapref

fun reset_lib () = mapref := T.emlib ()

val end_session = reset_lib

end
