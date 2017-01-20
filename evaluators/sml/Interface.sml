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
 *  o File name:   Interface.sml
 *  o Description: .
 *)


structure Interface :> INTERFACE = struct

structure E = Evaluators
structure P = Parser
structure A = Ast

val test = E.test

(* Extracts the evaluator n form Evaluators.sml
 * The evaluator is extracted in evaln.sml. *)
fun extractEval n =
    let val nst     = Int.toString n
	val fileout = "eval" ^ nst ^ ".sml"
	val evfile  = "Evaluator" ^ nst ^ ".sml"
	val stin    = TextIO.openIn evfile
	val stout   = TextIO.openOut fileout
	fun extract (b1, b2) =
	    (* If b1 then we're in grabbing text mode of the first function.
	     * If b2 too then we're in grabbing mode of the second function. *)
	    case TextIO.inputLine stin of
		NONE => ()
	      | SOME line =>
		if b1
		then if b2
		     then if String.isPrefix "    end" line
			  then (TextIO.output (stout, line); ())
			  else (TextIO.output (stout, line); extract (true, true))
		     else if String.isPrefix "fun Evaluator1" line
		     then (TextIO.output (stout, line); extract (true, true))
		     else (TextIO.output (stout, line); extract (true, false))
		else if String.isPrefix "fun Evaluator1'" line
		then (TextIO.output (stout, line); extract (true, false))
		else extract (false, false)
	val _ = extract (false, false)
	val _ = TextIO.closeOut stout
	val _ = TextIO.closeIn stin
	val _ = print ("**** Evaluator " ^ nst ^ " has been extracted to " ^ fileout ^ "\n")
    in ()
    end

fun toPython input =
    let val file   =
	    if String.isSuffix ".sml" input
	    then String.substring (input, 0, String.size input - 4)
	    else raise Fail "The input should be a sml file"
	val output = file ^ ".py"
	val term   = P.parse input
	val st     = A.exportToPython term
	val stout  = TextIO.openOut output
	val _      = TextIO.output (stout, st)
	val _      = TextIO.closeOut stout
    in ()
    end

(* This function transform the evaluator in input to a new evaluator which
 * gets copied in output, all that using the set of transformation rules
 * corresponding to the transform function. *)
fun transformGen transform input output =
    let val t1    = P.parse input
	val t2    = transform t1
	val st    = A.export t2
	val stout = TextIO.openOut output
	val _     = TextIO.output (stout, st)
	val _     = TextIO.closeOut stout
    in ()
    end

fun transform 1 = transformGen A.transform1
  | transform 2 = transformGen A.transform2
  | transform _ = fn _ => fn _ => print "This transformation has not been implemented yet\n"

fun transform' n file =
    transform n
	      (file ^ Int.toString n ^ ".sml")
	      (file ^ Int.toString (n + 1) ^ ".sml")

end
