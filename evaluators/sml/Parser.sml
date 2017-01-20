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
 *  o File name:   Parser.sml
 *  o Description: .
 *)


structure Parser :> PARSER = struct

(* shorten the same of structures we use *)
structure A  = Ast
structure R  = Reg
structure F  = Infix
structure PD = ParseDefs
structure LD = LexDefs
structure EH = ErrorHandler

(* currently used to hold information about syntax errors *)
type messages = (string * string * R.region list) list

(*exception ParsingError of int * messages*)

structure MLLrVals = MLLrValsFun(structure Token      = LrParser.Token)
structure MLLex    = MLLexFun   (structure Tokens     = MLLrVals.Tokens)
structure MLParser = JoinWithArg(structure ParserData = MLLrVals.ParserData
				 structure Lex        = MLLex
				 structure LrParser   = LrParser)

(* sets a val for holding information about syntax errors *)
val messages : messages ref = ref []

(* add and print functions for the messages list *)
fun addmessage msg = messages := (!messages) @ [msg]
fun printmessages _ = app (fn (x, _, _) => print (x ^ "\n")) (!messages)

fun dummyparsing messages = A.mk_term A.PROG_PROG "" [] []

(* returns a string reporting that there was an unexpected parsing error *)
fun failureParse file = "an unexpected parsing error occurred in " ^ file

fun parseStream file inputStream =
    let val error = ref false
	(* lexer argument: file name and start position *)
	val lexarg = (file, ref (1, 1))
	(* create a stream of lexical tokens *)
	val lexstream = MLParser.makeLexer
			    (fn n => TextIO.inputN(inputStream, n)) lexarg
	(* initial parsing error messages *)
	val _ = messages := []
	(* a function for reporting syntax errors *)
	fun syntaxError (msg, from, to) =
	    (fn extmsg => (error := true; addmessage extmsg))
		(file ^ ":"  ^ R.printPos from ^ "-" ^ R.printPos to ^ ": " ^  msg,
		 msg,
		 [R.consReg from to])
	(* build the AST, parameterized by its lowest node label *)
	val (astFunction, _) =
	    LD.handleLex MLParser.parse (15, lexstream, syntaxError, ())
	(* label the nodes starting from n, the second parameter is the typevar substitution *)
	val ast = astFunction ()
    in if !error
       then dummyparsing (!messages) (*raise ParsingError (n, (!messages))*)
       else ast
    end
    handle LD.LexError x            => (print "Lex errors\n";     dummyparsing [x])
	 | MLParser.ParseError      => (print "ParseError(ML)\n"; dummyparsing (!messages))
	 | PD.ParseError (msg, reg) => (print "ParseError(PD)\n"; dummyparsing [(msg, msg, reg)])
	 | _                        => (print "Unknown error\n";  dummyparsing [(failureParse file, failureParse file, [])])

fun parse file =
    let val instr = TextIO.openIn file
	val prog = parseStream file instr
	val _   = TextIO.closeIn instr
    in prog
    end
    handle IO.Io {name, function, cause} =>
	   let val message = "cannot access file: " ^ file
	   in print (message ^ "\n");
	      raise EH.DeadBranch message
	   end

end



