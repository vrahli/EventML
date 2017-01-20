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
 *  o File name:   Infix.sml
 *  o Description: .
 *)


structure Infix :> INFIX = struct

structure OrdStr : ORD_KEY = struct
type ord_key = string
val compare  = String.compare
end

structure R  = Reg
structure EH = ErrorHandler
structure MS = SplayMapFn(OrdStr)

type 'a pack = 'a * R.region list

type packstr = string pack

datatype 'a tree = L of 'a pack
		 | O of packstr
		 | N of packstr * 'a tree * 'a tree

datatype assoc = LEFT | RIGHT

type prec = int

type fixity = assoc * prec

type label = int ref

type operators = (label * fixity MS.map) list ref


(* For now, we don't associate associativity with the operators,
 * it depends on the precedence.
 * The label that goes with the id is the label corresponding to the
 * scope of the fixity declaration.  For example, if an id x is
 * declared an infix in a let expression labelled l, then we will
 * add (x, l) in one of the list. *)

val initOperators7 = [("div", LEFT,  7), ("mod", LEFT,  7), ("*", LEFT, 7), ("/", LEFT, 7)]
val initOperators6 = [("+",   LEFT,  6), ("-",   LEFT,  6), ("^", LEFT, 6)]
val initOperators5 = [("::",  RIGHT, 5), ("@",   RIGHT, 5)]
val initOperators4 = [("=",   LEFT,  4), ("<>",  LEFT,  4), (">", LEFT, 4), ("<", LEFT, 4), (">=", LEFT, 4), ("<=", LEFT, 4)]
val initOperators3 = [(":=",  LEFT,  3), ("o",   LEFT,  3)]
val initOperators0 = [("before", LEFT, 0)]

val initOperators = initOperators7 @
		    initOperators6 @
		    initOperators5 @
		    initOperators4 @
		    initOperators3 @
		    initOperators0

val initOperatorsMap = foldr (fn ((st, assoc, prec), operators) =>
				 MS.insert (operators, st, (assoc, prec)))
			     MS.empty
			     initOperators

val dummyLabel = ref 0
val label = ref 1
fun nextLabel () = let val x = !label in (label := x +1; x) end

val operators = ref [(dummyLabel, initOperatorsMap)]

fun reset () =
    (label := 1; operators := [(dummyLabel, initOperatorsMap)])

fun isInfix st = List.exists (fn (lab, map) => Option.isSome (MS.find (map, st))) (!operators)

(* We use foldl because we start with the tighter binding *)
fun getOperator st = List.foldl (fn ((lab, map), SOME x) => SOME x
				  | ((lab, map), NONE) => MS.find (map, st))
				NONE
				(!operators)

fun newScope lab = operators := (lab, MS.empty) :: (!operators)

fun rmScope lab =
    case !operators of
	[] => raise EH.DeadBranch "leaving an empty scope?!"
      | ((lab', map) :: operators') =>
	if !lab = !lab'
	then operators := operators'
	else raise EH.DeadBranch "leaving another scope?!"

fun addInfix st assoc prec =
    case !operators of
	[] => raise EH.DeadBranch "cannot add operator to empty scope"
      | ((lab, map) :: operators') =>
	operators := (lab, MS.insert (map, st, (assoc, prec))) :: operators'

fun addInfixL st prec = addInfix st LEFT  prec
fun addInfixR st prec = addInfix st RIGHT prec

fun rmInfix st =
    case !operators of
	[] => raise EH.DeadBranch "cannot remove operator from empty scope"
      | ((lab, map) :: operators') =>
	(operators := (lab, #1 (MS.remove (map, st))) :: operators')
	handle LibBase.NotFound => ()


(*(* convert a list of string to a list of leaves *)
fun preConvert xs = map (fn x => L x) xs*)

fun convertGen []     _ _ = raise EH.DeadBranch ""
  | convertGen [x]    _ _ = [x]
  | convertGen [_, _] _ _ = raise EH.DeadBranch ""
  | convertGen (x :: (z as O (str, r)) :: y :: xs) assoc prec =
    (case getOperator str of
	 NONE => raise EH.DeadBranch (str ^ " should be an infix operator")
       | SOME (assoc', prec') =>
	 if assoc = assoc' andalso prec = prec'
	 then let val node = case assoc of
				 LEFT  => N ((str, r), x, y)
			       | RIGHT => N ((str, r), y, x)
	      in convertGen (node :: xs) assoc prec
	      end
	 else x :: z :: (convertGen (y :: xs) assoc prec))
  | convertGen (x :: (N _) :: y :: xs) _ _ = raise EH.DeadBranch ""
  | convertGen (x :: (L _) :: y :: xs) _ _ = raise EH.DeadBranch ""

fun convertGen' tokens prec =
    let val tokens1 = convertGen tokens LEFT prec
	val tokens2 = rev (convertGen (rev tokens1) RIGHT prec)
    in tokens2
    end

(* pass converting the precedence 9 *)
fun convert9 xs = convertGen' xs 9

(* pass converting the precedence 8 *)
fun convert8 xs = convertGen' xs 8

(* pass converting the precedence 7 *)
fun convert7 xs = convertGen' xs 7

(* pass converting the precedence 6 *)
fun convert6 xs = convertGen' xs 6

(* pass converting the precedence 5 *)
fun convert5 xs = convertGen' xs 5

(* pass converting the precedence 4 *)
fun convert4 xs = convertGen' xs 4

(* pass converting the precedence 3 *)
fun convert3 xs = convertGen' xs 3

(* pass converting the precedence 2 *)
fun convert2 xs = convertGen' xs 2

(* pass converting the precedence 1 *)
fun convert1 xs = convertGen' xs 1

(* pass converting the precedence 0 *)
fun convert0 xs = convertGen' xs 0

fun convert' x = (convert0 o
		  convert1 o
		  convert2 o
		  convert3 o
		  convert4 o
		  convert5 o
		  convert6 o
		  convert7 o
		  convert8 o
		  convert9) x

fun convert xs =
    case convert' xs of
	[x] => x
      | _   => raise EH.DeadBranch ""


end
