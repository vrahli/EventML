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
 *  o File name:   Region.sml
 *  o Description: .
 *)


structure Reg :> REG = struct

structure EH = ErrorHandler

(* pos is represented by a line number and a character number *)
type pos    = int * int

(* a region is FROM a line number + character TO a line number + character *)
type region = {from : pos, to : pos}

val tabBool   = ref false
val tabSize   = ref 8
fun getTab () =
    if !tabSize < 0
    then ""
    else let val s = ref ""
	     val n = ref 0
	 in while !n < !tabSize do (s := !s ^ " "; n := !n + 1); !s
	 end

fun getTabSize () = !tabSize
fun setTabSize ts = (tabBool := true; tabSize := ts)

fun addString (l, c) s =
    let val s' = if !tabBool
		 then String.translate
			  (fn #"\t" => getTab ()
			    | x     => Char.toString x)
			  s
		 else s
    in (l, c + String.size s') end

(* constructs a region givin two pairs of integers *)
fun consReg  p1 p2 = {from = p1, to = p2}

fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(* support for printing a position *)
fun printPos (a, b) = Int.toString a ^ "." ^ Int.toString b

(* prints the position as a pair *)
fun printSmlPos (a, b) = "(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")"

(* support for printing a region. from and to are of type pos, so they are each a pair of integers *)
fun printReg {from, to} = "(" ^ printPos from ^ "," ^ printPos to ^ ")"

(* prints a region as a record (as it is represented internally) *)
fun printSmlReg {from, to} =
    "{from=" ^ printSmlPos from ^
    ",to="   ^ printSmlPos to   ^
    "}"

fun printLispReg {from = (x1, y1), to = (x2, y2)} =
    "(" ^ Int.toString x2 ^ " " ^ Int.toString y1 ^
    " " ^ Int.toString x2 ^ " " ^ Int.toString y2 ^
    ")"

fun printRegList xs = printlistgen xs printReg

(* verify that two regions are the same *)
fun checkSameRegs ({from = (l1, c1), to = (l2, c2)} : region)
		  ({from = (l3, c3), to = (l4, c4)} : region) =
    l1 = l3 andalso c1 = c3 andalso l2 = l4 andalso c2 = c4

(* checks that the (l1, c1) region comes before (l2, c2) *)
fun infPos (line1, column1) (line2, column2) =
    line1 < line2 orelse (line1 = line2 andalso column1 <= column2)

(* same as infPos, though in this case region 2 may not start at the end of region 1 *)
fun strictInfPos (line1, column1) (line2, column2) =
    line1 < line2 orelse (line1 = line2 andalso column1 < column2)

(* increments the column number by one *)
fun upPos   (line, column) = (line, column + 1)
fun downPos (line, column) = (line, column - 1)

(* Generates regions for blank spaces that may contain empty lines. *)
(* For now we also have some uses of it for labexp and labpat but it should disapear. *)
fun getRegionList (left as (l1, c1)) (right as (l2, c2)) =
    let fun endlines n c =
	    if l2 < n
	    then raise EH.DeadBranch ""
	    else if n = l2
	    then [consReg (l2, 1) (downPos right)]
	    else let val newpos = upPos (n, c)
		     val regs = endlines (n + 1) 0
		 in (consReg newpos newpos) :: regs
		 end
    in if l1 < l2
       then endlines l1 c1
       else [consReg (upPos left) (downPos right)]
    end

(* checks for two overlapping regions *)
fun overlapReg {from = p1, to = p2} {from = p1', to = p2'} =
    (strictInfPos p1 p1' andalso infPos p1' p2 andalso strictInfPos p2 p2')
    orelse
    (strictInfPos p1' p1 andalso infPos p1 p2' andalso strictInfPos p2' p2)

(* combines two regions into one region *)
fun fusionReg {from = p1, to = p2} {from = p1', to = p2'} =
    if infPos p1 p1'
    then if infPos p2 p2'
	 then {from = p1, to = p2'}
	 else {from = p1, to = p2}
    else if infPos p2 p2'
    then {from = p1', to = p2'}
    else {from = p1', to = p2}

(* checks if two regions intersect *)
fun infReg {from = p1, to = p2} {from = p3, to = p4} =
    infPos p2 p3

(* checks if two regions intersect, and one does not immediately follow the other *)
fun strictInfReg {from = p1, to = p2} {from = p3, to = p4} =
    strictInfPos p2 p3

fun inclReg {from = p1, to = p2} {from = p3, to = p4} =
    infPos p3 p1 andalso infPos p2 p4

fun inclRegList [] _ = true
  | inclRegList (r' :: rs) r = inclReg r' r andalso inclRegList rs r

fun upReg        {from = p1, to = p2} = {from = p1,         to = upPos p2}
fun downReg      {from = p1, to = p2} = {from = downPos p1, to = p2}
fun downRegRight {from = p1, to = p2} = {from = p1,         to = downPos p2}

(* grabs where the region starts or where the region ends, respectively *)
fun getFrom {from, to} = from
fun getTo   {from, to} = to

(* returns the line number or the column number of the pair, respectively *)
fun getPosLine (line, _) = line
fun getPosCol  (_, column) = column

fun getAllPos {from = (l1, c1), to = (l2, c2)} = (l1, c1, l2, c2)

(* checks that r is a feasible region (that from <= to *)
fun isReg r = infPos (getFrom r) (getTo r)
fun isVisList rs = case rs of [] => false | [r] => isReg r | _ => true

(* returns a list of all regions that are in a specified line *)
fun getRegsLine _ [] = ([], [])
  | getRegsLine (line : int) (r :: rs) =
    let
	val (rs1, rs2) = getRegsLine line rs
    in if line = getPosLine (getFrom r)
       then (r :: rs1, rs2)
       else (rs1, r :: rs2)
    end

(* removes a region from a list of regions *)
fun removeReg r [] = (false, [])
  | removeReg r (r' :: rs) =
    if checkSameRegs r r'
    then (true, rs)
    else (fn (b, rs') => (b, r :: rs')) (removeReg r rs)

fun areEqualRegs [] [] = true
  | areEqualRegs [] _  = false
  | areEqualRegs _  [] = false
  | areEqualRegs (r :: rs) xs =
    let
	val (b, xs') = removeReg r xs
    in b andalso areEqualRegs rs xs'
    end

end
