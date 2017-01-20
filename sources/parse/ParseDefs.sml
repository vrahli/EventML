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
 *  o File name:   ParseDefs.sml
 *  o Description: .
 *)


structure ParseDefs :> PARSEDEFS = struct

structure R = Reg

exception ParseError of string * R.region list

val file = ref ""

fun setFile f = file := f
fun getFile () = !file

type parsing_error = string * R.region list

val parsing_error : parsing_error option ref = ref NONE

fun set_parsing_error (msg, regs) = parsing_error := SOME (msg, regs)
fun get_parsing_error () = !parsing_error
fun reset_parsing_error () = parsing_error := NONE

end
