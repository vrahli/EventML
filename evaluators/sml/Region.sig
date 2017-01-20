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
 *  o File name:   Region.sig
 *  o Description: .
 *)


signature REG = sig

    type pos    = int * int (* (line, column) *)
    type region = {from : pos, to : pos}
    (* this is useful to highlight a slice in a piece of code *)

    (* constructs a region from two pairs of integers (pos) *)
    val consReg         : pos -> pos -> region
    val getRegionList   : pos -> pos -> region list (* This is used by ML.grm and ExtReg.sml*)

    val infPos          : pos -> pos -> bool
    val strictInfPos    : pos -> pos -> bool
    val upPos           : pos -> pos
    val downPos         : pos -> pos

    val addString       : pos -> string -> pos

    val checkSameRegs   : region -> region -> bool
    val overlapReg      : region -> region -> bool
    val infReg          : region -> region -> bool
    val strictInfReg    : region -> region -> bool
    val inclReg         : region -> region -> bool (* true if the first region is included into the second one *)
    val inclRegList     : region list -> region -> bool (* true if all the regions in the list are included in the second argument *)
    val isVisList       : region list -> bool
    val areEqualRegs    : region list -> region list -> bool
    val isReg           : region -> bool

    val upReg           : region -> region
    val downReg         : region -> region
    val downRegRight    : region -> region
    val fusionReg       : region -> region -> region

    (* ACCESSORS *)
    val getFrom         : region -> pos
    val getTo           : region -> pos
    val getPosLine      : pos -> int
    val getPosCol       : pos -> int
    val getAllPos       : region -> int * int * int * int

    (* TABULATIONS *)
    val getTabSize      : unit -> int
    val setTabSize      : int  -> unit

    (* TOSTRING FUNCTIONS  *)
    val printPos        : pos -> string
    val printReg        : region -> string
    val printSmlReg     : region -> string
    val printLispReg    : region -> string
    val printRegList    : region list -> string

    (*val getRegsLine     : int -> region list -> region list * region list*)

end
