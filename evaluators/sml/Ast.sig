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
 *  o File name:   Ast.sig
 *  o Description: .
 *)


signature AST = sig

    type kind

    datatype term = N of {kind     : kind,
			  value    : string,
			  regions  : Reg.region list,
			  children : term list}

    datatype class_prod =
	     (* SCON *)
	     SCON_INT
	   | SCON_WORD
	   | SCON_REAL
	   | SCON_STRING
	   | SCON_CHAR
	   (* ID *)
	   | ID_VID
	   | ID_LONGID
	   | ID_TYCON
	   (* CONBIND *)
	   | CONBIND_ID
	   | CONBIND_OP
	   | CONBIND_OF
	   | CONBIND_OPOF
	   (* CONBINDSEQ *)
	   | CONBINDSEQ_SEQ
	   (* TYLAB *)
	   | TYLAB_LAB
	   (* TYPEVAR *)
	   | TYPEVAR_VAR
	   (* TYPEVARSEQ *)
	   | TYPEVARSEQ_ONE
	   | TYPEVARSEQ_SEQ
	   | TYPEVARSEQ_EM
	   (* TYPEROW *)
	   | TYPEROW_ROW
	   (* TYPE *)
	   | TYPE_ARROW
	   | TYPE_TUPLE
	   | TYPE_TYCON
	   | TYPE_VAR
	   | TYPE_RECORD
	   | TYPE_PAREN
	   (* ATPAT *)
	   | ATPAT_ID
	   | ATPAT_WILD
	   | ATPAT_SCON
	   | ATPAT_RECORD
	   | ATPAT_LIST
	   | ATPAT_PAREN
	   | ATPAT_TUPLE
	   (* IDENTTY *)
	   | IDENTTY_ID
	   | IDENTTY_TY
	   (* PATROW *)
	   | PATROW_EQ
	   | PATROW_ID
	   | PATROW_AS
	   | PATROW_WILD
	   (* PAT *)
	   | PAT_AS
	   | PAT_TYPE
	   | PAT_OP
	   | PAT_ATPAT
	   | PAT_APP
	   (* EXP *)
	   | EXP_OR
	   | EXP_AND
	   | EXP_TYPE
	   | EXP_HANDLE
	   | EXP_FN
	   | EXP_CASE
	   | EXP_ITE
	   | EXP_WHILE
	   | EXP_RAISE
	   | EXP_ATEXP
	   | EXP_APP
	   | EXP_OP
	   (* MATCH *)
	   | MATCH_M
	   (* MRULE *)
	   | MRULE_M
	   (* ATEXP *)
	   | ATEXP_ID
	   | ATEXP_SCON
	   | ATEXP_SEL
	   | ATEXP_TUPLE
	   | ATEXP_SEQ
	   | ATEXP_PAREN
	   | ATEXP_RECORD
	   | ATEXP_LET
	   | ATEXP_SLET
	   | ATEXP_LIST
	   (* EXPROW *)
	   | EXPROW_ROW
	   (* EXPSEQ *)
	   | EXPSEQ_SEQ
	   (* DECS *)
	   | DECS_DECS
	   (* DEC *)
	   | DEC_VAL
	   | DEC_DAT
	   | DEC_DATWITH
	   | DEC_DATREP
	   | DEC_ABSTYP
	   | DEC_ABSWITH
	   | DEC_FVAL
	   | DEC_TYP
	   | DEC_EXC
	   | DEC_LOCAL
	   (* EXBIND *)
	   | EXBIND_BIND
	   (* ONEEXBIND *)
	   | ONEEXBIND_ID
	   | ONEEXBIND_OF
	   | ONEEXBIND_EQ
	   (* TYPBIND *)
	   | TYPBIND_BIND
	   (* ONETYPBIND *)
	   | ONETYPBIND_EQ
	   (* FVALBIND *)
	   | FVALBIND_BIND
	   (* ONEFVALBIND *)
	   | ONEFVALBIND_BRANCHES
	   (* BRANCHFVALBIND *)
	   | BRANCHFVALBIND_BRANCH
	   (* FMATCHTY *)
	   | FMATCHTY_FM
	   | FMATCHTY_TY
	   (* FMATCH *)
	   | FMATCH_ID
	   | FMATCH_APP
	   (* TYPESEQ *)
	   | TYPESEQ_ONE
	   | TYPESEQ_SEQ
	   | TYPESEQ_EM
	   (* VALBIND *)
	   | VALBIND_REC
	   | VALBIND_NREC
	   (* ONEVALBIND *)
	   | ONEVALBIND_EQ
	   (* DATNAME *)
	   | DATNAME_DAT
	   (* DATBIND *)
	   | DATBIND_BIND
	   (* ONEDATBIND *)
	   | ONEDATBIND_EQ
	   (* FATPAT *)
	   | FATPAT_PAT
	   (* PROG *)
	   | PROG_PROG

    (* Used by the parser *)
    val mk_term : class_prod      ->
		  string          ->
		  Reg.region list ->
		  term list       ->
		  term

    val export : term -> string

    (* Used by the parser *)
    val getIDinFATPAT : term -> term

    val exportToPython : term -> string

    val transform1 : term -> term
    val transform2 : term -> term

end
