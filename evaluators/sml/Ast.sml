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
 *  o File name:   Ast.sml
 *  o Description: .
 *)


structure Ast :> AST = struct

structure R  = Reg
structure EH = ErrorHandler

type class = int
type prod  = int
type kind  = class * prod

datatype term = N of {kind     : kind,
		      value    : string,
		      regions  : R.region list,
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

(*type mem_term = term option ref

val mem_term : mem_term = ref NONE

fun setTerm term = mem_term := SOME term
fun getTerm () = !mem_term
fun unsetTerm () = mem_term := NONE*)

(* SCON *)
val assoc_kind = [(SCON_INT,               (0,  0)),
		  (SCON_WORD,              (0,  1)),
		  (SCON_REAL,              (0,  2)),
		  (SCON_STRING,            (0,  3)),
		  (SCON_CHAR,              (0,  4)),
		  (* ID *)
		  (ID_VID,                 (1,  0)),
		  (ID_LONGID,              (1,  1)),
		  (ID_TYCON,               (1,  2)),
		  (* CONBIND *) (* ----- 2? *)
		  (CONBIND_ID,             (3,  0)),
		  (CONBIND_OP,             (3,  1)),
		  (CONBIND_OF,             (3,  2)),
		  (CONBIND_OPOF,           (3,  3)),
		  (* CONBINDSEQ *)
		  (CONBINDSEQ_SEQ,         (4,  0)),
		  (* TYLAB, *)
		  (TYLAB_LAB,              (5,  0)),
		  (* TYPEVAR *)
		  (TYPEVAR_VAR,            (6,  0)),
		  (* TYPEVARSEQ *)
		  (TYPEVARSEQ_ONE,         (7,  0)),
		  (TYPEVARSEQ_SEQ,         (7,  1)),
		  (TYPEVARSEQ_EM,          (7,  2)),
		  (* TYPEROW *)
		  (TYPEROW_ROW,            (8,  0)),
		  (* TYPE *)
		  (TYPE_ARROW,             (9,  0)),
		  (TYPE_TUPLE,             (9,  1)),
		  (TYPE_TYCON,             (9,  2)),
		  (TYPE_VAR,               (9,  3)),
		  (TYPE_RECORD,            (9,  4)),
		  (TYPE_PAREN,             (9,  5)),
		  (* ATPAT *)
		  (ATPAT_ID,               (10, 0)),
		  (ATPAT_WILD,             (10, 1)),
		  (ATPAT_SCON,             (10, 2)),
		  (ATPAT_RECORD,           (10, 3)),
		  (ATPAT_LIST,             (10, 4)),
		  (ATPAT_PAREN,            (10, 5)),
		  (ATPAT_TUPLE,            (10, 6)),
		  (* IDENTTY *)
		  (IDENTTY_ID,             (11, 0)),
		  (IDENTTY_TY,             (11, 1)),
		  (* PATROW *)
		  (PATROW_EQ,              (12, 0)),
		  (PATROW_ID,              (12, 1)),
		  (PATROW_AS,              (12, 2)),
		  (PATROW_WILD,            (12, 3)),
		  (* PAT *)
		  (PAT_AS,                 (13, 0)),
		  (PAT_TYPE,               (13, 1)),
		  (PAT_OP,                 (13, 2)),
		  (PAT_ATPAT,              (13, 3)),
		  (PAT_APP,                (13, 4)),
		  (* EXP *)
		  (EXP_OR,                 (14, 0)),
		  (EXP_AND,                (14, 1)),
		  (EXP_TYPE,               (14, 2)),
		  (EXP_HANDLE,             (14, 3)),
		  (EXP_FN,                 (14, 4)),
		  (EXP_CASE,               (14, 5)),
		  (EXP_ITE,                (14, 6)),
		  (EXP_WHILE,              (14, 7)),
		  (EXP_RAISE,              (14, 8)),
		  (EXP_ATEXP,              (14, 9)),
		  (EXP_APP,                (14, 10)),
		  (EXP_OP,                 (14, 11)),
		  (* MATCH *)
		  (MATCH_M,                (15, 0)),
		  (* MRULE *)
		  (MRULE_M,                (16, 0)),
		  (* ATEXP *)  (* ---- 17? *)
		  (ATEXP_ID,               (18, 0)),
		  (ATEXP_SCON,             (18, 1)),
		  (ATEXP_SEL,              (18, 2)),
		  (ATEXP_TUPLE,            (18, 3)),
		  (ATEXP_SEQ,              (18, 4)),
		  (ATEXP_PAREN,            (18, 5)),
		  (ATEXP_RECORD,           (18, 6)),
		  (ATEXP_LET,              (18, 7)),
		  (ATEXP_SLET,             (18, 8)),
		  (ATEXP_LIST,             (18, 9)),
		  (* EXPROW *)
		  (EXPROW_ROW,             (19, 0)),
		  (* EXPSEQ *)
		  (EXPSEQ_SEQ,             (20, 0)),
		  (* DECS *)
		  (DECS_DECS,              (21, 0)),
		  (* DEC *)
		  (DEC_VAL,                (22, 0)),
		  (DEC_DAT,                (22, 1)),
		  (DEC_DATWITH,            (22, 2)),
		  (DEC_DATREP,             (22, 3)),
		  (DEC_ABSTYP,             (22, 4)),
		  (DEC_ABSWITH,            (22, 5)),
		  (DEC_FVAL,               (22, 6)),
		  (DEC_TYP,                (22, 7)),
		  (DEC_EXC,                (22, 8)),
		  (DEC_LOCAL,              (22, 9)),
		  (* EXBIND *)
		  (EXBIND_BIND,            (23, 0)),
		  (* ONEEXBIND *)
		  (ONEEXBIND_ID,           (24, 0)),
		  (ONEEXBIND_OF,           (24, 1)),
		  (ONEEXBIND_EQ,           (24, 2)),
		  (* TYPBIND *)
		  (TYPBIND_BIND,           (25, 0)),
		  (* ONETYPBIND *)
		  (ONETYPBIND_EQ,          (26, 0)),
		  (* FVALBIND *)
		  (FVALBIND_BIND,          (27, 0)),
		  (* ONEFVALBIND *)
		  (ONEFVALBIND_BRANCHES,   (28, 0)),
		  (* BRANCHFVALBIND *)
		  (BRANCHFVALBIND_BRANCH,  (29, 0)),
		  (* FMATCHTY *)
		  (FMATCHTY_FM,            (30, 0)),
		  (FMATCHTY_TY,            (30, 1)),
		  (* FMATCH *)
		  (FMATCH_ID,              (31, 0)),
		  (FMATCH_APP,             (31, 1)),
		  (* TYPESEQ *)
		  (TYPESEQ_ONE,            (32, 0)),
		  (TYPESEQ_SEQ,            (32, 1)),
		  (TYPESEQ_EM,             (32, 2)),
		  (* VALBIND *)
		  (VALBIND_REC,            (33, 0)),
		  (VALBIND_NREC,           (33, 1)),
		  (* ONEVALBIND *) (* ----- 34? *)
		  (ONEVALBIND_EQ,          (35, 0)),
		  (* DATNAME *)
		  (DATNAME_DAT,            (36, 0)),
		  (* DATBIND *)
		  (DATBIND_BIND,           (37, 0)),
		  (* ONEDATBIND *)
		  (ONEDATBIND_EQ,          (38, 0)),
		  (* FATPAT *)
		  (FATPAT_PAT,             (39, 0)),
		  (* PROG *)
		  (PROG_PROG,              (40, 0))]

fun getKind     (N {kind, value, regions, children}) = kind
fun getValue    (N {kind, value, regions, children}) = value
fun getRegions  (N {kind, value, regions, children}) = regions
fun getChildren (N {kind, value, regions, children}) = children

fun kindIsExp (14, _) = true
  | kindIsExp _ = false

fun kindIsAtExp (18, _) = true
  | kindIsAtExp _ = false

fun toKind tok =
    case List.find (fn (t, _) => t = tok) assoc_kind of
	SOME (_, kind) => kind
      | NONE => let val msg = "unknown token" in print msg; raise EH.DeadBranch msg end

fun toToken kind =
    case List.find (fn (_, k) => k = kind) assoc_kind of
	SOME (tok, _) => tok
      | NONE => let val msg = "unknown kind" in print msg; raise EH.DeadBranch msg end

fun toTokenTerm term = toToken (getKind term)

fun mk_term class_prod value regs children =
    N {kind     = toKind class_prod,
       value    = value,
       regions  = regs,
       children = children}

fun toString_class class = Int.toString class
fun toString_prod  prod  = Int.toString prod

fun toString_kind (class, prod) =
    "(" ^ toString_class class ^ "," ^ toString_prod prod ^ ")"

fun toString_regions regions = R.printRegList regions

fun toString_value value = value

fun toString_term (N {kind, value, regions, children}) =
    "N{kind="  ^ toString_kind     kind     ^ "," ^
    "value="   ^ toString_value    value    ^ "," ^
    "regions"  ^ toString_regions  regions  ^ "," ^
    "children" ^ toString_children children ^ "}"
and toString_children children =
    #1 (foldl (fn (term, (st, sep)) => (st ^ sep ^ toString_term term, ","))
	      ("[", "")
	      children) ^ "]"

val toString = toString_term

fun print_term term = print (toString term)

val sepnl = "\n"

(* SCON *)
fun export (N {kind = (0,  0),  value, regions, children = []}) = value
  | export (N {kind = (0,  1),  value, regions, children = []}) = value
  | export (N {kind = (0,  2),  value, regions, children = []}) = value
  | export (N {kind = (0,  3),  value, regions, children = []}) = value
  | export (N {kind = (0,  4),  value, regions, children = []}) = value
  (* ID *)
  | export (N {kind = (1,  0),  value, regions, children = []}) = value
  | export (N {kind = (1,  1),  value, regions, children = []}) = value
  | export (N {kind = (1,  2),  value, regions, children = []}) = value
  (* CONBIND *)
  | export (N {kind = (3,  0),  value, regions, children = [id]}) = export id
  | export (N {kind = (3,  1),  value, regions, children = [id]}) = "OP " ^ export id
  | export (N {kind = (3,  2),  value, regions, children = [id, ty]}) = export id ^ " OF " ^ export ty
  | export (N {kind = (3,  3),  value, regions, children = [id, ty]}) = "OP " ^ export id ^ " OF " ^ export ty
  (* CONBINDSEQ *)
  | export (N {kind = (4,  0),  value, regions, children}) = exportList children " | "
  (* TYLAB *)
  | export (N {kind = (5,  0),  value, regions, children = []}) = value
  (* TYPEVAR *)
  | export (N {kind = (6,  0),  value, regions, children = []}) = value
  (* TYPEVARSEQ *)
  | export (N {kind = (7,  0),  value, regions, children = [tv]}) = export tv
  | export (N {kind = (7,  1),  value, regions, children}) = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (7,  2),  value, regions, children}) = ""
  (* TYPEROW *)
  | export (N {kind = (8,  0),  value, regions, children = [lab, ty]}) = export lab ^ " : " ^ export ty
  (* TYPE *)
  | export (N {kind = (9,  0),  value, regions, children = [ty1, ty2]}) = export ty1 ^ " -> " ^ export ty2
  | export (N {kind = (9,  1),  value, regions, children}) = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (9,  2),  value, regions, children = [tn, ts]}) = export ts ^ " " ^ export tn
  | export (N {kind = (9,  3),  value, regions, children = [tv]}) = export tv
  | export (N {kind = (9,  4),  value, regions, children}) = "{" ^ exportList children ", " ^ "}"
  | export (N {kind = (9,  5),  value, regions, children = [ty]}) = "(" ^ export ty ^ ")"
  (* ATPAT *)
  | export (N {kind = (10, 0),  value, regions, children = [id]}) = export id
  | export (N {kind = (10, 1),  value, regions, children = []}) = "_"
  | export (N {kind = (10, 2),  value, regions, children = [sc]}) = export sc
  | export (N {kind = (10, 3),  value, regions, children}) = "{" ^ exportList children ", " ^ "}"
  | export (N {kind = (10, 4),  value, regions, children}) = "[" ^ exportList children ", " ^ "]"
  | export (N {kind = (10, 5),  value, regions, children = [x]}) = "(" ^ export x ^ ")"
  | export (N {kind = (10, 6),  value, regions, children}) = "(" ^ exportList children ", " ^ ")"
  (* IDENTTY *)
  | export (N {kind = (11, 0),  value, regions, children = [id]}) = export id
  | export (N {kind = (11, 1),  value, regions, children = [id, ty]}) = export id ^ " : " ^ export ty
  (* PATROW *)
  | export (N {kind = (12, 0),  value, regions, children = [lab, pat]}) = export lab ^ " = " ^ export pat
  | export (N {kind = (12, 1),  value, regions, children = [id]}) = export id
  | export (N {kind = (12, 2),  value, regions, children = [id, pat]}) = export id ^ " as " ^ export pat
  | export (N {kind = (12, 3),  value, regions, children = []}) = "..."
  (* PAT *)
  | export (N {kind = (13, 0),  value, regions, children = [pat1, pat2]}) = export pat1 ^ " as " ^ export pat2
  | export (N {kind = (13, 1),  value, regions, children = [pat, ty]}) = export pat ^ " : " ^ export ty
  | export (N {kind = (13, 2),  value, regions, children = [pat1, pat2]}) = export pat1 ^ " " ^ value ^ " " ^ export pat2
  | export (N {kind = (13, 3),  value, regions, children = [x]}) = export x
  | export (N {kind = (13, 4),  value, regions, children = [f, x]}) = export f ^ " " ^ export x
  (* EXP *)
  | export (N {kind = (14, 0),  value, regions, children = [e1, e2]}) = export e1 ^ " orelse " ^ export e2
  | export (N {kind = (14, 1),  value, regions, children = [e1, e2]}) = export e1 ^ " andalso " ^ export e2
  | export (N {kind = (14, 2),  value, regions, children = [e, t]}) = export e ^ " : " ^ export t
  | export (N {kind = (14, 3),  value, regions, children = [e, m]}) = export e ^ " handle " ^ export m
  | export (N {kind = (14, 4),  value, regions, children = [m]}) = "fn " ^ export m
  | export (N {kind = (14, 5),  value, regions, children = [e, m]}) = "case " ^ export e ^ " of " ^ export m
  | export (N {kind = (14, 6),  value, regions, children = [e1, e2, e3]}) = "if " ^ export e1 ^ sepnl ^ " then " ^ export e2 ^ sepnl ^ " else " ^ export e3
  | export (N {kind = (14, 7),  value, regions, children = [e1, e2]}) = "while " ^ export e1 ^ " do " ^ export e2
  | export (N {kind = (14, 8),  value, regions, children = [e]}) = "raise " ^ export e
  | export (N {kind = (14, 9),  value, regions, children = [a]}) = export a
  | export (N {kind = (14, 10), value, regions, children = [f, x]}) = export f ^ " " ^ export x
  | export (N {kind = (14, 11), value, regions, children = [e1, e2]}) = export e1 ^ " " ^ value ^ " " ^ export e2
  (* MATCH *)
  | export (N {kind = (15, 0),  value, regions, children}) = exportList children " | "
  (* MRULE *)
  | export (N {kind = (16, 0),  value, regions, children = [p, e]}) = export p ^ " => " ^ export e
  (* ATEXP *)
  | export (N {kind = (18, 0),  value, regions, children = [id]}) = export id
  | export (N {kind = (18, 1),  value, regions, children = [sc]}) = export sc
  | export (N {kind = (18, 2),  value, regions, children = [lab]}) = "#" ^ export lab
  | export (N {kind = (18, 3),  value, regions, children}) = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (18, 4),  value, regions, children = [s]}) = "(" ^ export s ^ ")"
  | export (N {kind = (18, 5),  value, regions, children = [x]}) = "(" ^ export x ^ ")"
  | export (N {kind = (18, 6),  value, regions, children}) = "{" ^ exportList children ", " ^ "}"
  | export (N {kind = (18, 7),  value, regions, children = [d, e]}) = "let " ^ export d ^ sepnl ^ " in " ^ export e ^ sepnl ^ " end"
  | export (N {kind = (18, 8),  value, regions, children = [d, s]}) = "let " ^ export d ^ sepnl ^ " in " ^ export s ^ sepnl ^ " end"
  | export (N {kind = (18, 9),  value, regions, children}) = "[" ^ exportList children ", " ^ "]"
  (* EXPROW *)
  | export (N {kind = (19, 0),  value, regions, children = [l, e]}) = export l ^ " = " ^ export e
  (* EXPSEQ *)
  | export (N {kind = (20, 0),  value, regions, children}) = exportList children ";"
  (* DECS *)
  | export (N {kind = (21, 0),  value, regions, children}) = exportList children (sepnl ^ " ")
  (* DEC *)
  | export (N {kind = (22, 0),  value, regions, children = [t, b]}) = "val " ^ export t ^ " " ^ export b
  | export (N {kind = (22, 1),  value, regions, children = [d]}) = "datatype " ^ export d
  | export (N {kind = (22, 2),  value, regions, children = [d, t]}) = "datatype " ^ export d ^ " withtype " ^ export t
  | export (N {kind = (22, 3),  value, regions, children = [t1, t2]}) = "datatype " ^ export t1 ^ " = datatype " ^ export t2
  | export (N {kind = (22, 4),  value, regions, children = [d1, d2]}) = "abstype " ^ export d1 ^ " with " ^ export d2 ^ " end"
  | export (N {kind = (22, 5),  value, regions, children = [d1, t, d2]}) = "abstype " ^ export d1 ^ " withtype " ^ export t ^ " with " ^ export d2 ^ " end"
  | export (N {kind = (22, 6),  value, regions, children = [t, f]}) = "fun " ^ export t ^ " " ^ export f
  | export (N {kind = (22, 7),  value, regions, children = [t]}) = "type " ^ export t
  | export (N {kind = (22, 8),  value, regions, children = [e]}) = "exception " ^ export e
  | export (N {kind = (22, 9),  value, regions, children = [d1, d2]}) = "local " ^ export d1 ^ " in " ^ export d1 ^ " end"
  (* EXBIND*)
  | export (N {kind = (23, 0),  value, regions, children}) = exportList children " and "
  (* ONEEXBIND *)
  | export (N {kind = (24, 0),  value, regions, children = [id]}) = export id
  | export (N {kind = (24, 1),  value, regions, children = [id, ty]}) = export id ^ " of " ^ export ty
  | export (N {kind = (24, 2),  value, regions, children = [id1, id2]}) = export id1 ^ " = " ^ export id2
  (* TYPBIND *)
  | export (N {kind = (25, 0),  value, regions, children}) = exportList children " and "
  (* ONETYPBIND *)
  | export (N {kind = (26, 0),  value, regions, children = [d, t]}) = export d ^ " = " ^ export t
  (* FVALBIND *)
  | export (N {kind = (27, 0),  value, regions, children}) = exportList children " and "
  (* ONEFVALBIND *)
  | export (N {kind = (28, 0),  value, regions, children}) = exportList children " | "
  (* BRANCHFVALBIND *)
  | export (N {kind = (29, 0),  value, regions, children = [f, e]}) = export f ^ " =" ^ sepnl ^ " " ^ export e
  (* FMATCHTY *)
  | export (N {kind = (30, 0),  value, regions, children = [f]}) = export f
  | export (N {kind = (30, 1),  value, regions, children = [f, t]}) = export f ^ " : " ^ export t
  (* FMATCH *)
  | export (N {kind = (31, 0),  value, regions, children = [f, a]}) = export f ^ " " ^ export a
  | export (N {kind = (31, 1),  value, regions, children = [f, a]}) = export f ^ " " ^ export a
  (* TYPESEQ *)
  | export (N {kind = (32, 0),  value, regions, children = [t]}) = export t
  | export (N {kind = (32, 1),  value, regions, children}) = "(" ^ exportList children ", " ^ ")"
  | export (N {kind = (32, 2),  value, regions, children = []}) = ""
  (* VALBIND *)
  | export (N {kind = (33, 0),  value, regions, children}) = "rec " ^ exportList children " and "
  | export (N {kind = (33, 1),  value, regions, children}) = exportList children " and "
  (* ONEVALBIND *)
  | export (N {kind = (35, 0),  value, regions, children = [p, e]}) = export p ^ " = " ^ export e
  (* DATNAME *)
  | export (N {kind = (36, 0),  value, regions, children = [tc, tv]}) = export tv ^ " " ^ export tc
  (* DATBIND *)
  | export (N {kind = (37, 0),  value, regions, children}) = exportList children " and "
  (* ONEDATBIND *)
  | export (N {kind = (38, 0),  value, regions, children = [d, c]}) = export d ^ " = " ^ export c
  (* FATPAT *)
  | export (N {kind = (39, 0),  value, regions, children = [x]}) = export x
  (* PROG *)
  | export (N {kind = (40, 0),  value, regions, children = [d]}) = export d
  | export (term as N {kind, value, regions, children}) =
    raise EH.DeadBranch "Term has a non valid form\n"

and exportList list sep =
    #1 (foldl (fn (elt, (x, y)) => (x ^ y ^ export elt, sep)) ("", "") list)

val unexpectedFormat  = "term has an unexpected format"
val wrongFormat       =  "wrong term format"

fun flattenFMatch (N {kind, value, regions, children}) =
    let val tok = toToken kind
    in if tok  = FMATCH_ID
       then case children of
		[f, a] => (f, [a])
	      | _ => raise EH.DeadBranch wrongFormat
       else if tok = FMATCH_APP
       then case children of
		[f, a] =>
		let val (id, args) = flattenFMatch f
		in (id, args @ [a])
		end
	      | _ => raise EH.DeadBranch wrongFormat
       else raise EH.DeadBranch unexpectedFormat
    end

(* To Python *)

fun separateList [] _ = ""
  | separateList [x] _ = x
  | separateList (x :: xs) sep = x ^ sep ^ separateList xs sep

fun toPython tab ret (N {kind, value, regions, children}) =
    case toToken kind of
	SCON_INT => value
      | SCON_WORD => raise EH.DeadBranch "Unsupported"
      | SCON_REAL => value
      | SCON_STRING => "\"" ^ value ^ "\""
      | SCON_CHAR => raise EH.DeadBranch "Unsupported"
      (* ID *)
      | ID_VID => value
      | ID_LONGID => raise EH.DeadBranch "Unsupported"
      | ID_TYCON => raise EH.DeadBranch "Unsupported"
      (* CONBIND *)
      | CONBIND_ID => raise EH.DeadBranch "Unsupported"
      | CONBIND_OP => raise EH.DeadBranch "Unsupported"
      | CONBIND_OF => raise EH.DeadBranch "Unsupported"
      | CONBIND_OPOF => raise EH.DeadBranch "Unsupported"
      (* CONBINDSEQ *)
      | CONBINDSEQ_SEQ => raise EH.DeadBranch "Unsupported"
      (* TYLAB *)
      | TYLAB_LAB => raise EH.DeadBranch "Unsupported"
      (* TYPEVAR *)
      | TYPEVAR_VAR => raise EH.DeadBranch "Unsupported"
      (* TYPEVARSEQ *)
      | TYPEVARSEQ_ONE => raise EH.DeadBranch "Unsupported"
      | TYPEVARSEQ_SEQ => raise EH.DeadBranch "Unsupported"
      | TYPEVARSEQ_EM => raise EH.DeadBranch "Unsupported"
      (* TYPEROW *)
      | TYPEROW_ROW => raise EH.DeadBranch "Unsupported"
      (* TYPE *)
      | TYPE_ARROW => raise EH.DeadBranch "Unsupported"
      | TYPE_TUPLE => raise EH.DeadBranch "Unsupported"
      | TYPE_TYCON => raise EH.DeadBranch "Unsupported"
      | TYPE_VAR => raise EH.DeadBranch "Unsupported"
      | TYPE_RECORD => raise EH.DeadBranch "Unsupported"
      | TYPE_PAREN => raise EH.DeadBranch "Unsupported"
      (* ATPAT *)
      | ATPAT_ID => raise EH.DeadBranch "Unsupported"
      | ATPAT_WILD => raise EH.DeadBranch "Unsupported"
      | ATPAT_SCON => raise EH.DeadBranch "Unsupported"
      | ATPAT_RECORD => raise EH.DeadBranch "Unsupported"
      | ATPAT_LIST => "[" ^ separateList (map (toPython tab ret) children) ", " ^ "]"
      | ATPAT_PAREN => raise EH.DeadBranch "Unsupported"
      | ATPAT_TUPLE => raise EH.DeadBranch "Unsupported"
      (* IDENTTY *)
      | IDENTTY_ID => raise EH.DeadBranch "Unsupported"
      | IDENTTY_TY => raise EH.DeadBranch "Unsupported"
      (* PATROW *)
      | PATROW_EQ => raise EH.DeadBranch "Unsupported"
      | PATROW_ID => raise EH.DeadBranch "Unsupported"
      | PATROW_AS => raise EH.DeadBranch "Unsupported"
      | PATROW_WILD => raise EH.DeadBranch "Unsupported"
      (* PAT *)
      | PAT_AS => raise EH.DeadBranch "Unsupported"
      | PAT_TYPE => raise EH.DeadBranch "Unsupported"
      | PAT_OP => raise EH.DeadBranch "Unsupported"
      | PAT_ATPAT => raise EH.DeadBranch "Unsupported"
      | PAT_APP => raise EH.DeadBranch "Unsupported"
      (* EXP *)
      | EXP_OR =>
	(case children of
	     [e1, e2] => toPython tab ret e1 ^ " or " ^ toPython tab ret e2
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_AND =>
	(case children of
	     [e1, e2] => toPython tab ret e1 ^ " and " ^ toPython tab ret e2
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_TYPE => raise EH.DeadBranch "Unsupported"
      | EXP_HANDLE => raise EH.DeadBranch "Unsupported"
      | EXP_FN => raise EH.DeadBranch "Unsupported"
      | EXP_CASE => raise EH.DeadBranch "Unsupported"
      | EXP_ITE =>
	(case children of
	     [e1, e2, e3] => "if (" ^ toPython tab ret e1 ^ "):\n" ^
			     tab ^ "  " ^ toPython (tab ^ "  ") ret e2 ^ "\n" ^
			     tab ^ "else:" ^
			     tab ^ "  " ^ toPython (tab ^ "  ") ret e3
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_WHILE => raise EH.DeadBranch "Unsupported"
      | EXP_RAISE => raise EH.DeadBranch "Unsupported"
      | EXP_ATEXP =>
	(case children of
	     [x] => toPython tab ret x
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_APP =>
	(case children of
	     [e1, e2] => toPython tab ret e1 ^ " " ^ toPython tab ret e2
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_OP => raise EH.DeadBranch "Unsupported"
      (* MATCH *)
      | MATCH_M => raise EH.DeadBranch "Unsupported"
      (* MRULE *)
      | MRULE_M => raise EH.DeadBranch "Unsupported"
      (* ATEXP *)
      | ATEXP_ID =>
	(case children of
	     [x] => toPython tab ret x
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_SCON =>
	(case children of
	     [x] => toPython tab ret x
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_SEL => raise EH.DeadBranch "Unsupported"
      | ATEXP_TUPLE => raise EH.DeadBranch "Unsupported"
      | ATEXP_SEQ => raise EH.DeadBranch "Unsupported"
      | ATEXP_PAREN =>
	(case children of
	     [exp] => "(" ^ toPython tab ret exp ^ ")"
	   | _  => raise EH.DeadBranch wrongFormat)
      | ATEXP_RECORD => raise EH.DeadBranch "Unsupported"
      | ATEXP_LET =>
	(case children of
	     [decs, exp] =>
	     (*"def sub():" ^
	     toPython (tab ^ "  ") ret decs ^*)
	     raise EH.TODO
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_SLET => raise EH.DeadBranch "Unsupported"
      | ATEXP_LIST => "[" ^ separateList (map (toPython tab ret) children) ", " ^ "]"
      (* EXPROW *)
      | EXPROW_ROW => raise EH.DeadBranch "Unsupported"
      (* EXPSEQ *)
      | EXPSEQ_SEQ => raise EH.DeadBranch "Unsupported"
      (* DECS *)
      | DECS_DECS => separateList (map (toPython tab ret) children) ("\n" ^ tab)
      (* DEC *)
      | DEC_VAL =>
	(case children of
	     [tyvars, bind] =>
	     (case toTokenTerm tyvars of
		  TYPEVARSEQ_EM => toPython tab ret bind
		| _ => raise EH.DeadBranch "Unsupported")
	   | _ => raise EH.DeadBranch wrongFormat)
      | DEC_DAT => raise EH.DeadBranch "Unsupported"
      | DEC_DATWITH => raise EH.DeadBranch "Unsupported"
      | DEC_DATREP => raise EH.DeadBranch "Unsupported"
      | DEC_ABSTYP => raise EH.DeadBranch "Unsupported"
      | DEC_ABSWITH => raise EH.DeadBranch "Unsupported"
      | DEC_FVAL =>
	(case children of
	     [tyvars, bind] =>
	     (case toTokenTerm tyvars of
		  TYPEVARSEQ_EM => "def " ^ toPython tab ret bind
		| _ => raise EH.DeadBranch "Unsupported")
	   | _ => raise EH.DeadBranch wrongFormat)
      | DEC_TYP => raise EH.DeadBranch "Unsupported"
      | DEC_EXC => raise EH.DeadBranch "Unsupported"
      | DEC_LOCAL => raise EH.DeadBranch "Unsupported"
      (* EXBIND *)
      | EXBIND_BIND => raise EH.DeadBranch "Unsupported"
      (* ONEEXBIND *)
      | ONEEXBIND_ID => raise EH.DeadBranch "Unsupported"
      | ONEEXBIND_OF => raise EH.DeadBranch "Unsupported"
      | ONEEXBIND_EQ => raise EH.DeadBranch "Unsupported"
      (* TYPBIND *)
      | TYPBIND_BIND => raise EH.DeadBranch "Unsupported"
      (* ONETYPBIND *)
      | ONETYPBIND_EQ => raise EH.DeadBranch "Unsupported"
      (* FVALBIND *)
      | FVALBIND_BIND =>
	(case children of
	     [bind] => toPython tab ret bind
	   | _ => raise EH.DeadBranch "Unsupported")
      (* ONEFVALBIND *)
      | ONEFVALBIND_BRANCHES =>
	(case children of
	     [branch] => toPython tab ret branch
	   | _ => raise EH.DeadBranch wrongFormat)
      (* BRANCHFVALBIND *)
      | BRANCHFVALBIND_BRANCH =>
	(case children of
	     [fmatch, exp] =>
	     toPython tab ret fmatch ^ ":\n" ^
	     toPython (tab ^ "  ") ret exp
	   | _ => raise EH.DeadBranch wrongFormat)
      (* FMATCHTY *)
      | FMATCHTY_FM =>
	(case children of
	     [fmatch] =>
	     let val (f, args) = flattenFMatch fmatch
		 val list_args =
		     map (fn arg =>
			     if toTokenTerm arg = FATPAT_PAT
			     then case getChildren arg of
				      [atpat] =>
				      if toTokenTerm atpat = ATPAT_ID
				      then case getChildren atpat of
					       [id] =>
					       if toTokenTerm id = ID_VID
					       then getValue id
					       else raise EH.DeadBranch unexpectedFormat
					     | _ => raise EH.DeadBranch wrongFormat
				      else raise EH.DeadBranch "Unsupported"
				    | _ => raise EH.DeadBranch wrongFormat
			     else raise EH.DeadBranch unexpectedFormat)
			 args
	     in if toTokenTerm f = ID_VID
		then getValue f ^ "(" ^ separateList list_args ", " ^ ")"
		else raise EH.DeadBranch unexpectedFormat
	     end
	   | _ => raise EH.DeadBranch wrongFormat)
      | FMATCHTY_TY => raise EH.DeadBranch "Unsupported"
      (* FMATCH *)
      | FMATCH_ID => raise EH.DeadBranch "Unsupported"
      | FMATCH_APP => raise EH.DeadBranch "Unsupported"
      (* TYPESEQ *)
      | TYPESEQ_ONE => raise EH.DeadBranch "Unsupported"
      | TYPESEQ_SEQ => raise EH.DeadBranch "Unsupported"
      | TYPESEQ_EM => raise EH.DeadBranch "Unsupported"
      (* VALBIND *)
      | VALBIND_REC => raise EH.DeadBranch "Unsupported"
      | VALBIND_NREC => raise EH.DeadBranch "Unsupported"
      (* ONEVALBIND *)
      | ONEVALBIND_EQ => raise EH.DeadBranch "Unsupported"
      (* DATNAME *)
      | DATNAME_DAT => raise EH.DeadBranch "Unsupported"
      (* DATBIND *)
      | DATBIND_BIND => raise EH.DeadBranch "Unsupported"
      (* ONEDATBIND *)
      | ONEDATBIND_EQ => raise EH.DeadBranch "Unsupported"
      (* FATPAT *)
      | FATPAT_PAT => raise EH.DeadBranch "Unsupported"
      (* PROG *)
      | PROG_PROG =>
	(case children of
	     [decs] => toPython tab ret decs
	   | _ => raise EH.DeadBranch wrongFormat)

val exportToPython = toPython "" false

(* Special strings *)

val evalFun           = "Eval"
val evaluator1Fun     = "Evaluator1"
val evaluator1pFun    = "Evaluator1'"
val evaluator2Fun     = "Evaluator2"
val evaluator2pFun    = "Evaluator2'"
val evaluator3Fun     = "Evaluator3"
val evaluator3pFun    = "Evaluator3'"
val evaluator4Fun     = "Evaluator4"
val evaluator4pFun    = "Evaluator4'"
val mkSimpleTermFun   = "mk_simple_term"
val mkTermFun         = "mk_term"
val destTermFun       = "dest_term"
val destSimpleTermFun = "dest_simple_term"
val foSubstFun        = "fo_subst"
val initEnvStr        = "env"
val emptyEnvStr       = "empty_env"
val initTermStr       = "term"
val initStepsStr      = "steps"
val addEnvBindingFun  = "add_env_binding"
val closure2TermFun   = "closure2term"
val lookupFun         = "lookup_binding"
val destVariableFun   = "dest_variable"
val freshVariableFun  = "fresh_variable"
val mkVariableTermFun = "mk_variable_term"
val destIntegerFun    = "dest_integer"

(* Term updating functions *)

fun updValue (N {kind, value = _, regions, children}) value =
    N {kind = kind, value = value, regions = regions, children = children}

fun updChildren (N {kind, value, regions, children = _}) children =
    N {kind = kind, value = value, regions = regions, children = children}

(* Useful functions for transformations *)

fun getIDinFATPAT (t as N {kind     = kind1,
			   value    = value1,
			   regions  = regions1,
			   children = [N {kind     = kind2,
					  value    = value2,
					  regions  = regions2,
					  children = [x as N {kind     = kind3,
							      value    = value3,
							      regions  = regions3,
							      children = []}]}]}) =
    if toToken kind1 = FATPAT_PAT
       andalso
       toToken kind2 = ATPAT_ID
       andalso
       toToken kind3 = ID_VID
    then x
    else (print_term t; raise EH.DeadBranch "")
  | getIDinFATPAT term = (print_term term; raise EH.DeadBranch "")

fun isAtPatPair (N {kind, value, regions, children}) =
    toToken kind = ATPAT_TUPLE
    andalso
    (case children of [_, _] => true | _ => false)

fun isPatPair (N {kind, value, regions, children}) =
    toToken kind = PAT_ATPAT
    andalso
    (case children of [atpat] => isAtPatPair atpat | _ => false)

fun getIdId (N {kind, value, regions, children}) =
    if toToken kind = ID_VID
    then value
    else raise EH.DeadBranch wrongFormat

fun getAtPatId (N {kind, value, regions, children}) =
    if toToken kind = ATPAT_ID
    then case children of
	     [id] => getIdId id
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch wrongFormat

fun getPatId (N {kind, value, regions, children}) =
    if toToken kind = PAT_ATPAT
    then case children of
	     [atpat] => getAtPatId atpat
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch wrongFormat

fun getAtPatTriple (N {kind, value, regions, children}) =
    if toToken kind = ATPAT_TUPLE
    then case children of
	     [p1, p2, p3] => (getPatId p1, getPatId p2)
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch wrongFormat

fun getPatTriple (N {kind, value, regions, children}) =
    if toToken kind = PAT_ATPAT
    then case children of
	     [atpat] => getAtPatTriple atpat
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch wrongFormat

fun pushEnvInPatPair (term as N {kind, value, regions, children}) id =
    if toToken kind = PAT_ATPAT
    then let val children' =
		 case children of
		     [term as N {kind, value, regions, children}] =>
		     if toToken kind = ATPAT_TUPLE
		     then let val children' =
				  case children of
				      [p1, p2] =>
				      let val a1 = mk_term ID_VID id [] []
					  val a2 = mk_term ATPAT_ID "" [] [a1]
					  val a3 = mk_term PAT_ATPAT "" [] [a2]
				      in [p1, a3, p2]
				      end
				    | _ => raise EH.DeadBranch wrongFormat
			  in [updChildren term children']
			  end
		     else raise EH.DeadBranch wrongFormat
		   | _ => raise EH.DeadBranch wrongFormat
	 in updChildren term children'
	 end
    else raise EH.DeadBranch wrongFormat

fun isExpApp (N {kind, value, regions, children}) =
    toToken kind = EXP_APP

fun isIdId (N {kind, value, regions, children}) id =
    toToken kind = ID_VID
    andalso
    (case id of SOME st => value = st | NONE => true)

(* 'id' is a string option.
 * If it is a SOME st then we check that it is an ident st.
 * If it is a NONE then we just check that it is an ident. *)
fun isAtExpId (N {kind, value, regions, children}) id =
    toToken kind = ATEXP_ID
    andalso
    (case children of [ident] => isIdId ident id | _ => false)

fun isExpId (N {kind, value, regions, children}) id =
    toToken kind = EXP_ATEXP
    andalso
    (case children of [atexp] => isAtExpId atexp id | _ => false)

fun isExpNAppId (N {kind, value, regions, children}) id n =
    (List.length n = 0
     andalso
     toToken kind = EXP_ATEXP
     andalso
     (case children of [atexp] => isAtExpId atexp (SOME id) | _ => false))
    orelse
    (List.length n > 0
     andalso
     toToken kind = EXP_APP
     andalso
     (case children of
	  [exp, atexp] =>
	  isExpNAppId exp id (List.tl n)
	  andalso
	  (case List.hd n of
	       NONE => true
	     | SOME id' => isAtExpId atexp (SOME id'))
	| _ => false))

fun isExpOneAppEval          exp = isExpNAppId exp evalFun [NONE]
fun isExpTwoAppEval          exp = isExpNAppId exp evalFun [NONE, NONE]
fun isExpThreeAppEval        exp = isExpNAppId exp evalFun [NONE, NONE, NONE]
fun isExpTwoAppEvaluator1'   exp = isExpNAppId exp evaluator1pFun [NONE, NONE]
fun isExpThreeAppEvaluator1' exp = isExpNAppId exp evaluator1pFun [NONE, NONE, NONE]
fun isExpFourAppEvaluator1'  exp = isExpNAppId exp evaluator1pFun [NONE, NONE, NONE, NONE]

fun isExpAppMk exp =
    isExpNAppId exp mkSimpleTermFun [NONE]
    orelse
    isExpNAppId exp mkTermFun [NONE]

fun isExpOneAppInitDest exp =
    isExpNAppId exp destTermFun [SOME initTermStr]
    orelse
    isExpNAppId exp destSimpleTermFun [SOME initTermStr]

fun pushEnvInExpApp exp id =
    let val tid   = mk_term ID_VID id [] []
	val atexp = mk_term ATEXP_ID "" [] [tid]
    in mk_term EXP_APP "" [] [exp, atexp]
    end

fun replaceLastIdInExpApp (exp as N {kind, value, regions, children}) id =
    if toToken kind = EXP_APP
    then let val children' =
		 case children of
		     [e, ae] =>
		     let val ae1 = mk_term ID_VID id [] []
			 val ae2 = mk_term ATEXP_ID "" [] [ae1]
		     in [e, ae2]
		     end
		   | _ => raise EH.DeadBranch wrongFormat
	 in updChildren exp children'
	 end
    else raise EH.DeadBranch unexpectedFormat

fun isAtexpEmptyList (N {kind, value, regions, children}) =
    toToken kind = ATEXP_LIST
    andalso
    null children

fun isExpEmptyList (N {kind, value, regions, children}) =
    toToken kind = EXP_ATEXP
    andalso
    (case children of [atexp] => isAtexpEmptyList atexp | _ => false)

fun expToAtExp (term as N {kind, value, regions, children}) =
    if kindIsExp kind
    then if toToken kind = EXP_ATEXP
	 then case children of
		  [atexp] => atexp
		| _ => raise EH.DeadBranch wrongFormat
	 else mk_term ATEXP_PAREN "" [] [term]
    else raise EH.DeadBranch unexpectedFormat

fun atExpListToList (N {kind, value, regions, children}) =
    if toToken kind = ATEXP_LIST
    then let fun getPairsIds (term as N {kind, value, regions, children}) =
		 if toToken kind = EXP_ATEXP
		 then case children of
			  [atexp as N {kind, value, regions, children}] =>
			  if toToken kind = ATEXP_TUPLE
			  then case children of
				   [e1, e2] => (expToAtExp e1, expToAtExp e2)
				 | _ => raise EH.DeadBranch unexpectedFormat
			  else raise EH.DeadBranch unexpectedFormat
			| _ => raise EH.DeadBranch wrongFormat
		 else raise EH.DeadBranch unexpectedFormat
	 in map getPairsIds children
	 end
    else raise EH.DeadBranch unexpectedFormat

fun destExpSubst (N {kind, value, regions, children}) =
    case toToken kind of
	EXP_ATEXP =>
	(case children of
	     [atexp] => destExpSubst atexp
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_PAREN =>
	(case children of
	     [exp] => destExpSubst exp
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_APP =>
	(case children of
	     [e1 as N {kind, value, regions, children}, ae2] =>
	     if toToken kind = EXP_APP
	     then case children of
		      [e11, ea12] =>
		      if isExpId e11 (SOME foSubstFun)
		      then SOME (atExpListToList ea12, ae2)
		      else NONE
		    | _ => raise EH.DeadBranch wrongFormat
	     else NONE
	   | _ => raise EH.DeadBranch wrongFormat)
      | _ => NONE

fun getVariablesPattern (term as N {kind, value, regions, children}) =
    case toToken kind of
	PAT_ATPAT   =>
	(case children of
	     [atpat] => getVariablesPattern atpat
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATPAT_ID    =>
	(case children of
	     [id as N {kind, value, regions, children}] =>
	     (case toToken kind of
		  ID_VID => [value]
		| _ => raise EH.DeadBranch wrongFormat)
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATPAT_LIST  =>
	List.concat (List.map getVariablesPattern children)
      | ATPAT_PAREN =>
	(case children of
	     [pat] => getVariablesPattern pat
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATPAT_TUPLE =>
	List.concat (List.map getVariablesPattern children)
      | _ => raise EH.DeadBranch unexpectedFormat

fun getVariablesExpression (term as N {kind, value, regions, children}) =
    case toToken kind of
	EXP_ATEXP   =>
	(case children of
	     [atexp] => getVariablesExpression atexp
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_ID    =>
	(case children of
	     [id as N {kind, value, regions, children}] =>
	     (case toToken kind of
		  ID_VID => [value]
		| _ => raise EH.DeadBranch wrongFormat)
	   | _ => raise EH.DeadBranch wrongFormat)
      | EXP_APP  =>
	(case children of
	     [e, ae] => getVariablesExpression ae
	   (* NOTE: We might want a bit more than that.
	    * But currently this is how we don't get 'subterm' in the
	    * decide case when dealing with the term 'subterm 1 v'.
	    * getEnvOfId only returns v then. *)
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_PAREN =>
	(case children of
	     [e] => getVariablesExpression e
	   | _ => raise EH.DeadBranch wrongFormat)
      | ATEXP_TUPLE => List.concat (map getVariablesExpression children)
      | ATEXP_LIST => List.concat (map getVariablesExpression children)
      | EXP_OP => List.concat (map getVariablesExpression children)
      | ATEXP_SCON => []
      | _ => (print (toString term);
	      raise EH.DeadBranch unexpectedFormat)

fun getVariables (N {kind, value, regions, children}) =
    if toToken kind = ID_VID
    then [value]
    else List.concat (map getVariables children)

fun isin_var_list _ [] = false
  | isin_var_list (var : string) (x :: xs) =
    var = x orelse isin_var_list var xs

fun concat_var_list [] list = list
  | concat_var_list (x :: xs) list =
    if isin_var_list x list
    then concat_var_list xs list
    else x :: concat_var_list xs list

fun idToEnv (list1, list2, list3, list4) id =
    let fun closeEnv env =
	    foldr (fn ((env1, env2), envs) => concat_var_list (closeEnv env2) envs)
		  []
		  (List.filter (fn (env1 : string, env2) => env1 = env) list4)
	fun id_to_env [] = NONE
	  | id_to_env ((id', env) :: list) =
	    if id = (id' : string)
	    then case closeEnv env of
		     []    => SOME env
		   | [env] => SOME env
		   | _     => raise EH.DeadBranch "term could potentially depend on various environments"
	    else id_to_env list
    in id_to_env list1
    end

fun idToEnv2 (lists as (list1, list2, list3, list4)) id =
    let fun id_to_env [] =
	    (case idToEnv lists id of
		 SOME env => [env]
	       | _ => [])
	  | id_to_env ((ids1, ids2 : string list) :: list) =
	    (case List.find (fn x => x = (id : string)) ids1 of
		 (* NOTE: We can use find here because ids1 comes from patterns
		  * and an identifier can only be declared ones, and our lists
		  * are stacks. *)
		 SOME _ =>
		 List.foldr (fn (id, envs) =>
				concat_var_list envs (idToEnv2 lists id))
			    []
			    ids2
	       | NONE => id_to_env list)
    in id_to_env list2
    end

fun getEnvOfId term lists env =
    case getVariablesExpression term of
	[id] => idToEnv2 lists id
      | _ => raise EH.DeadBranch ""

fun getOneEnvOfId term lists env =
    case getEnvOfId term lists env of
	[]    => env
      | [env] => env
      | _     => raise EH.DeadBranch "term could potentially depend on various environments"

fun idToAtPat id =
    let val x1 = mk_term ID_VID id [] []
    in mk_term ATPAT_ID "" [] [x1]
    end

fun idToPat id =
    let val x1 = idToAtPat id
    in mk_term PAT_ATPAT "" [] [x1]
    end

fun idToAtExp id =
    let val x1 = mk_term ID_VID id [] []
    in mk_term ATEXP_ID "" [] [x1]
    end

fun idToExp id =
    let val x1 = idToAtExp id
    in mk_term EXP_ATEXP "" [] [x1]
    end

fun expTestEqVariable (term as N {kind, value, regions, children}) =
    toToken kind = EXP_OP
    andalso
    value = "="
    andalso
    (case children of
	 [e1, e2] =>
	 let fun isExpStr (term as N {kind, value, regions, children}) =
		 toToken kind = EXP_ATEXP
		 andalso
		 (case children of
		      [atexp as N {kind, value, regions, children}] =>
		      (toToken kind = ATEXP_SCON
		       andalso
		       (case children of
			    [sc as N {kind, value, regions, children}] =>
			    toToken kind = SCON_STRING
			    andalso
			    value = "\"variable\""
			  | _ => raise EH.DeadBranch wrongFormat))
		    | _ => raise EH.DeadBranch wrongFormat)
	 in isExpStr e1 orelse isExpStr e2
	 end
       | _ => raise EH.DeadBranch wrongFormat)

fun destructPatternEvalThree (pat as N {kind, value, regions, children}) =
    if toToken kind = PAT_ATPAT
    then case children of
	     [atpat as N {kind, value, regions, children}] =>
	     if toToken kind = ATPAT_TUPLE
	     then case children of
		      [p1, p2, p3] => (p1, p2, p3)
		    | _ => raise EH.DeadBranch unexpectedFormat
	     else raise EH.DeadBranch unexpectedFormat
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch unexpectedFormat

fun subTerm sub (term as N {kind, value, regions, children}) =
    if toToken kind = ID_VID
    then case List.find (fn (v : string, t) => v = value) sub of
	     SOME (v, t) => t
	   | NONE => term
    else updChildren term (map (subTerm sub) children)

fun freshVariable vars var =
    if isin_var_list var vars
    then freshVariable vars (var ^ "'")
    else var

(* TRANSFORMATIONS *)

(* TRANSFORMATION 1.1 *)

fun transform1_1 (term as N {kind, value, regions, children}) =
    if toToken kind = FVALBIND_BIND
    then let val children' =
		 case children of
		     [N {kind, value, regions, children}] =>
		     if toToken kind = ONEFVALBIND_BRANCHES
		     then let val children' =
				  case children of
				      [N {kind, value, regions, children}] =>
				      if toToken kind = BRANCHFVALBIND_BRANCH
				      then let val children' =
						   case children of
						       [t as N {kind, value, regions, children}, e] =>
						       let val tok = toToken kind
							   val (fmatch, opty) =
							       if tok = FMATCHTY_FM
							       then case children of
									[f] => (f, NONE)
								      | _ => raise EH.DeadBranch wrongFormat
							       else if tok = FMATCHTY_TY
							       then case children of
									[f, t] => (f, SOME t)
								      | _ => raise EH.DeadBranch wrongFormat
							       else raise EH.DeadBranch wrongFormat
							   val (id, args) = flattenFMatch fmatch
						       (* Check that id is 'Eval' that takes two arguments
							* if it is then add an extra parameter *)
						       in case id of
							      N {kind = k, value = v, regions = _, children = _} =>
							      if toToken k = ID_VID
								 andalso
								 ((v = evalFun
								   andalso
								   List.length args = 2)
								  orelse
								  (v = evaluator1pFun
								   andalso
								   List.length args = 3))
							      then let val t1 = mk_term ID_VID initEnvStr [] []
								       val t2 = mk_term ATPAT_ID "" [] [t1]
								       val t3 = mk_term FATPAT_PAT "" [] [t2]
								       val t4 = mk_term FMATCH_ID "" [] [fmatch, t3]
								   in case opty of
									  SOME ty => [mk_term tok value regions [t4, ty], transform1_1 e]
									| NONE => [mk_term tok value regions [t4], transform1_1 e]
								   end
							      else [t, transform1_1 e]
						       end
						     | _ => raise EH.DeadBranch wrongFormat
					   in [N {kind = kind, value = value, regions = regions, children = children'}]
					   end
				      else raise EH.DeadBranch wrongFormat
				    | _ => map transform1_1 children
			  in [N {kind = kind, value = value, regions = regions, children = children'}]
			  end
		     else raise EH.DeadBranch wrongFormat
		   | _ => map transform1_1 children
	 in updChildren term children'
	 end
    else updChildren term (map transform1_1 children)

(* TRANSFORMATION 1.2 *)

(* transform the pairs pattern/expression when application of Eval *)
fun transform1_2_val (term as N {kind, value, regions, children}) n =
    if toToken kind = ONEVALBIND_EQ
    then let val (children', c) =
		 (* b is true if we actually updated something *)
		 case children of
		     [p, e] =>
		     if isPatPair p
			andalso
			(isExpTwoAppEval e
			 orelse
			 isExpThreeAppEvaluator1' e)
		     then ([pushEnvInPatPair p (initEnvStr ^ Int.toString n),
			    pushEnvInExpApp e initEnvStr], true)
		     else ([p, transform1_2 e], false)
		   | _ => raise EH.DeadBranch wrongFormat
	 in (updChildren term children', c)
	 end
    else raise EH.DeadBranch wrongFormat

(* transform each non-recursive value declaration *)
and transform1_2_dec (term as N {kind, value, regions, children}) n =
    if toToken kind = DEC_VAL
    then let val (children', c) =
		 case children of
		     [t, b as N {kind, value, regions, children}] =>
		     if toToken kind = VALBIND_NREC
		     then let val (children', c) =
				  case children of
				      [t] =>
				      let val (t', c) = transform1_2_val t n
				      in ([t'], c)
				      end
				    | _ => (map transform1_2 children, false)
			  in ([t, updChildren b children'], c)
			  end
		     else ([t, transform1_2 b], false)
		   | _ => raise EH.DeadBranch wrongFormat
	 in (updChildren term children', c)
	 end
    else (updChildren term (map transform1_2 children), false)

(* transform each declarations in a sequence of declarations *)
and transform1_2_let (term as N {kind, value, regions, children}) =
    if toToken kind = DECS_DECS
    then let val children' = #1 (foldl (fn (t, (xs, n)) =>
					   let val (t', b) = transform1_2_dec t n
					       val n' = if b then n + 1 else n
					   in (xs @ [t'], n')
					   end)
				       ([], 1)
				       children)
	 in updChildren term children'
	 end
    else raise EH.DeadBranch wrongFormat

(* transfrom the declarations in a let expression *)
and transform1_2 (term as N {kind, value, regions, children}) =
    if toToken kind = ATEXP_LET
    then let val children' =
		 case children of
		     [decs, exp] => [transform1_2_let decs, transform1_2 exp]
		   | _ => raise EH.DeadBranch wrongFormat
	 in updChildren term children'
	 end
    else updChildren term (map transform1_2 children)

(* TRANSFORMATION 1.3 *)

(* Add 'env' to each pair at the leaves.
 * Note that it is wrong to do so for the very last pair, but this is
 * fixed by transformation 5. *)

fun transform1_3 (term as N {kind, value, regions, children}) =
    if kindIsExp kind
    then let val children' =
		 case toToken kind of
		     EXP_ITE =>
		     (case children of
			  [e1, e2, e3] => [e1, transform1_3 e2, transform1_3 e3]
			| _ => raise EH.DeadBranch wrongFormat)
		   | EXP_HANDLE =>
		     (case children of
			  [e, m] => [transform1_3 e, transform1_3 m]
			| _ => raise EH.DeadBranch wrongFormat)
		   | EXP_ATEXP =>
		     (case children of
			  [a] => [transform1_3 a]
			| _ => raise EH.DeadBranch wrongFormat)
		   | _ => children
	 in updChildren term children'
	 end
    else if kindIsAtExp kind
    then let val children' =
		 case toToken kind of
		     ATEXP_TUPLE =>
		     (case children of
			  [e1, e2] =>
			  (* NOTE: Isn't that too much do to that for all tuples
			   * isn't enough to do that for pairs after 'in' and 'handle' *)
			  let val x1 = mk_term ID_VID initEnvStr [] []
			      val x2 = mk_term ATEXP_ID "" [] [x1]
			      val x3 = mk_term EXP_ATEXP "" [] [x2]
			  in [e1, x3, e2]
			  end
			| _ => children)
		   | ATEXP_LET =>
		     (case children of
			  [d, e] => [transform1_3 d, transform1_3 e]
			| _ => raise EH.DeadBranch wrongFormat)
		   | _ => children
	 in updChildren term children'
	 end
    else if toToken kind = MATCH_M
    then updChildren term (map transform1_3 children)
    else if toToken kind = MRULE_M
    then let val children' =
		 case children of
		     [p, e] => [p, transform1_3 e]
		   | _ => raise EH.DeadBranch wrongFormat
	 in updChildren term children'
	 end
    else updChildren term (map transform1_3 children)

(* TRANSFORMATION 1.4 *)

(* Transforms the fo_subst in environments. *)

(* list1 is the list of the variables obtained by Eval
 *   - pairs: variables, environment (identifiers).
 * list2 is the list of the variables obtained by non initial dests
 *   - as pairs of lists of variables.
 * list3 is the list of the variables obtained by the initial dest
 *   - list of identifiers.
 * list4 is the list of safe sub-environments
 *   - list of pairs of environments. *)
fun transform1_4_val (term as N {kind, value, regions, children})
		     (lists as (list1, list2, list3, list4))
		     vars =
    if toToken kind = ONEVALBIND_EQ
    then case children of
	     [p, e] =>
	     if isExpThreeAppEval e
		orelse
		isExpFourAppEvaluator1' e
	     then ([], term, ((getPatTriple p) :: list1, list2, list3, list4))
	     else if isExpOneAppInitDest e
	     (* Then we wanna get variables in the destructed pattern *)
	     then ([], term, (list1, list2, (getVariablesPattern p) @ list3, list4))
	     else if isExpApp e
	     (* If it is another application then get two lists:
	      *  - the variables from the pattern
	      *  - the variables from the expression.
	      *
	      * We also want to replace the the identifier depending on an
	      * environment env' while the other ones depend on env.
	      *)
	     then let val (pvars, evars) =
			  if isExpNAppId e destIntegerFun [NONE]
			  (* NOTE: This ia a really special case to deal with
			   * the ind case, because an integer does not depend
			   * on an environment. *)
			  then ([], [])
			  else (getVariablesPattern p, getVariablesExpression e)
		      (* If the evars depend on different environment then it's
		       * gonna be a problem to provide a single evaluation environment
		       * for the pvars variables. *)
		      fun sameEnv [] _ = true
			| sameEnv ((v, e) :: xs) NONE = sameEnv xs (SOME e)
			| sameEnv ((v, e : string) :: xs) (SOME env) =
			  e = env andalso sameEnv xs (SOME env)
		      fun getNonInit [] = []
			| getNonInit ((v, e) :: xs) =
			  if e = initEnvStr
			  then getNonInit xs
			  else (v, e) :: getNonInit xs
		      fun getOneNonInitEnv vars =
			  if sameEnv vars NONE
			  then NONE
			  else case getNonInit vars of
				   [] => raise EH.DeadBranch ""
				 | [(v, e)] => SOME (v, e)
				 | _ => raise EH.DeadBranch "A term might depend on at least two non-initial environments and we currently do not deal with that."
		      val varse = foldr (fn (var, vars) =>
					     let val e = getOneEnvOfId (idToExp var) lists initEnvStr
					     in (var, e) :: vars
					     end)
					 []
					 evars
		      val (decs, sub, list1', list4') =
			  case getOneNonInitEnv varse of
			      SOME (v, e) =>
			      let val w = freshVariable vars "w"
				  val envr = freshVariable vars  "envr"
				  (**)
				  val patw   = idToPat w
				  val fresh  = idToExp freshVariableFun
				  val t      = idToAtExp initTermStr
				  val exp    = mk_term EXP_APP "" [] [fresh, t]
				  val bind11 = mk_term ONEVALBIND_EQ "" [] [patw, exp]
				  val bind12 = mk_term VALBIND_NREC "" [] [bind11]
				  val tyvars = mk_term TYPEVARSEQ_EM "" [] []
				  val bind13 = mk_term DEC_VAL "" [] [tyvars, bind12]
				  (**)
				  val pate   = idToPat envr
				  val add    = idToExp addEnvBindingFun
				  val env    = idToAtExp initEnvStr
				  val tw     = idToAtExp w
				  val tb     = idToAtExp v
				  val te'    = idToAtExp e
				  val appe1  = mk_term EXP_APP "" [] [add, env]
				  val appe2  = mk_term EXP_APP "" [] [appe1, tw]
				  val appe3  = mk_term EXP_APP "" [] [appe2, tb]
				  val appe4  = mk_term EXP_APP "" [] [appe3, te']
				  val bind21 = mk_term ONEVALBIND_EQ "" [] [pate, appe4]
				  val bind22 = mk_term VALBIND_NREC "" [] [bind21]
				  val bind23 = mk_term DEC_VAL "" [] [tyvars, bind22]
				  (**)
				  val mkv    = idToExp mkVariableTermFun
				  val wa     = idToAtExp w
				  val app    = mk_term EXP_APP "" [] [mkv, wa]
				  val sub    = [(v, app)]
				  (* NOTE: This substition might substitute too much.
				   * We might need to have unique identifier for each
				   * term node. *)
				  (**)
				  val extend1 = [(w, envr)]
				  val extend4 = [(initEnvStr, envr)]
			      in ([bind13, bind23], sub, extend1, extend4)
			      end
			    | NONE => ([], [], [], [])
		      val evars' = List.foldr (fn (var, vars) =>
						  case List.find (fn (v : string, t) => var = v) sub of
						      SOME (v, t) => (getVariablesExpression t) @ vars
						    | NONE => var :: vars)
					      []
					      evars
		      val pair = (pvars, evars')
		      val lists' = (list1' @ list1, pair :: list2, list3, list4' @ list4)
		  in (decs, updChildren term [p, subTerm sub e], lists')
		  end
	     else ([], term, lists)
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch wrongFormat

and transform1_4_dec (term as N {kind, value, regions, children}) lists vars =
    if toToken kind = DEC_VAL
    then case children of
	     [tvs, b as N {kind, value, regions, children}] =>
	     if toToken kind = VALBIND_NREC
	     then case children of
		      [bind] =>
		      let val (decs, bind', lists') = transform1_4_val bind lists vars
		      in (decs, updChildren term ([tvs, updChildren b [bind']]), lists')
		      end
		    | _ => ([], term, lists)
	     else ([], term, lists)
	   | _ => raise EH.DeadBranch wrongFormat
    else ([], term, lists)

and transform1_4_let (term as N {kind, value, regions, children}) lists =
    if toToken kind = DECS_DECS
    then let val (decs, lists', _) =
		 foldl (fn (dec, (decs, lists, vars)) =>
			   let val (decs', dec', lists') = transform1_4_dec dec lists vars
			       val vars' = foldr (fn (dec, vars) => concat_var_list (getVariables dec) vars) vars (dec' :: decs')
			   in (decs @ decs' @ [dec'], lists', vars')
			   end)
		       ([], lists, getVariables term)
		       children
	 in (updChildren term decs, lists')
	 end
    else raise EH.DeadBranch wrongFormat

and transform1_4_mklistexp (term as N {kind, value, regions, children}) lists =
    if toToken kind = EXP_ATEXP
    then case children of
	     [atexp as N {kind, value, regions, children}] =>
	     if toToken kind = ATEXP_ID
	     then case children of
		      [id as N {kind, value, regions, children}] =>
		      (case idToEnv lists value of
			   SOME env =>
			   let val c1 = mk_term ID_VID closure2TermFun [] []
			       val c2 = mk_term ATEXP_ID "" [] [c1]
			       val c3 = mk_term EXP_ATEXP "" [] [c2]
			       val t1 = mk_term ID_VID env [] []
			       val t2 = mk_term ATEXP_ID "" [] [t1]
			       val t3 = mk_term EXP_ATEXP "" [] [t2]
			       val p1 = mk_term ATEXP_TUPLE "" [] [term, t3]
			       val a1 = mk_term EXP_APP "" [] [c3, p1]
			   in a1
			   end
			 | NONE => term)
		    | _ => raise raise EH.DeadBranch wrongFormat
	     else if toToken kind = ATEXP_TUPLE
	     then case children of
		      [l, e as N {kind, value, regions, children}] =>
		      if isExpEmptyList l
		      then if toToken kind = EXP_ATEXP
			   then case children of
				    [ae as N {kind, value, regions, children}] =>
				    if toToken kind = ATEXP_ID
				    then case children of
					     [id as N {kind, value, regions, children}] =>
					     (case idToEnv lists value of
						  SOME env =>
						  let val c1 = mk_term ID_VID closure2TermFun [] []
						      val c2 = mk_term ATEXP_ID "" [] [c1]
						      val c3 = mk_term EXP_ATEXP "" [] [c2]
						      val t1 = mk_term ID_VID env [] []
						      val t2 = mk_term ATEXP_ID "" [] [t1]
						      val t3 = mk_term EXP_ATEXP "" [] [t2]
						      val p1 = mk_term ATEXP_TUPLE "" [] [e, t3]
						      val a1 = mk_term EXP_APP "" [] [c3, p1]
						  in updChildren term [updChildren atexp [l, a1]]
						  end
						| NONE => term)
					   | _ => raise raise EH.DeadBranch wrongFormat
				    else term
				  | _ => raise raise EH.DeadBranch wrongFormat
			   else term
		      else term
		    | _ => term
	     else term
	   | _ => raise raise EH.DeadBranch wrongFormat
    else term

and transform1_4_mklist (term as N {kind, value, regions, children}) lists =
    if toToken kind = ATEXP_LIST
    then let val children' =
		 map (fn e => transform1_4_mklistexp e lists)
		     children
	 in updChildren term children'
	 end
    else term

and transform1_4_mkapp (term as N {kind, value, regions, children}) lists =
    if toToken kind = EXP_APP
    then let val children' =
		 case children of
		     [e1, ea2] =>
		     if isExpAppMk e1
		     then [e1, transform1_4_mklist ea2 lists]
		     else [e1, ea2]
		   | _ => raise EH.DeadBranch wrongFormat
	 in updChildren term children'
	 end
    else term

and transform1_4_mk (term as N {kind, value, regions, children}) lists =
    if toToken kind = EXP_ATEXP
    then let val children' =
		 case children of
		     [atexp as N {kind, value, regions, children}] =>
		     if toToken kind = ATEXP_TUPLE
		     then let val children' =
				  case children of
				      [t, e, s] => [transform1_4_mkapp t lists, e, s]
				    | _ => children
			  in [updChildren atexp children']
			  end
		     else [atexp]
		   | _ => raise EH.DeadBranch wrongFormat
	 in updChildren term children'
	 end
    else term

and transform1_4_list (term as N {kind, value, regions, children}) lists =
    case toToken kind of
	ATEXP_LET =>
	let val children' =
		case children of
		    [decs, exp] =>
		    let val (decs', lists') = transform1_4_let decs lists
		    in [transform1_4_list decs' lists,
			transform1_4_list exp lists']
		    end
		  | _ => raise EH.DeadBranch wrongFormat
	in updChildren term children'
	end
      | EXP_HANDLE =>
	let val children' =
		case children of
		    [e, match as N {kind, value, regions, children}] =>
		    if toToken kind = MATCH_M
		    then let val children' =
				 case children of
				     [mrule as N {kind, value, regions, children}] =>
				     if toToken kind = MRULE_M
				     then let val children' =
						  case children of
						      [p, e] => [p, transform1_4_mk e lists]
						    | _ => raise EH.DeadBranch wrongFormat
					  in [updChildren mrule children']
					  end
				     else raise EH.DeadBranch wrongFormat
				   | _ => children
			 in [transform1_4_list e lists,
			     updChildren match children']
			 end
		    else raise EH.DeadBranch wrongFormat
		  | _ => raise EH.DeadBranch wrongFormat
	in updChildren term children'
	end
      | EXP_APP =>
	(case children of
	     [e1, ea2] =>
	     if isExpOneAppEval e1
		orelse
		isExpTwoAppEvaluator1' e1
	     (* Note: The problem here is that we're counting on the fact
	      * that the fo_subst is in the Eval expression and not outide
	      * but it might as well be outside.
	      * This solution is too restrictive. *)
	     then case destExpSubst ea2 of
		      NONE =>
		      let val i1 = mk_term ID_VID initEnvStr [] []
			  val i2 = mk_term ATEXP_ID "" [] [i1]
		      in mk_term EXP_APP "" [] [term, i2]
		      end
		    | SOME (list_sub, ae_term) =>
		      let fun toEnvs (v, t) env =
			      let val env' = env ^ "'"
				  (* NOTE: An issue is that env' could already
				   * be declared in the term *)
				  val p1 = mk_term ID_VID env' [] []
				  val p2 = mk_term ATPAT_ID "" [] [p1]
				  val p3 = mk_term PAT_ATPAT "" [] [p2]
				  (**)
				  val e1 = mk_term ID_VID addEnvBindingFun [] []
				  val e2 = mk_term ATEXP_ID "" [] [e1]
				  val e3 = mk_term EXP_ATEXP "" [] [e2]
				  val f1 = mk_term ID_VID env [] []
				  val f2 = mk_term ATEXP_ID "" [] [f1]
				  (* this environment depends on the environment in which t is evaluated
				   * - if t is in list1 then take the associated env.
				   * - if t is in list2 then take the associated env. *)
				  val g1 = mk_term ID_VID (getOneEnvOfId t lists initEnvStr) [] []
				  val g2 = mk_term ATEXP_ID "" [] [g1]
				  (**)
				  val a1 = mk_term EXP_APP "" [] [e3, f2]
				  val a2 = mk_term EXP_APP "" [] [a1, v]
				  val a3 = mk_term EXP_APP "" [] [a2, t]
				  val a4 = mk_term EXP_APP "" [] [a3, g2]
				  (**)
				  val tv  = mk_term TYPEVARSEQ_EM "" [] []
				  (**)
				  val vb1 = mk_term ONEVALBIND_EQ "" [] [p3, a4]
				  val vb2 = mk_term VALBIND_NREC "" [] [vb1]
				  val vb3 = mk_term DEC_VAL "" [] [tv, vb2]
			      in (vb3, env')
			      end
			  (* The initial environment is initEnvStr if ae_term
			   * has not been evaluated, otherwith the environment
			   * in which it has been evaluated *)
			  val init_env = getOneEnvOfId ae_term lists initEnvStr
			  val (envs, le) = foldl (fn ((v, t), (envs, le)) =>
						     let val (env, le') = toEnvs (v, t) le
						     in (envs @ [env], le')
						     end)
						 ([], init_env)
						 list_sub
			  val decs = mk_term DECS_DECS "" [] envs
			  val te1  = mk_term ID_VID le [] []
			  val te2  = mk_term ATEXP_ID "" [] [te1]
			  val te3  = mk_term EXP_ATEXP "" [] [te2]
			  (* NOTE: te3 is the last env built by toEnvs *)
			  val app1 = mk_term EXP_APP "" [] [e1, ae_term]
			  val app2 = mk_term EXP_APP "" [] [app1, te3]
		      in mk_term EXP_ATEXP "" [] [mk_term ATEXP_LET "" [] [decs, app2]]
		      end
	     else term
	   | _ => raise EH.DeadBranch wrongFormat)
      | _ => updChildren term (map (fn t => transform1_4_list t lists) children)

fun transform1_4 term = transform1_4_list term ([], [], [], [])

(* TRANSFORMATION 1.5 *)

(* Transform the last returned tuple.  It also fixes the call to Evaluator1'
 * on empty_env. *)

fun transform1_5_env (term as N {kind, value, regions, children}) env =
    let val tok = toToken kind
    in if tok = DECS_DECS
       then let val (children', env') =
		    foldl (fn (dec, (decs, env)) =>
			      let val (dec', env') = transform1_5_env dec env
			      in (decs @ [dec'], env')
			      end)
			  ([], env)
			  children
	    in (updChildren term children', env')
	    end
       else if tok = DEC_VAL
       then let val (children', env') =
		    case children of
			[tyvars, bind as N {kind, value, regions, children}] =>
			if toToken kind = VALBIND_NREC
			then let val (children', env') =
				     case children of
					 [core as N {kind, value, regions, children}] =>
					 if toToken kind = ONEVALBIND_EQ
					 then let val (children', env') =
						      case children of
							  [pat as N {kind, value, regions, children}, exp] =>
							  if isExpFourAppEvaluator1' exp
							  then if toToken kind = PAT_ATPAT
							       then let val (exp', env') =
									    case children of
										[atpat as N {kind, value, regions, children}] =>
										if toToken kind = ATPAT_TUPLE
										then case children of
											 [e1, e2, e3] =>
											 let val env' = SOME e2
											 in (replaceLastIdInExpApp exp emptyEnvStr, env')
											 end
										       | _ => raise EH.DeadBranch unexpectedFormat
										else raise EH.DeadBranch unexpectedFormat
									      | _ => raise EH.DeadBranch wrongFormat
								    in ([pat, exp'], env')
								    end
							       else raise EH.DeadBranch unexpectedFormat
							  else ([pat, exp], env)
							| _ => raise EH.DeadBranch wrongFormat
					      in ([updChildren core children'], env')
					      end
					 else raise EH.DeadBranch wrongFormat
				       | _ => raise EH.DeadBranch unexpectedFormat
			     in ([tyvars, updChildren bind children'], env')
			     end
			else raise EH.DeadBranch wrongFormat
		      | _ => raise EH.DeadBranch wrongFormat
	    in (updChildren term children', env')
	    end
       else (term, env)
    end

fun transform1_5_lastexp (term as N {kind, value, regions, children}) env =
    let val tok = toToken kind
    in if tok = ATEXP_LET
       then let val children' =
		    case children of
			[decs, exp] =>
			let val (decs', env') = transform1_5_env decs env
			in [decs', transform1_5_lastexp exp env']
			end
		      | _ => raise EH.DeadBranch wrongFormat
	    in updChildren term children'
	    end
       else if tok = ATEXP_TUPLE
       then let val children' =
		    case (children, env) of
			([e1, e2, e3], SOME ev) =>
			(* we should check that e2 is 'env' and that e1 is the id
			 * obtained from Evaluator1'. *)
			let val pair = mk_term ATEXP_TUPLE "" [] [e1, ev]
			    val c1   = mk_term ID_VID closure2TermFun [] []
			    val c2   = mk_term ATEXP_ID "" [] [c1]
			    val c3   = mk_term EXP_ATEXP "" [] [c2]
			    val app  = mk_term EXP_APP "" [] [c3, pair]
			in [app, e3]
			end
		      | _ => children
	    in updChildren term children'
	    end
       else if tok = EXP_ATEXP
       then let val children' =
		    case children of
			[atexp] => [transform1_5_lastexp atexp env]
		      | _ => raise EH.DeadBranch wrongFormat
	    in updChildren term children'
	    end
       else raise EH.DeadBranch unexpectedFormat
    end

fun transform1_5 (term as N {kind, value, regions, children}) =
    let val tok = toToken kind
    in if tok = DECS_DECS
       then let val (start, last) = case List.rev children of
					x :: xs => (xs, x)
				      | _ => raise Empty
	    in updChildren term (start @ [transform1_5 last])
	    end handle Empty => term
       else if tok = DEC_FVAL
       then let val children' =
		    case children of
			[tyvars, fvalbind as N {kind, value, regions, children}] =>
			if toToken kind = FVALBIND_BIND
			then let val children' =
				     case children of
					 [afun as N {kind, value, regions, children}] =>
					 if toToken kind = ONEFVALBIND_BRANCHES
					 then let val children' =
						      case children of
							  [abranch as N {kind, value, regions, children}] =>
							  if toToken kind = BRANCHFVALBIND_BRANCH
							  then let val children' =
								       case children of
									   [pat, exp] => [pat, transform1_5_lastexp exp NONE]
									 | _ => raise EH.DeadBranch wrongFormat
							       in [updChildren abranch children']
							       end
							  else raise EH.DeadBranch wrongFormat
							| _ => raise EH.DeadBranch unexpectedFormat
					      in [updChildren afun children']
					      end
					 else raise EH.DeadBranch wrongFormat
				       | _ => raise EH.DeadBranch unexpectedFormat
			     in [tyvars, updChildren fvalbind children']
			     end
			else raise EH.DeadBranch wrongFormat
		      | _ => raise EH.DeadBranch unexpectedFormat
	    in updChildren term children'
	    end
       else updChildren term (map transform1_5 children)
    end

(* TRANSFORMATION 1.6 *)

(* This transformation changes the occurrences of Evaluator1 into Evaluator2
 * (as a prefix). *)

fun transform1_6 (term as N {kind, value, regions, children}) =
    if toToken kind = ID_VID
    then if String.isPrefix evaluator1Fun value
	 then let val b = String.size evaluator1Fun
		  val e = String.size value
		  val v = String.substring (value, b, e - b)
	      in updValue term (evaluator2Fun ^ v)
	      end
	 else term
    else updChildren term (map transform1_6 children)

(* TRANSFORMATION 1.7 *)

(* This transformation is specialized for the variable case.
 * It allows looking up an environment. *)

fun transform1_7 (term as N {kind, value, regions, children}) =
    if toToken kind = EXP_ITE
    then let val children' =
		 case children of
		     [e1, e2, e3] =>
		     if expTestEqVariable e1
		     then let val lkup   = idToExp lookupFun
			      val env    = idToAtExp initEnvStr
			      val destv  = idToExp destVariableFun
			      val term   = idToAtExp initTermStr
			      val app1   = mk_term EXP_APP "" [] [destv, term]
			      val arg    = mk_term ATEXP_PAREN "" [] [app1]
			      val app2   = mk_term EXP_APP "" [] [lkup, env]
			      val app3   = mk_term EXP_APP "" [] [app2, arg]
			      (* We've got the all expression in declaration *)
			      val apat   = idToPat "a"
			      val epat   = idToPat "e"
			      val pair1  = mk_term ATPAT_TUPLE "" [] [apat, epat]
			      val pair2  = mk_term PAT_ATPAT "" [] [pair1]
			      (* We've got the all pattern in declaration *)
			      val dec1   = mk_term ONEVALBIND_EQ "" [] [pair2, app3]
			      val dec2   = mk_term VALBIND_NREC "" [] [dec1]
			      val tyvars = mk_term TYPEVARSEQ_EM "" [] []
			      val dec3   = mk_term DEC_VAL "" [] [tyvars, dec2]
			      (* We've got the declaration in let expression *)
			      val eval   = idToExp evalFun
			      val steps  = idToAtExp initStepsStr (* NOTE: mmm, this is actually a bound variable *)
			      val aexp   = idToAtExp "a"
			      val eexp   = idToAtExp "e"
			      val eapp1  = mk_term EXP_APP "" [] [eval, steps]
			      val eapp2  = mk_term EXP_APP "" [] [eapp1, aexp]
			      val eapp3  = mk_term EXP_APP "" [] [eapp2, eexp]
			      (* We've got the entire in expression *)
			      val decs   = mk_term DECS_DECS ""  [] [dec3]
			      val lexp1  = mk_term ATEXP_LET "" [] [decs, eapp3]
			      val lexp2  = mk_term EXP_ATEXP "" [] [lexp1]
			      (* We've got the whole let-expression *)
			      val matpat = mk_term ATPAT_WILD "" [] []
			      val mpat   = mk_term PAT_ATPAT "" [] [matpat]
			      val mrule  = mk_term MRULE_M "" [] [mpat, e2]
			      val match  = mk_term MATCH_M "" [] [mrule]
			      val hdl    = mk_term EXP_HANDLE "" [] [lexp2, match]
			  in [e1, hdl, e3]
			  end
		     else map transform1_7 children
		   | _ => raise EH.DeadBranch ""
	 in updChildren term children'
	 end
    else updChildren term (map transform1_7 children)

(* TRANSFORMATION 1 *)

val transform1 =
    transform1_7 o
    transform1_6 o
    transform1_5 o
    transform1_4 o
    transform1_3 o
    transform1_2 o
    transform1_1

(* - We also have to add the definitions of the functions we are
 * using, such as closure2term, add_env_binding, empty_env,
 * and lookup_binding_env.
 * Is that the complete list? *)

(* TRANSFORMATION 2.1 *)

fun transform2_1_val n (term as N {kind, value, regions, children = ch}) =
    if toToken kind = DEC_VAL
    then case ch of
	     [_, bind as N {kind, value, regions, children}] =>
	     if toToken kind = VALBIND_NREC
	     then case children of
		      [core as N {kind, value, regions, children}] =>
		      if toToken kind = ONEVALBIND_EQ
		      then case children of
			       [pat, exp as N {kind, value, regions, children}] =>
			       if isExpThreeAppEval exp
			       then let val (p1, p2, p3) = destructPatternEvalThree pat
					val (exp', steps) =
					    case children of
						[e, ae] =>
						(* ae should be 'env'
						 * We're now going to build the a new atexp
						 * which is a continuation instead of an environment
						 * and we'll apply e to this new continuation. *)
						let fun appF m fapp =
							if m <= n
							then let val mst  = Int.toString m
								 val vm   = idToAtExp ("v" ^ mst)
								 val em   = idToAtExp ("env" ^ mst)
								 val app1 = mk_term EXP_APP "" [] [fapp, vm]
								 val app2 = mk_term EXP_APP "" [] [app1, em]
							     in if m = n
								then let val sm = idToAtExp ("steps" ^ mst)
								     in mk_term EXP_APP "" [] [app2, sm]
								     end
								else appF (m + 1) app2
							     end
							else raise EH.DeadBranch ""
						    val nst   = Int.toString n
						    val vpat  = idToPat ("v" ^ nst)
						    val epat  = idToPat ("env" ^ nst)
						    val spat  = idToPat ("steps" ^ nst)
						    val pat1  = mk_term ATPAT_TUPLE "" [] [vpat, epat, spat]
						    val pat2  = mk_term PAT_ATPAT "" [] [pat1]
						    val fid   = idToExp "f"
						    val fapp  = appF 1 fid
						    val mrule = mk_term MRULE_M "" [] [pat2, fapp]
						    val match = mk_term MATCH_M "" [] [mrule]
						    val fnexp = mk_term EXP_FN "" [] [match]
						    val par   = mk_term ATEXP_PAREN "" [] [fnexp]
						    (* steps is the steps identifier to which 'Eval' in 'e' is applied to *)
						    val steps = case e of
								    N {kind, value, regions, children} =>
								    case children of
									[e as N {kind, value, regions, children}, ae] => (* e = Eval steps *)
									(case children of
									     [e, ae as N {kind, value, regions, children}] =>
									     (if toToken kind = ATEXP_ID
									      then case children of
										       [id] => id
										     | _ => raise EH.DeadBranch wrongFormat
									      else raise EH.DeadBranch unexpectedFormat)
									   | _ => raise EH.DeadBranch wrongFormat)
								      | _ => raise EH.DeadBranch wrongFormat
						in (mk_term EXP_APP "" [] [e, par], steps)
						end
					      | _ => raise EH.DeadBranch wrongFormat
					(* Now that we have exp' and steps, we can build the new declaration.
					 * exp' is our new eval-app expression applied to a continuation instead of an environment. *)
					fun appF m fapp =
					    if m = n
					    then fapp
					    else if m < n
					    then let val mst = Int.toString m
						     val vm1 = idToAtPat ("v" ^ mst)
						     val vm2 = mk_term FATPAT_PAT "" [] [vm1]
						     val em1 = idToAtPat ("env" ^ mst)
						     val em2 = mk_term FATPAT_PAT "" [] [em1]
						     val fm1 = mk_term FMATCH_APP "" [] [fapp, vm2]
						     val fm2 = mk_term FMATCH_APP "" [] [fm1, em2]
						 in appF (m + 1) fm2
						 end
					    else raise EH.DeadBranch ""
					val farg1   = idToAtPat "f"
					val farg2   = mk_term FATPAT_PAT "" [] [farg1]
					val fid     = mk_term ID_VID ("F" ^ Int.toString n) [] []
					val fapp    = mk_term FMATCH_ID "" [] [fid, farg2]
					val fmatch1 = appF 1 fapp
					val sm      = mk_term FATPAT_PAT "" [] [steps]
					val fmatch2 = mk_term FMATCH_APP "" [] [fmatch1, sm]
					val branch  = mk_term BRANCHFVALBIND_BRANCH "" [] [fmatch2, exp']
					val func    = mk_term ONEFVALBIND_BRANCHES "" [] [branch]
					val bind    = mk_term FVALBIND_BIND "" [] [func]
					val tyvars  = mk_term TYPEVARSEQ_EM "" [] []
	    			    in (mk_term DEC_FVAL "" [] [tyvars, bind], n + 1)
				    end
			       else (updChildren term (map (fn x => #1 (transform2_1_n n x)) ch), n)
			     | _ => raise EH.DeadBranch wrongFormat
		      else raise EH.DeadBranch wrongFormat
		    | _ => (updChildren term (map (fn x => #1 (transform2_1_n n x)) ch), n)
	     else (updChildren term (map (fn x => #1 (transform2_1_n n x)) ch), n)
	   | _ => raise EH.DeadBranch wrongFormat
    else raise EH.DeadBranch unexpectedFormat

and transform2_1_n n (term as N {kind, value, regions, children}) =
    let val tok = toToken kind
    in if toToken kind = DEC_VAL
       then transform2_1_val n term
       else if tok = DECS_DECS
       then let val (children', m) =
		    foldl (fn (dec, (decs, n)) =>
			      let val (dec', m) = transform2_1_n n dec
			      in (decs @ [dec'], m)
			      end)
			  ([], 1)
			  children
	    in (updChildren term children', m)
	    end
       else if tok = ATEXP_LET
       then let val children' =
		    case children of
			[decs, exp] =>
			let val (decs', m) = transform2_1_n n decs
			    val (exp', _) = transform2_1_n m exp
			in [decs', exp']
			end
		      | _ => raise EH.DeadBranch wrongFormat
	    in (updChildren term children', n)
	    end
       else (updChildren term (map (fn x => #1 (transform2_1_n n x)) children), n)
    end

and transform2_1 term = #1 (transform2_1_n 1 term)

(* TRANSFORMATION 2.2 *)

(* This transformation changes the occurrences of Evaluator2 into Evaluator3
 * (as a prefix). *)

fun transform2_2 (term as N {kind, value, regions, children}) =
    if toToken kind = ID_VID
    then if String.isPrefix evaluator2Fun value
	 then let val b = String.size evaluator2Fun
		  val e = String.size value
		  val v = String.substring (value, b, e - b)
	      in updValue term (evaluator3Fun ^ v)
	      end
	 else term
    else updChildren term (map transform2_2 children)

val transform2 = transform2_2 o
		 transform2_1

(* We still have to change the in part of the let-expressions.
 * when checking the in parts, we can check that they don't contain
 * declaration of the form 'pat = Eval exp ...'. *)

end
