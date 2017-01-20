# Copyright 2011 Cornell University
# Copyright 2012 Cornell University
# Copyright 2013 Cornell University
#
#
# This file is part of EventML - a tool aiming at specifying
# distributed protocols in an ML like language.  It is an interface
# to the logic of events and is compiled into Nuprl.  It is written
# by the NUPRL group of Cornell University, Ithaca, NY.
#
# EventML is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# EventML is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EventML.  If not, see <http://www.gnu.org/licenses/>.
#
#  o Authors:     Vincent Rahli
#  o Affiliation: Cornell University, NUPRL group
#  o Date:        20 May 2011
#  o File name:   Makefile
#  o Description: .
#


SHELL=/bin/bash
PARSE_FILE=EML
NUPRL_ASCII_FILE=NuprlAscii
CONFIG_PARSE_FILE=Config
EVENTML_BIN_DIR=../bin
EVENTML_LIB_DIR=../lib
LOCAL_BIN_DIR=bin
DATE1=`date +%Y-%m-%d`
DATE2=`date`
TIME=`date +%T`
HOST=`hostname`
TMPDIR=/tmp
PARSE=parse
EXPORT=export
TERMS=../terms
EVALUATORS=../evaluators
BIN_NAME=eventml
PROFKIND=time
ARCH=x86
MLTON=mlton
MLLEX=mllex
MLYACC=mlyacc
SPEC_FILE=foobar

VERSION=0.4

default:
	# you can:
	#   - generate a mlton binary: bin
	#   - clean the directory:     clean

prog:
	make lib
	cp ${EVENTML_LIB_DIR}/alldefs.eml .
	# --------- Generating .prog file
	${EVENTML_BIN_DIR}/${BIN_NAME} --i ${SPEC_FILE}.esh --nuprl --lib alldefs.eml --extra dump --file1 ${SPEC_FILE}.prog

lisp:
	# --------- Generating .prog file
	${EVENTML_BIN_DIR}/${BIN_NAME} --i ${SPEC_FILE}.esh --nuprl --lib alldefs.eml --extra dump --file1 ${SPEC_FILE}.prog
	# --------- Generating .lisp file
	${EVENTML_BIN_DIR}/${BIN_NAME} --i ${SPEC_FILE}.prog --o ${SPEC_FILE}.lisp --tolisp --nuprl-defs alldefs

clean-parse :
	rm -f ${PARSE}/*.grm.sml  ${PARSE}/*.grm.desc  ${PARSE}/*.grm.sig  ${PARSE}/*.lex.sml
	rm -f ${TERMS}/*.grm.sml  ${TERMS}/*.grm.desc  ${TERMS}/*.grm.sig  ${TERMS}/*.lex.sml
	rm -f ${EXPORT}/*.grm.sml ${EXPORT}/*.grm.desc ${EXPORT}/*.grm.sig ${EXPORT}/*.lex.sml

clean: clean-parse
	rm -Rf .cm mlton-control.sml

# ultra clean
uclean: clean
	rm -Rf alldefs*
	rm -Rf *~
	rm -Rf test*
	rm -Rf tmp*
	rm -Rf Paxos_*
	rm -Rf mlmon.out
	rm -Rf ${BIN_NAME}

bin/cmtotes: PGOps.sml
	make cmtotes

bin/cmtodot: PGOps.sml
	make cmtodot

## cmtotes generates a cmtotes binary to transform a cm file into a tes file.
cmtotes:
	# creates the heap image file
	@echo "use \"PGOps.sml\"; open SmlTesDeps; SMLofNJ.exportFn (\"${TMPDIR}/cmtotes\", cmToTes);" | sml
	# creates the normal executable
	heap2exec ${TMPDIR}/cmtotes.x86-linux ${TMPDIR}/cmtotes
	# makes implementation directory
	mkdir -p bin
	# copies the executable to the implementation directory
	mv ${TMPDIR}/cmtotes bin/cmtotes

## cmtodot generates a cmtodot binary to transform a cm file into a dot file.
cmtodot:
	# creates the heap image file
	@echo "use \"PGOps.sml\"; open SmlTesDeps; SMLofNJ.exportFn (\"${TMPDIR}/cmtodot\", cmToDot);" | sml
	# creates the normal executable
	heap2exec ${TMPDIR}/cmtodot.x86-linux ${TMPDIR}/cmtodot
	# makes implementation directory
	mkdir -p bin
	# copies the executable to the implementation directory
	mv ${TMPDIR}/cmtodot bin/cmtodot

### cmtomlb generates a .mlb file from a .cm file.
cmtomlb:
	./cmtomlb.sh

mlton-base:
	${MLLEX}  ${PARSE}/${PARSE_FILE}.lex
	${MLYACC} ${PARSE}/${PARSE_FILE}.grm
	${MLLEX}  ${TERMS}/${NUPRL_ASCII_FILE}.lex
	${MLYACC} ${TERMS}/${NUPRL_ASCII_FILE}.grm
	${MLLEX}  ${EXPORT}/${CONFIG_PARSE_FILE}.lex
	${MLYACC} ${EXPORT}/${CONFIG_PARSE_FILE}.grm
	cp mlton-control.sml.in mlton-control.sml

mlton-bin: mlton-base
	echo -e "val _ = print \"EventML version ${VERSION} (built ${DATE2} on ${HOST} using MLton)\\\n\\\n\";" >> mlton-control.sml
	echo "val _ = slice ();" >> mlton-control.sml
	mkdir -p ${EVENTML_BIN_DIR}
	#nice --adjustment=19 mlton -verbose 1 -output ${EVENTML_BIN_DIR}/${BIN_NAME} sources.mlb
	${MLTON} -verbose 1 -output ${EVENTML_BIN_DIR}/${BIN_NAME} sources.mlb
	make clean-parse

smlnj-bin:
	cp mlton-control.sml.in mlton-control.sml
	echo "fun prt () = print \"EventML version ${VERSION} (built ${DATE2} on ${HOST} using SML/NJ)\\\\n\\\\n\";" >> mlton-control.sml
	echo "fun slice x = (prt (); slicerArgs x; OS.Process.success);" >> mlton-control.sml
	@echo "CM.make \"sources.cm\"; use \"mlton-control.sml\"; SMLofNJ.exportFn (\"${TMPDIR}/${BIN_NAME}\", slice);" | sml
	mkdir -p ${EVENTML_BIN_DIR}
	heap2exec ${TMPDIR}/${BIN_NAME}.${ARCH}-linux ${EVENTML_BIN_DIR}/${BIN_NAME}

polyml-bin:
	./cmtomlb.sh cmtopoly
	poly < FILES.sml
	cc -o slicer-poly slicer-poly.o
	rm slicer-poly.o
	mv slicer-poly bin/slicer-poly

bin: mlton-bin

all-bin: mlton-bin ocaml

paxos: mlton-base
	echo -e "val _ = print \"EventML version ${VERSION} (built ${DATE2} on ${HOST} using MLton)\\\n\\\n\";" >> mlton-control.sml
	echo "val _ = paxos ();" >> mlton-control.sml
	mkdir -p ${EVENTML_BIN_DIR}
	#nice --adjustment=19 mlton -verbose 1 -output ${EVENTML_BIN_DIR}/${BIN_NAME} sources.mlb
	${MLTON} -verbose 1 -output ${EVENTML_BIN_DIR}/paxos sources.mlb
	make clean-parse

.PHONY: lib
lib:
	(cd ${EVENTML_LIB_DIR}; tar -xvzf alldefs.tar.gz 2>/dev/null)
	${EVENTML_BIN_DIR}/${BIN_NAME} "${EVENTML_LIB_DIR}/alldefs" --ascii -o "${EVENTML_LIB_DIR}/alldefs.eml"

${TERMS}/nuprlTerms.cmi: ${TERMS}/nuprlTerms.mli
	ocamlfind ocamlc -package batteries -c \
			${TERMS}/nuprlTerms.mli

${TERMS}/nuprlTerms.cmo: ${TERMS}/nuprlTerms.ml \
			 ${TERMS}/nuprlTerms.cmi
	ocamlfind ocamlc -package batteries -c -I ${TERMS} \
			${TERMS}/nuprlTerms.ml

${TERMS}/nuprlTerms.cmx: ${TERMS}/nuprlTerms.ml
	ocamlfind ocamlopt -package batteries -c -I ${TERMS} \
			${TERMS}/nuprlTerms.mli \
			${TERMS}/nuprlTerms.ml

${EVALUATORS}/sml/tools.cmi: ${EVALUATORS}/sml/tools.mli
	ocamlfind ocamlc -package batteries -c \
			${EVALUATORS}/sml/tools.mli

${EVALUATORS}/sml/tools.cmo: ${EVALUATORS}/sml/tools.ml \
			     ${EVALUATORS}/sml/tools.cmi
	ocamlfind ocamlc -package batteries -c -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/tools.ml

${EVALUATORS}/sml/tools.cmx: ${EVALUATORS}/sml/tools.ml
	ocamlfind ocamlopt -package batteries -c -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/tools.mli \
			${EVALUATORS}/sml/tools.ml

${EVALUATORS}/sml/monad.cmi: ${EVALUATORS}/sml/monad.mli
	ocamlfind ocamlc -package batteries -c \
			${EVALUATORS}/sml/monad.mli

${EVALUATORS}/sml/monad.cmo: ${EVALUATORS}/sml/monad.ml \
			     ${EVALUATORS}/sml/monad.cmi \
			     ${TERMS}/nuprlTerms.cmi \
			     ${TERMS}/nuprlTerms.cmo \
			     ${EVALUATORS}/sml/tools.cmi \
			     ${EVALUATORS}/sml/tools.cmo
	ocamlfind ocamlc -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/monad.ml

${EVALUATORS}/sml/monad.cmx: ${EVALUATORS}/sml/monad.ml \
			     ${TERMS}/nuprlTerms.cmi \
			     ${TERMS}/nuprlTerms.cmx \
			     ${EVALUATORS}/sml/tools.cmi \
			     ${EVALUATORS}/sml/tools.cmx
	ocamlfind ocamlopt -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/monad.mli \
			${EVALUATORS}/sml/monad.ml

${EVALUATORS}/sml/primitive.cmi: ${EVALUATORS}/sml/primitive.mli \
				 ${TERMS}/nuprlTerms.cmi
	ocamlfind ocamlc -package batteries -c -I ${TERMS} \
			${EVALUATORS}/sml/primitive.mli

${EVALUATORS}/sml/primitive.cmo: ${EVALUATORS}/sml/primitive.ml \
				 ${EVALUATORS}/sml/primitive.cmi \
				 ${TERMS}/nuprlTerms.cmi \
			     	 ${TERMS}/nuprlTerms.cmo \
			     	 ${EVALUATORS}/sml/tools.cmi \
			     	 ${EVALUATORS}/sml/tools.cmo
	ocamlfind ocamlc -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/primitive.ml

${EVALUATORS}/sml/primitive.cmx: ${EVALUATORS}/sml/primitive.ml \
				 ${TERMS}/nuprlTerms.cmi \
			     	 ${TERMS}/nuprlTerms.cmx \
			     	 ${EVALUATORS}/sml/tools.cmi \
			     	 ${EVALUATORS}/sml/tools.cmx
	ocamlfind ocamlopt -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/primitive.mli \
			${EVALUATORS}/sml/primitive.ml

${EVALUATORS}/sml/evalProperties.cmi: ${EVALUATORS}/sml/evalProperties.mli \
				      ${TERMS}/nuprlTerms.cmi
	ocamlfind ocamlc -package batteries -c -I ${TERMS} \
			${EVALUATORS}/sml/evalProperties.mli

${EVALUATORS}/sml/evalProperties.cmo: ${EVALUATORS}/sml/evalProperties.ml \
				      ${EVALUATORS}/sml/evalProperties.cmi \
				      ${TERMS}/nuprlTerms.cmi \
			     	      ${TERMS}/nuprlTerms.cmo
	ocamlfind ocamlc -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/evalProperties.ml

${EVALUATORS}/sml/evalProperties.cmx: ${EVALUATORS}/sml/evalProperties.ml \
				      ${TERMS}/nuprlTerms.cmi \
			     	      ${TERMS}/nuprlTerms.cmx
	ocamlfind ocamlopt -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/evalProperties.mli \
			${EVALUATORS}/sml/evalProperties.ml

${EVALUATORS}/sml/evaluator2.cmo: ${EVALUATORS}/sml/evaluator2.ml \
				  ${TERMS}/nuprlTerms.cmi \
	       			  ${TERMS}/nuprlTerms.cmo \
			          ${EVALUATORS}/sml/tools.cmi \
			          ${EVALUATORS}/sml/tools.cmo \
			          ${EVALUATORS}/sml/monad.cmi \
				  ${EVALUATORS}/sml/monad.cmo \
			          ${EVALUATORS}/sml/primitive.cmi \
			          ${EVALUATORS}/sml/primitive.cmo \
			          ${EVALUATORS}/sml/evalProperties.cmi \
			          ${EVALUATORS}/sml/evalProperties.cmo
	# ocamc for evaluators
	ocamlfind ocamlc -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			nums.cma \
			str.cma  \
			${EVALUATORS}/sml/evaluator2.ml

${EVALUATORS}/sml/evaluator2.cmx: ${EVALUATORS}/sml/evaluator2.ml \
				  ${TERMS}/nuprlTerms.cmi \
	       			  ${TERMS}/nuprlTerms.cmx \
			          ${EVALUATORS}/sml/tools.cmi \
			          ${EVALUATORS}/sml/tools.cmx \
			          ${EVALUATORS}/sml/monad.cmi \
				  ${EVALUATORS}/sml/monad.cmx \
			          ${EVALUATORS}/sml/primitive.cmi \
			          ${EVALUATORS}/sml/primitive.cmx \
			          ${EVALUATORS}/sml/evalProperties.cmi \
			          ${EVALUATORS}/sml/evalProperties.cmx
	# ocamc for evaluators
	ocamlfind ocamlopt -package batteries -c -I ${TERMS} -I ${EVALUATORS}/sml \
			nums.cmxa \
			str.cmxa  \
			${EVALUATORS}/sml/evaluator2.ml

${TERMS}/nuprlAsciiParse.cmi: ${TERMS}/nuprlAsciiParse.mly \
			      ${TERMS}/nuprlTerms.cmi \
	       	              ${TERMS}/nuprlTerms.cmo
	(cd ${TERMS}; \
			ocamlyacc nuprlAsciiParse.mly; \
			ocamlc -c nuprlAsciiParse.mli)

${TERMS}/nuprlAsciiParse.cmo: ${TERMS}/nuprlAsciiParse.mly \
			      ${TERMS}/nuprlTerms.cmi \
	       	              ${TERMS}/nuprlTerms.cmo
	(cd ${TERMS}; \
			ocamlyacc nuprlAsciiParse.mly; \
			ocamlc -c nuprlAsciiParse.mli; \
			ocamlc -c nuprlAsciiParse.ml)

${TERMS}/nuprlAsciiParse.cmx: ${TERMS}/nuprlAsciiParse.mly \
			      ${TERMS}/nuprlTerms.cmi \
	       	              ${TERMS}/nuprlTerms.cmx
	(cd ${TERMS}; \
			ocamlyacc nuprlAsciiParse.mly; \
			ocamlopt -c nuprlAsciiParse.mli; \
			ocamlopt -c nuprlAsciiParse.ml)

${TERMS}/nuprlAsciiLex.cmo: ${TERMS}/nuprlAsciiLex.mll \
			    ${TERMS}/nuprlTerms.cmi \
	       	            ${TERMS}/nuprlTerms.cmo
	(cd ${TERMS}; \
			ocamllex nuprlAsciiLex.mll; \
			ocamlc -c nuprlAsciiLex.ml)

${TERMS}/nuprlAsciiLex.cmx: ${TERMS}/nuprlAsciiLex.mll \
			    ${TERMS}/nuprlTerms.cmi \
	       	            ${TERMS}/nuprlTerms.cmx
	(cd ${TERMS}; \
			ocamllex nuprlAsciiLex.mll; \
			ocamlopt -c nuprlAsciiLex.ml)

${TERMS}/parserNuprlAscii.cmi: ${TERMS}/parserNuprlAscii.mli \
			       ${TERMS}/nuprlTerms.cmi
	ocamlc -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${TERMS}/parserNuprlAscii.mli \

${TERMS}/parserNuprlAscii.cmo: ${TERMS}/parserNuprlAscii.ml \
			       ${TERMS}/parserNuprlAscii.cmi \
			       ${TERMS}/nuprlAsciiParse.cmo \
			       ${TERMS}/nuprlAsciiLex.cmo
	ocamlc -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${TERMS}/parserNuprlAscii.ml \

${TERMS}/parserNuprlAscii.cmx: ${TERMS}/parserNuprlAscii.ml \
			       ${TERMS}/nuprlAsciiParse.cmx \
			       ${TERMS}/nuprlAsciiLex.cmx
	ocamlopt -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${TERMS}/parserNuprlAscii.mli \
			${TERMS}/parserNuprlAscii.ml \

${EVALUATORS}/sml/evaluators.cmo: ${EVALUATORS}/sml/evaluators.ml \
				  ${TERMS}/nuprlTerms.cmi \
				  ${TERMS}/nuprlTerms.cmo \
				  ${TERMS}/parserNuprlAscii.cmi \
				  ${TERMS}/parserNuprlAscii.cmo \
			          ${EVALUATORS}/sml/evaluator2.cmo
	# ocamlc for nuprl term parser and parser interface
	ocamlc -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/evaluators.ml

${EVALUATORS}/sml/evaluators.cmx: ${EVALUATORS}/sml/evaluators.ml \
				  ${TERMS}/nuprlTerms.cmi \
				  ${TERMS}/nuprlTerms.cmx \
				  ${TERMS}/parserNuprlAscii.cmi \
				  ${TERMS}/parserNuprlAscii.cmx \
			          ${EVALUATORS}/sml/evaluator2.cmx
	# ocamlopt for nuprl term parser and parser interface
	ocamlopt -c -I ${TERMS} -I ${EVALUATORS}/sml \
			${EVALUATORS}/sml/evaluators.ml

ocaml: ${EVALUATORS}/sml/evaluators.cmo \
       ${EVALUATORS}/sml/evaluators.cmx
	# ocamlc for config parser
	(cd ${TERMS}; \
			ocamlyacc configParse.mly; \
			ocamlc -c configParse.mli; \
			ocamlc -c configParse.ml; \
			ocamllex  configLex.mll; \
			ocamlc -c configLex.ml)
	# ocamlopt for config parser
	(cd ${TERMS}; \
			ocamlyacc configParse.mly; \
			ocamlopt -c configParse.mli; \
			ocamlopt -c configParse.ml; \
			ocamllex  configLex.mll; \
			ocamlopt -c configLex.ml)
	# ocamlc for config parser interface
	ocamlc -c -I ${TERMS} \
			${TERMS}/configParser.mli \
			${TERMS}/configParser.ml
	# ocamlopt for config parser interface
	ocamlopt -c -I ${TERMS} \
			${TERMS}/configParser.mli \
			${TERMS}/configParser.ml
	# ocamlc for main interface
	ocamlc -c -I ${TERMS} -I ${EVALUATORS}/sml \
			interface.ml
	# ocamlopt for main interface
	ocamlopt -c -I ${TERMS} -I ${EVALUATORS}/sml \
			interface.ml
	# ocamlc output
	ocamlfind ocamlc -package batteries -o ${EVENTML_BIN_DIR}/eventml_ocaml_b \
			unix.cma str.cma nums.cma \
			-I ${TERMS} -I ${EVALUATORS}/sml \
			tools.cmo nuprlTerms.cmo \
			configLex.cmo configParse.cmo configParser.cmo \
			nuprlAsciiLex.cmo nuprlAsciiParse.cmo parserNuprlAscii.cmo \
			primitive.cmo evalProperties.cmo monad.cmo \
			evaluator2.cmo evaluators.cmo \
			interface.ml
	# ocamlopt output
	ocamlopt -o ${EVENTML_BIN_DIR}/eventml_ocaml \
			unix.cmxa str.cmxa nums.cmxa \
			-I ${TERMS} -I ${EVALUATORS}/sml \
			tools.cmx nuprlTerms.cmx \
			configLex.cmx configParse.cmx configParser.cmx \
			nuprlAsciiLex.cmx nuprlAsciiParse.cmx parserNuprlAscii.cmx \
			primitive.cmx evalProperties.cmx monad.cmx \
			evaluator2.cmx evaluators.cmx \
			interface.ml

ocaml-clean:
	rm -Rf \
			${TERMS}/*.cmo ${TERMS}/*.cmi ${TERMS}/*.cmx ${TERMS}/*.o \
			${EVALUATORS}/sml/*.cmo ${EVALUATORS}/sml/*.cmi ${EVALUATORS}/sml/*.cmx ${EVALUATORS}/sml/*.o \
			*.cmo *.cmi *.cmx *.o

profile-gen :
	mkdir -p ${LOCAL_BIN_DIR}
	${MLTON} -verbose 3 -profile ${PROFKIND} -profile-branch true -profile-stack true -profile-val true -output ${LOCAL_BIN_DIR}/eventml-profile sources.mlb
	${LOCAL_BIN_DIR}/eventml-profile
	mlprof -show-line true -raw true ${LOCAL_BIN_DIR}/eventml-profile mlmon.out > ${LOCAL_BIN_DIR}/eventml-profile-out-${DATE1}-${TIME}
	make clean-parse

weak-profile-gen :
	mkdir -p ${LOCAL_BIN_DIR}
	${MLTON} -verbose 3 -profile ${PROFKIND} -profile-stack true -output ${LOCAL_BIN_DIR}/eventml-profile sources.mlb
	${LOCAL_BIN_DIR}/eventml-profile
	mlprof -show-line true -raw true ${LOCAL_BIN_DIR}/eventml-profile mlmon.out > ${LOCAL_BIN_DIR}/eventml-profile-out-${DATE1}-${TIME}
	make clean-parse

profile1: mlton-base
	echo "val _ = sliceProfile1 ();" >> mlton-control.sml
	make profile-gen

profile2: mlton-base
	echo "val _ = sliceProfile2 ();" >> mlton-control.sml
	make profile-gen

profile3: mlton-base
	echo "val _ = sliceProfile3 ();" >> mlton-control.sml
	make weak-profile-gen

profile4: mlton-base
	echo "val _ = sliceProfile4 ();" >> mlton-control.sml
	make profile-gen

profile5: mlton-base
	echo "val _ = sliceProfile5 ();" >> mlton-control.sml
	make weak-profile-gen

profile6: mlton-base
	echo "val _ = sliceProfile6 ();" >> mlton-control.sml
	make weak-profile-gen

profile7: mlton-base
	echo "val _ = sliceProfile7 ();" >> mlton-control.sml
	make weak-profile-gen
