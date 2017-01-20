#!/bin/bash

EML=../bin/eventml
ALLDEFS=alldefs
ALLDEFS_EML=alldefs.eml
EML_FILE=paxos-list.esh
PROG_FILE=paxos_list_nuprl_terms.prog
LISP_FILE=paxos-list.lisp
CONF_FILE=conf_paxos.emlc

# we generate the .prog file from the .eml file
${EML} --i ${EML_FILE} --lib ${ALLDEFS_EML} --nuprl-defs ${ALLDEFS} --nuprl --extra dump
# the .prog file was dumped in a temporary file, we rename it
cp output-term-0.800 ${PROG_FILE}
# we generate the .lisp file from the .prog file (what's the false for?)
${EML} --i ${PROG_FILE} --simul --conf ${CONF_FILE} --nuprl-defs ${ALLDEFS} --extra lisp:false
# the .lisp file was dumped in a temporary file, we rename it
cp program-365-12-31-0.740.lisp ${LISP_FILE}
