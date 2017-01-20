#!/bin/bash

DIR=`pwd`

FILE_23=robust-simple-consensus-v4.esh
FILE_23_NTERM=two-thirds-terms
CONF_23=conf_sc2.emlc

#FILE_PAX=paxos-v11-part1.esh
#FILE_PAX_NTERM=pax_opt3.prog
FILE_PAX_NTERM=../examples/paxos/paxos_nuprl_terms.prog
CONF_PAX=../examples/paxos/conf_paxos.emlc

# FILE_CHRONOS=paxos-v11-inc.esh
# FILE_CHRONOS_NTERM=chronos-terms
# CONF_CHRONOS=conf_chronos_inc.emlc

#FILE_ANERIS=aneris-control.esh
#FILE_ANERIS_NTERM=../examples/aneris/aneris_nuprl_terms.prog
FILE_ANERIS_NTERM=aneris_fastrep2_opt4.prog
#FILE_ANERIS_NTERM=aneris_opt1.prog
#FILE_ANERIS_NTERM=aneris_nuprl_terms.prog
CONF_ANERIS=../examples/aneris/conf_aneris.emlc
#CONF_ANERIS_LOCAL=../examples/aneris/conf_aneris_local.emlc
CONF_ANERIS_LOCAL=conf_aneris_local.emlc

FILE_ANERIS_BATCHING_NTERM=aneris-batching-control2.prog
#FILE_ANERIS_BATCHING_NTERM=aneris_batch_completely_unfolded.prog
CONF_ANERIS_BATCHING_LOCAL=conf_aneris_batching.emlc

FILE_ANERIS_BATCHING_OPT_NTERM=aneris-batching-opt.prog
CONF_ANERIS_BATCHING_OPT_LOCAL=conf_aneris_batching_opt.emlc

ELIB=../lib/alldefs.eml
#NLIB=../lib/alldefs
NLIB=alldefs
TERMINAL=gnome-terminal
EVENTML="nice -19 ../bin/eventml"
EVENTML_OCAML=../bin/eventml_ocaml
EMLWINTITLE=runeml

function twothirds(){
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p1; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id l1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p2; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id l2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p3 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p3; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id l3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p4 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p4; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id l4 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p4"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c1 --hide-menubar --geometry 50x20+0+340    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c1; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id client1 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c2 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c2; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id client2 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c3 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c3; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id client3 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c4 --hide-menubar --geometry 50x20+1050+340 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c4; ${EVENTML} ${FILE_23} --conf ${CONF_23} --id client4 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c4"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_23} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_23} --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function twothirdsO(){
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p1; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id l1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p2; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id l2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p3 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p3; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id l3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}p4 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p4; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id l4 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}p4"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c1 --hide-menubar --geometry 50x20+0+340    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c1; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id client1 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c2 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c2; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id client2 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c3 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c3; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id client3 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}c4 --hide-menubar --geometry 50x20+1050+340 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c4; ${EVENTML_OCAML} -i ${FILE_23_NTERM} --conf ${CONF_23} --id client4 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}c4"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML_OCAML} --conf ${CONF_23} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML_OCAML} --conf ${CONF_23} --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function paxos(){
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id ldr1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id ldr2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id acc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id acc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id acc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id rep1 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML} ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id rep2 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_PAX} --nuprl-defs ${NLIB} --gc --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_PAX} --nuprl-defs ${NLIB} --gc --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function paxosO(){
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id ldr1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id ldr2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id acc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id acc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id acc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id rep1 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML_OCAML} -i ${FILE_PAX_NTERM} --conf ${CONF_PAX} --id rep2 --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML_OCAML} --conf ${CONF_PAX} --nuprl-defs ${NLIB} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML_OCAML} --conf ${CONF_PAX} --nuprl-defs ${NLIB} --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

# function chronos(){
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id ldr1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id ldr2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id acc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id acc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id acc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc4 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id rep1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id rep2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id client --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_CHRONOS} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_CHRONOS} --nuprl-defs ${NLIB} --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
# }

# function nchronos(){
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id ldr1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id ldr2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id acc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id acc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id acc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id loc4 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id rep1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id rep2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML} ${FILE_CHRONOS} --conf ${CONF_CHRONOS} --id client --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_CHRONOS} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_CHRONOS} --nuprl-defs ${NLIB} --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
# }

# function nchronosO(){
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id ldr1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id ldr2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id acc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id acc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id acc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id loc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id loc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id loc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id loc4 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id rep1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id rep2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML_OCAML} -i ${FILE_CHRONOS_NTERM} --conf ${CONF_CHRONOS} --id client --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML_OCAML} --conf ${CONF_CHRONOS} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
#     ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML_OCAML} --conf ${CONF_CHRONOS} --nuprl-defs ${NLIB} --send;   bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
# }

function aneris(){
    ## Leaders
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id ldr1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id ldr2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"

    ## Acceptors
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id acc1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id acc2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id acc3 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ## 2/3 replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc3 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc4 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

    ## Aneris replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id rep1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id rep2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ## Client
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id client --other --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

    ## Receiver
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_ANERIS_LOCAL} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ## Sender
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_ANERIS_LOCAL} --nuprl-defs ${NLIB} --send; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function anerisO(){
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id ldr1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id ldr2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id acc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id acc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id acc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc3 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id loc4 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id rep1 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id rep2 --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML_OCAML} --i ${FILE_ANERIS_NTERM} --conf ${CONF_ANERIS_LOCAL} --id client --other --lib ${ELIB} --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML_OCAML} --conf ${CONF_ANERIS_LOCAL} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML_OCAML} --conf ${CONF_ANERIS_LOCAL} --nuprl-defs ${NLIB} --send; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function batching(){
    ## Leaders
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id ldr1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id ldr2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"

    ## Acceptors
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id acc1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id acc2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id acc3 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ## 2/3 replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id loc1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id loc2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id loc3 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id loc4 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

    ## Aneris replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id rep1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id rep2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ## Client
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML} --i ${FILE_ANERIS_BATCHING_NTERM} --conf ${CONF_ANERIS_BATCHING_LOCAL} --id client1 --client2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

    ## Sender + Receiver
    ##${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_ANERIS_BATCHING_LOCAL} --client2; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ## Sender
    ##${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_ANERIS_BATCHING_LOCAL} --nuprl-defs ${NLIB} --send; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function batchingopt(){
    ## Leaders
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 50x20+0+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id ldr1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 50x20+350+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id ldr2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"

    ## Acceptors
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 50x20+700+0  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id acc1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 50x20+1050+0 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id acc2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 50x20+0+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id acc3 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ## 2/3 replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 50x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id loc1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 50x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id loc2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 50x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id loc3 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 50x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id loc4 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

    ## Aneris replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 50x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id rep1 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 50x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id rep2 --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ## Client
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl --hide-menubar --geometry 95x2+0+500 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl; ${EVENTML} --i ${FILE_ANERIS_BATCHING_OPT_NTERM} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --id client1 --other --nuprl-defs ${NLIB} --gc --extra newsock; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl"

    ## Receiver
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a1 --hide-menubar --geometry 95x2+0+690   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1; read line; ${EVENTML} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --client; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a1"
    ## Sender
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}a2 --hide-menubar --geometry 95x2+700+690 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2; read line; ${EVENTML} --conf ${CONF_ANERIS_BATCHING_OPT_LOCAL} --nuprl-defs ${NLIB} --send; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}a2"
}

function stop(){
    wmctrl -F -c ${EMLWINTITLE}p1
    wmctrl -F -c ${EMLWINTITLE}p2
    wmctrl -F -c ${EMLWINTITLE}p3
    wmctrl -F -c ${EMLWINTITLE}p4

    wmctrl -F -c ${EMLWINTITLE}c1
    wmctrl -F -c ${EMLWINTITLE}c2
    wmctrl -F -c ${EMLWINTITLE}c3
    wmctrl -F -c ${EMLWINTITLE}c4

    wmctrl -F -c ${EMLWINTITLE}ldr1
    wmctrl -F -c ${EMLWINTITLE}ldr2

    wmctrl -F -c ${EMLWINTITLE}acc1
    wmctrl -F -c ${EMLWINTITLE}acc2
    wmctrl -F -c ${EMLWINTITLE}acc3

    wmctrl -F -c ${EMLWINTITLE}loc1
    wmctrl -F -c ${EMLWINTITLE}loc2
    wmctrl -F -c ${EMLWINTITLE}loc3
    wmctrl -F -c ${EMLWINTITLE}loc4

    wmctrl -F -c ${EMLWINTITLE}rep1
    wmctrl -F -c ${EMLWINTITLE}rep2

    wmctrl -F -c ${EMLWINTITLE}cl

    wmctrl -F -c ${EMLWINTITLE}a1
    wmctrl -F -c ${EMLWINTITLE}a2

}

case $1 in
    batching)
	batching
	;;
    batchingopt)
	batchingopt
	;;
    aneris)
	aneris
	;;
    anerisO)
	anerisO
	;;
    twothirds)
	twothirds
	;;
    paxos)
	paxos
	;;
    # chronos)
    # 	chronos
    # 	;;
    # nchronos)
    # 	nchronos
    # 	;;
    # nchronosO)
    # 	nchronosO
    # 	;;
    twothirdsO)
	twothirdsO
	;;
    paxosO)
	paxosO
	;;
    stop)
	stop
	;;
    *)
	echo "The only options are twothirds and stop"
	;;
esac
