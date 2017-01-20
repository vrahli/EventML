#!/bin/bash

S=scala
DIR=`pwd`
TARGET=target
CLS=${TARGET}/classes
JARS=lib/scala-pickling_2.10-0.8.0-SNAPSHOT.jar
PRC=process
CONF=aneris/conf_aneris_batching.emlc
TERMINAL=gnome-terminal
EMLWINTITLE=runeml

function aneris(){
    ## Replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ${S} -classpath ${CLS}:${JARS} ${PRC}_rep1 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 100x20+350+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ${S} -classpath ${CLS}:${JARS} ${PRC}_rep2 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"

    ## Leaders
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 100x20+700+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ${S} -classpath ${CLS}:${JARS} ${PRC}_ldr1 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 100x20+1050+0   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ${S} -classpath ${CLS}:${JARS} ${PRC}_ldr2 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"

    ## Acceptors
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 100x20+0+340    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ${S} -classpath ${CLS}:${JARS} ${PRC}_acc1 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 100x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ${S} -classpath ${CLS}:${JARS} ${PRC}_acc2 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 100x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ${S} -classpath ${CLS}:${JARS} ${PRC}_acc3 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"

    ## 2/3 replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 100x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ${S} -classpath ${CLS}:${JARS} ${PRC}_loc1 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 100x20+350+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ${S} -classpath ${CLS}:${JARS} ${PRC}_loc2 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 100x20+700+800  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ${S} -classpath ${CLS}:${JARS} ${PRC}_loc3 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 100x20+1050+800 -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ${S} -classpath ${CLS}:${JARS} ${PRC}_loc4 ${CONF}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"
}

function stop(){
    wmctrl -F -c ${EMLWINTITLE}rep1
    wmctrl -F -c ${EMLWINTITLE}rep2

    wmctrl -F -c ${EMLWINTITLE}ldr1
    wmctrl -F -c ${EMLWINTITLE}ldr2

    wmctrl -F -c ${EMLWINTITLE}acc1
    wmctrl -F -c ${EMLWINTITLE}acc2
    wmctrl -F -c ${EMLWINTITLE}acc3

    wmctrl -F -c ${EMLWINTITLE}loc1
    wmctrl -F -c ${EMLWINTITLE}loc2
    wmctrl -F -c ${EMLWINTITLE}loc3
    wmctrl -F -c ${EMLWINTITLE}loc4
}

case $1 in
    aneris)
	aneris
	;;
    stop)
	stop
	;;
    *)
	echo "The only options are aneris and stop"
	;;
esac
