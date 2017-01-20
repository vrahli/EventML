#!/bin/bash
TERMINAL=gnome-terminal
WINTITLE="runscala"
DIR=`pwd`
SCALA=scala
RUN_SCALA_FILE=run.scala
RUN_SCALA=${DIR}/${RUN_SCALA_FILE}

FILE=Config.scala
CONF_FILE=conf_aneris_local.emlc

rm *.class
echo "compiling ${FILE}"
scalac ${FILE}
echo "executing ${FILE}"
# Run acceptors
${TERMINAL} --title=acc1 -x scala "Config" acc1 ${CONF_FILE}
${TERMINAL} --title=acc2 -x scala "Config" acc2 ${CONF_FILE}
${TERMINAL} --title=acc3 -x scala "Config" acc3 ${CONF_FILE}

# Run leaders
${TERMINAL} --title=ldr1 -x scala "Config" ldr1 ${CONF_FILE}
${TERMINAL} --title=ldr2 -x scala "Config" ldr2 ${CONF_FILE}

# Run two_thirds
${TERMINAL} --title=loc1 -x scala "Config" loc1 ${CONF_FILE}
${TERMINAL} --title=loc2 -x scala "Config" loc2 ${CONF_FILE}
${TERMINAL} --title=loc3 -x scala "Config" loc3 ${CONF_FILE}
${TERMINAL} --title=loc4 -x scala "Config" loc4 ${CONF_FILE}

# Run replicas
${TERMINAL} --title=rep1 -x scala "Config" rep1 ${CONF_FILE}
${TERMINAL} --title=rep2 -x scala "Config" rep2 ${CONF_FILE}

# Run clients
${TERMINAL} --title=client1 -x scala "Config" client1 ${CONF_FILE}

