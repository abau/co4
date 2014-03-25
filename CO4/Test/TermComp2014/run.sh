#!/bin/bash

if [ $# -lt 4 ]
then
  echo "Syntax: $0 CMD TIMEOUT UPPER_BITWIDTH NUM_PRECEDENCES [FILES] ..."
  exit 1
fi
  
CMD=$1
TIMEOUT=$2
UPPER_BITWIDTH=$3
NUM_PRECEDENCES=$4

shift 4

while [ $# -gt 0 ]
do
  FILE=$1
  LOG_FILE=$(basename ${FILE}).log

  for BITWIDTH in $(seq 0 ${UPPER_BITWIDTH})
  do
    echo Solving ${CMD} ${BITWIDTH} ${NUM_PRECEDENCES} ${FILE}

    timeout --signal=SIGKILL ${TIMEOUT} ${CMD} ${BITWIDTH} ${NUM_PRECEDENCES} ${FILE} &> ${LOG_FILE}
    if [ $? -eq 0 ]
    then
      echo Terminates
      break
    fi
  done
  shift 1
done
