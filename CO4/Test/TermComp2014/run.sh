#!/bin/bash

if [ $# -lt 3 ]
then
  echo "Syntax: $0 CMD UPPER_BITWIDTH NUM_PRECEDENCES [FILES] ..."
  exit 1
fi
  
CMD=$1
UPPER_BITWIDTH=$2
NUM_PRECEDENCES=$3

shift 3

while [ $# -gt 0 ]
do
  FILE=$1
  LOG_FILE=$(basename ${FILE}).log

  for BITWIDTH in $(seq 0 ${UPPER_BITWIDTH})
  do
    echo Solving ${CMD} ${BITWIDTH} ${NUM_PRECEDENCES} ${FILE}

    ${CMD} ${BITWIDTH} ${NUM_PRECEDENCES} ${FILE} &> ${LOG_FILE}
    if [ $? -eq 0 ]
    then
      echo Terminates
      break
    fi
  done
  shift 1
done
