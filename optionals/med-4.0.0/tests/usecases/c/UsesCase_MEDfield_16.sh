#!/bin/sh
file=UsesCase_MEDfield_16
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${EXECDIR}/${file} > ${output} 2>&1
