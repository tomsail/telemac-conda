#!/bin/sh
file=Unittest_MEDstructElement_11
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${EXECDIR}/${file} > ${output} 2>&1

