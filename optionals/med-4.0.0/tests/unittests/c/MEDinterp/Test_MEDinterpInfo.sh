#!/bin/sh
file=`basename $0 | sed 's/\.sh.\{0,1\}$//g'`
she=`basename $0 | sed 's/^.*\(.\)$/\1/'`
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${EXECDIR}/${file} > ${output} 2>&1 && ( test "x${she}" = "xe" && rm ${BUILDDIR}/current.med || true )
