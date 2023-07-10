file=test11
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${BUILDDIR}/${file} > ${output} 2>&1

