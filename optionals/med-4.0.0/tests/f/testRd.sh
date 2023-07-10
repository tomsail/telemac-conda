file=`echo ${0##*/} | sed 's/\(^.*\)_Rd\.sh$/\1/'`
#basename n'est pas forcément disponible sur tous les unix
#file=basename $0 | sed 's/\(^.*\)\.sh$/\1/'
usefilter="no"
case $file in
    test3)          filter="sed -e 's/^[ ]*Nom universel : .*$//g'" && usefilter="yes";;
#   test11_f32)     file=test11 && optfile=test10_f32.med ;;
    test11|test11b) filter="sed -e 's/Type du champ :.*//g'" && usefilter="yes";;
    test11_*) filter="sed -e 's/Type du champ :.*//g'" && usefilter="yes"
	      optfile=test10_`echo ${file} | sed 's/^test11_\(.*\)$/\1/'`.med && file=test11;;
    test13)   optfile=test12.med ;;
    test15)   optfile=test14.med ;;
    # test30)    optfile=test29.med ;;
    # test32)    optfile=test31.med ;;
    test33) filter="sed -e 's/^.*\.c.*\[[0-9]*\] :\(.*\)$/\1/g'" && usefilter="yes";;
    *)
        optfile="";;
esac
#Suivant le compilateur fortran utiliser l'affichage des blancs est différent
#nous les enlevons
test "x$usefilter" = "xno" && usefilter="yes" && filter="sed"
filter="$filter -e 's/  //g'"
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}_${optfile}.out
test "x${DIFFOUTPUT}" = "xyes" && test "x${usefilter}" = "xyes" && \
    eval ${filter} ${SRCDIR}/dumps.ref/${output} > ${BUILDDIR}/${output}.ref
if test "x${DIFFOUTPUT}" = "xyes"; then
    ${CHECKER} ${EXECDIR}/${file} ${optfile} > ${output} 2>&1 && (
	if test "x${usefilter}" = "xno"; then diff ${BUILDDIR}/${output} ${SRCDIR}/dumps.ref/${output}; exit; fi
	( eval ${filter} ${BUILDDIR}/${output} > ${BUILDDIR}/${output}.tmp
	) &&  diff ${BUILDDIR}/${output}.tmp ${BUILDDIR}/${output}.ref; exit
    )
else
    ${CHECKER} ${EXECDIR}/${file} ${optfile} > ${output} 2>&1
fi
