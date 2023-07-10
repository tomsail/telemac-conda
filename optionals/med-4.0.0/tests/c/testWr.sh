#!/bin/sh
file=`echo ${0##*/} | sed 's/\(^.*\)_Wr\.sh$/\1/'`
h5dumpArgs=""
optfile=""
#medint permet de différencier les ./configure avec--with-med_int=int|long
medint=""
case $file in
       test2)  h5dumpArgs="-a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP -g /ENS_MAA/maa2 -g /ENS_MAA/maa3 -g /INFOS_GENERALES" 
               test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
       ;;
       test10*)
	       medint="${MEDINT}"
               test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
       ;;
       test20) 
               ${CP} ${BUILDDIR}/test2.med ${BUILDDIR}/test20-0.med && chmod 644 ${BUILDDIR}/test20-0.med 
               ${CP} ${SRCDIR}/dumps.ref/test10-med.hdf ${BUILDDIR}/test10-med.hdf && chmod 644 ${BUILDDIR}/test10-med.hdf
               ${CP} ${SRCDIR}/dumps.ref/test2-med.hdf ${BUILDDIR}/test2-med.hdf && chmod 644 ${BUILDDIR}/test2-med.hdf
               test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
       ;;
       test31)
               ${CP} ${BUILDDIR}/test14.med ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
               optfile="test31.med"
       ;;
       *)
               test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
       ;;
esac
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${EXECDIR}/${file} ${optfile} > ${output} 2>&1 && ${H5DUMP} ${h5dumpArgs} ${BUILDDIR}/${file}.med > ${BUILDDIR}/${file}.dump  && ( \
sed -e 's/H5T_STD_I32[LB]E//g'  -e 's/H5T_STD_I64[LB]E//g' -e 's/H5T_IEEE_F64[LB]E//g' -e 's/H5T_IEEE_F32[LB]E//g' \
    -e 's/H5T_STD_[IU]8[LB]E//g' \
    -e 's/HDF5.*{//g' \
	${BUILDDIR}/${file}.dump > ${BUILDDIR}/${file}.dump.tmp && \
sed -e 's/H5T_STD_I32[LB]E//g'  -e 's/H5T_STD_I64[LB]E//g'  \
    -e 's/H5T_IEEE_F64[LB]E//g' -e 's/H5T_IEEE_F32[LB]E//g' \
    -e 's/H5T_STD_[IU]8[LB]E//g'  \
    -e 's/HDF5.*{//g' \
	 ${BUILDDIR}/dumps.ref/${file}${medint}.dump > ${BUILDDIR}/${file}${medint}.dump.ref && \
	diff ${BUILDDIR}/${file}${medint}.dump.ref ${BUILDDIR}/${file}.dump.tmp && \
rm -f ${BUILDDIR}/${file}.dump.tmp && rm -f ${BUILDDIR}/${file}${medint}.dump.ref ) 
