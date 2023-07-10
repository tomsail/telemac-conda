#!/bin/sh

file=test31
${CP} ${BUILDDIR}/test14.med ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${EXECDIR}/${file} test31.med > ${output} 2>&1 && ${H5DUMP} -a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP  -d /ENS_MAA/maa1/NOE/COO -d /ENS_MAA/maa1/NOE/FAM -d /ENS_MAA/maa1/NOE/NOM -d /ENS_MAA/maa1/NOE/NUM -g /INFOS_GENERALES ${BUILDDIR}/${file}.med > ${BUILDDIR}/${file}.dump  && (\
${H5DUMP} -d /ENS_MAA/maa1/NOE/GLB ${BUILDDIR}/${file}.med  | ${AWK} 'BEGIN {i=0;p=0} i==0 && $0 ~ /ATTRIBUTE "NBR"/ {i=1;} i==1 && $0 ~ /\{/ {p=p+1;} i==1 && $0 ~ /\}/ {p=p-1; if (p==0) {i=0;next;} } i==0 {print;}' >> ${BUILDDIR}/${file}.dump ) && ( \
sed -e 's/H5T_STD_I32BE//g' -e 's/H5T_STD_I32LE//g'	 \
    -e 's/H5T_STD_I64BE//g' -e 's/H5T_STD_I64LE//g'  \
    -e 's/H5T_IEEE_F64BE//g' -e 's/H5T_IEEE_F64LE//g'  \
    -e 's/H5T_STD_[IU]8[LB]E//g'  \
    -e 's/HDF5.*{//g' \
	${BUILDDIR}/${file}.dump > ${BUILDDIR}/${file}.dump.tmp && \
sed -e 's/H5T_STD_I32BE//g' -e 's/H5T_STD_I32LE//g'	 \
    -e 's/H5T_STD_I64BE//g' -e 's/H5T_STD_I64LE//g'  \
    -e 's/H5T_IEEE_F64BE//g' -e 's/H5T_IEEE_F64LE//g'  \
    -e 's/H5T_STD_[IU]8[LB]E//g'  \
    -e 's/HDF5.*{//g' \
	 ${BUILDDIR}/dumps.ref/${file}.dump > ${BUILDDIR}/${file}.dump.ref && \
diff ${BUILDDIR}/${file}.dump.ref ${BUILDDIR}/${file}.dump.tmp && \
rm -f ${BUILDDIR}/${file}.dump.tmp && rm -f ${BUILDDIR}/${file}.dump.ref )

