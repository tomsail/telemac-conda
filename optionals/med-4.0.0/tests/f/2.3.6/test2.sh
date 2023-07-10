#!/bin/sh

file=test2
test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
${EXECDIR}/${file} > /dev/null 2>&1 && ${H5DUMP} -a /ENS_MAA/maa1/DIM -a /ENS_MAA/maa1/DES -a /ENS_MAA/maa1/TYP -g /ENS_MAA/maa2 -g /ENS_MAA/maa3 -g /INFOS_GENERALES ${BUILDDIR}/${file}.med > ${BUILDDIR}/${file}.dump  && (  \
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

