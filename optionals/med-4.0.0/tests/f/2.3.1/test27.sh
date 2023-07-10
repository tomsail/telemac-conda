#!/bin/sh

file=test27
test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
${CHECKER} ${EXECDIR}/${file} > ${output} 2>&1 && ${H5DUMP} -a /ENS_MAA/grille\ cartesienne/DIM -a /ENS_MAA/grille\ cartesienne/DES -a /ENS_MAA/grille\ cartesienne/TYP -a /ENS_MAA/grille\ cartesienne/GTY -g /ENS_MAA/grille\ cartesienne/NOE -a /ENS_MAA/grille\ standard/DIM -a /ENS_MAA/grille\ standard/DES -a /ENS_MAA/grille\ standard/TYP -a /ENS_MAA/grille\ standard/GTY -g /ENS_MAA/grille\ standard/NOE  -a /ENS_MAA/maillage\ vide/DIM -a /ENS_MAA/maillage\ vide/DES -a /ENS_MAA/maillage\ vide/TYP -g /INFOS_GENERALES ${BUILDDIR}/${file}.med > ${BUILDDIR}/${file}.dump  && ( \
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

