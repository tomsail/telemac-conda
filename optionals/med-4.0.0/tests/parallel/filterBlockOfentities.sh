#!/bin/sh
#MPIEXEC="lamwipe && lamboot && mpirun -ssi rpi tcp -np "
test "x${MPIEXEC}" = "x" && MPIEXEC="mpiexec -np "
test "x${NPROCS}" = "x" && NPROCS=8
test "x${KEEPOUTPUTFILES}" = "x" && KEEPOUTPUTFILES=no
file=filterBlockOfentities
test "x${CPYMED}" = "xyes" && ${CP} ${SRCDIR}/test.med.ref ${BUILDDIR}/${file}.med && chmod 644 ${BUILDDIR}/${file}.med
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
myabort() { echo $* >&2 ; exit 1;}

rm -f NENT-???_NVAL-???_NCST-???_NBL-???_CPU-???_@_*.med
rm -f NENT-???_NVAL-???_NCST-???_NBL-???_CPU-???_@_*.med.dump
rm -f NENT-???_NVAL-???_NCST-???_NBL-???_CPU-???_@_*.ascii
rm -f filterBlockOfentities.out
i=1
while test $i -le ${NPROCS}
do
 echo "Launching ${file} with ${i} processes"
 eval ${MPIEXEC} ${i} ${BUILDDIR}/${file}  || myabort "Can't launch ${file} with ${i} processes"
 i=`expr $i + 1`
done &&  \
i=1 &&
while test $i -le ${NPROCS} 
do
 i1000=`expr $i + 1000` && ii=`echo $i1000 | sed 's/^1//'`

 echo "Generating .dump files for ${ii} proc. runs" >> ${output}
 medfilelist=`find ${BUILDDIR} -name "*CPU-${ii}*.med" -print ` 
 for j in ${medfilelist} 
 do
   ${H5DUMP} ${j} | sed -e 's/HDF5.*{//g' > ${j}.dump || myabort "Can't generate ${j}.dump file"
 done 

 dumpfileref=${j}.dump
 dumpfilelist=`find ${BUILDDIR} -name "*CPU-${ii}*.dump" -print `
 for j in ${dumpfilelist} 
 do
  echo "Comparing with ${j}" >> ${output}
  diff ${dumpfileref} ${j} || myabort "file ${j} differ from ${dumpfileref}."
 done

 asciifilelist=`find ${BUILDDIR} -name "*CPU-${ii}*.ascii" -print | grep 'MED_GLOBAL'`
 asciifileref=`echo ${asciifilelist} | awk '{print $1}' `
 for j in ${asciifilelist}
 do
  echo "Comparing with ${j}" >> ${output}
  diff ${asciifileref} ${j} || myabort "file ${j} differ from ${asciifileref}."
 done

 asciifilelist=`find ${BUILDDIR} -name "*CPU-${ii}*.ascii" -print | grep 'MED_COMPACT'`
 asciifileref=`echo ${asciifilelist} | awk '{print $1}' `
 for j in ${asciifilelist}
 do
  echo "Comparing with ${j}" >> ${output}
  diff ${asciifileref} ${j} || myabort "file ${j} differ from ${asciifileref}."
 done

 i=`expr $i + 1`
done
test "x${KEEPOUTPUTFILES}" = "xno" && rm -f NENT-???_NVAL-???_NCST-???_NBL-???_CPU-???_@_*.med NENT-???_NVAL-???_NCST-???_NBL-???_CPU-???_@_*.med.dump && rm -f NENT-???_NVAL-???_NCST-???_NBL-???_CPU-???_@_*.ascii
exit 0

