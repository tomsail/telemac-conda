file=Parallel_test1
test "x${MPIEXEC}" = "x" && MPIEXEC="mpiexec -np "
test "x${NPROCS}" = "x" && NPROCS=8
test "x${KEEPOUTPUTFILES}" = "x" && KEEPOUTPUTFILES=no
output=/dev/null && test "x${OUTPUT}" = "xyes" && output=${file}.out
myabort() { echo $* >&2 ; exit 1;}

rm -f fort.*
rm -f _?_fort.*
i=1
while test $i -le ${NPROCS}
do

 echo "Launching Parallel_test1 with ${i} processes"
 eval ${MPIEXEC} ${i} ${EXECDIR}/Parallel_test1 > ${output} 2>&1 || myabort "Can't launch ${file} with ${i} processes"

 j=0
 while test $j -lt ${i}
 do
   cat fort.4${j} >> _${i}_fort.40
   j=`expr $j + 1`
 done

  echo "Comparing _${i}_fort.40 file to _1_fort.40" >> ${output}
  diff _${i}_fort.40 _1_fort.40 || myabort "_${i}_fort.40 differ from _1_fort.40"

 i=`expr $i + 1`
done &&  \
test "x${KEEPOUTPUTFILES}" = "xno" && rm -f fort.* && rm -f _?_fort.*
exit 0