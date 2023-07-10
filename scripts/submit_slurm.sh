#!/bin/bash

id_log=$1

if [[ $# -eq 2 ]]; then
  cp HPC_STDIN ../
  cd ../
fi

# Set the WCKey if provided
if [[ ! -z $2 ]]; then
  export SBATCH_WCKEY=$2
fi

ret=`sbatch < HPC_STDIN > err.log 2>&1`
err=$?
while [[ $err -ne 0 ]]; do
  let "i=0"
  for msg in `cat err.log`; do
    err_msg=$msg
    let "i+=1"
    if [[ $i -eq 3 ]]; then
      break
    fi
  done

  # If we have maxjob reached looping otherwise crashing
  if [[ "$err_msg" == "AssocMaxSubmitJobLimit" ]]; then
    echo Submission crashed waiting 5 minutes before retrying
    sleep 5m
    ret=`sbatch < HPC_STDIN > err.log 2>&1`
    err=$?
  else
    cat err.log
    rm err.log
    exit $err
  fi
done
ret=`cat err.log`
id=`echo $ret|tr ' ' '\n'|tail -n 1`
if [[ $# -eq 2 ]]; then
  dir=`readlink -f .`
else
  dir=$3
fi
echo "$id;$dir" >> $id_log
cat err.log
rm err.log
exit $err
