#!/bin/bash 


val_folder=$1

for case_dir in `ls $val_folder`; do
  cd $val_folder/$case_dir
  cp */*.fr .
  cd -
done
