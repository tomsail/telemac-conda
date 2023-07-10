#!/bin/bash

tmp_file='tmp_input.txt'
filename=$(basename -- "$1")
dirname=$(dirname -- "$1")
extension="${filename##*.}"
rootname="${filename%.*}"
cd $dirname
# if .F file removing preprocessor lines (makes doxygen bug)
if [[ ${extension} == 'F' ]]; then
  new_ext=F90
else
  new_ext=f90
fi

new_file=${rootname}.${new_ext}
echo $filename

cp $filename $new_file

# Changing continuation symbol position
$HOMETEL/optionals/addons/to_f90/continuation_line.py ${new_file} ${new_file}

# Changing header to doxygen header style
sed -i -e "s/^!\s*|\s*\(\w\+\)\s*|\s*\(.\+\)\s*|\s*\(.\)\(.*\)/!>@param[\2] \1 \3\L\4/g" ${new_file}
sed -i -e "s/^![ ]*|[ ]\+|[ ]\+|/!!  /g" ${new_file}
sed -i -e "s/^\([ ]*\)!BRIEF/\1!>@brief/g" ${new_file}
sed -i -e "s/^\([ ]*\)!brief/\1!>@brief/g" ${new_file}
sed -i -e "s/^\([ ]*\)!history/\1!>@history/g" ${new_file}
sed -i -e "s/^\([ ]*\)!HISTORY/\1!>@history/g" ${new_file}
sed -i -e "s/\[-->\]/[in]/g" ${new_file}
sed -i -e "s/\[<--\]/[out]/g" ${new_file}
sed -i -e "s/\[<->\]/[in, out]/g" ${new_file}
sed -i -e "/\s*!+\s*$/d" ${new_file}
sed -i -e "s/\(\s*\)!+ /\1!! / g" ${new_file}


cat ${new_file}
rm ${new_file}
