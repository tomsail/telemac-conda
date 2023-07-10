#!/bin/bash
new_file=$1


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
