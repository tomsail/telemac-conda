#!/bin/bash
#
# Author: Y. Audouin
# Date: 16/07/2014
#
#Returns incorrect lines
if [[ $# -lt 1 ]]; then
  echo "incorrect number of argument"
  echo "usage: check_code.sh path_to_code"
  exit 1
fi

INVALIDCHAR=cc_0-invalid-char.log
INDENT=cc_1-indent.log
COMMENT=cc_2-comment.log
CONTINUATION=cc_3-continuation.log
LOWERCASE=cc_4-lower-case.log
LINETOOLONG=cc_5-line-too-long.log
ENCODING=cc_6-encoding.log

if [ $1 == "-h" ]; then
  echo "Script checking some points of the coding convention "
  echo "for all the .f and .F in the folder given in parameter"
  echo "It will generate those files:"
  echo "-- $INVALIDCHAR: checks that there are no tabs or CLRF (Windows \\n)"
  echo "-- $INDENT: contains the line where the indentation is not a 6 + x*2"
  echo "-- $COMMENT: checks that the character used for comments is '!'"
  echo "-- $CONTINUATION: checks that the character for continuation is '&'"
  echo "-- $LOWERCASE: checks that there are no lowercase code"
  echo "-- $LINETOOLONG: checks that there are no line wider than 72 character (expect comments)"
  echo "-- $ENCODING: checks that there are no files which encoding does not support utf8"
  exit 1
fi
#
# Encoding issue
#
if [[ -e $ENCODING ]]; then
  rm $ENCODING
  touch $ENCODING
fi

for file in `find $1 -iname "*.[fF]"`; do
  file -i $file|grep -v "\(utf-8\|us-ascii\|en-ascii\)" >> $ENCODING
done

#
# Indent errors
#
grep -ER -n $2 '^(\ ){6}([ ]{2})*[ ][^\ ]' $1 --include=*.[fF] > $INDENT
#
# Comments error
#
grep -ER -n $2 '^[^!\n0-9#\ ]' $1 --include=*.[fF] > $COMMENT
#
# Continuation line error
#
grep -ER -n $2 '^(\ ){5}[^\&\ ]' $1 --include=*.[fF] > $CONTINUATION
#
# Lowercase error
#
grep -ER -n $2 '^[^!#\"'\'']*[azertyuiopqsdfghjklmnbvcxw]' $1 --include=*.[fF] > $LOWERCASE
#
# Line too long error
#
grep -ER -n $2 '^[^!]{73}' $1 --include=*.[fF] > $LINETOOLONG
#
# Invalid character error
#
grep -PR -n $2 '\t|\r' $1 --include=*.[fF] > $INVALIDCHAR

# Coding convention not applied to mascaret and nestor for now
sed -i -e "/mascaret\//d" *.log
sed -i -e "/nestor\//d" *.log

echo '*****************'
echo '     errors      '
echo '*****************'
wc -l $INVALIDCHAR
wc -l $INDENT
wc -l $COMMENT
wc -l $CONTINUATION
wc -l $LOWERCASE
wc -l $LINETOOLONG
wc -l $ENCODING
