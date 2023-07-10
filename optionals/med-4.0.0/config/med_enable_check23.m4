dnl  This file is part of MED.
dnl
dnl  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
dnl  MED is free software: you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published by
dnl  the Free Software Foundation, either version 3 of the License, or
dnl  (at your option) any later version.
dnl
dnl  MED is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with MED.  If not, see <http://www.gnu.org/licenses/>.
dnl

AC_DEFUN([MED_ENABLE_CHECK23],dnl
[

AC_ARG_ENABLE([check23],
               AC_HELP_STRING([--enable-check23],
               [Return an error if a MED file contains an object of a previous data model version \
which is not managable by a MED3.0 function. [default = enabled]]),
	       [med_check23=$enableval],
	       [med_check23=yes])
if test "x$med_check23" = "xyes"
then
	AC_DEFINE([MED_CHECK_23FORMAT],[1],[Return an error if a MED file contains at least one object of a previous data model \
which is not be managable by a MED3.0 function.])
	AC_MSG_NOTICE([Checking previous data model objects not strictly API3.0 compatible is activated.])
else
	AC_DEFINE([MED_CHECK_23FORMAT],[0],[Return an error if a MED file contains at least one object of a previous data model \
which is not be managable by a MED3.0 function.])
	AC_MSG_WARN([Checking previous data model objects not strictly API3.0 compatible is deactivated.])
fi
MED_CHECK_23FORMAT="$med_check23"
AC_SUBST(MED_CHECK_23FORMAT)
])

