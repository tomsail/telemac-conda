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

AC_DEFUN([MED_ENABLE_INSTALLTEST],dnl
[
AC_ARG_ENABLE([installtest],
               AC_HELP_STRING([--enable-installtest],
               [Install test programs. [default = disabled]]),
	       [med_installtest=$enableval],
	       [med_installtest=no])
if test "x$med_installtest" = "xyes"
then
	AM_CONDITIONAL([INSTALLTEST],[ true ])
	AC_MSG_NOTICE([Test programs will be installed.])
else
	AM_CONDITIONAL([INSTALLTEST],[ false ])
	AC_MSG_WARN([Test programs will not be installed.])
fi
])

