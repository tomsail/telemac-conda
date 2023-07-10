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

AC_DEFUN([MED_ENABLE_API23],dnl
[

AC_ARG_ENABLE([api23],
               AC_HELP_STRING([--enable-api23],
               [Provide the complete 2.3.6 MED API. [default = enabled]]),
	       [med_api23=$enableval],
	       [med_api23=yes])
if test "x$med_api23" = "xyes"
then
	MED_API_23="-DMED_API_23=1"
	AC_DEFINE([MED_API_23],[1],[Define to 1 if the complete 2.3.6 API is provided.])
	AC_MSG_NOTICE([The complete 2.3.6 API is provided.])
else
dnl TODO: Tester l'utilisation d'un compilateur GNU GCC > 4.0
	MED_API_23="-DMED_API_23=0"
	AC_DEFINE([MED_API_23],[0],[Define to 1 if the complete 2.3.6 API is provided.])
	AC_MSG_WARN([The complete 2.3.6 API is not provided.])
fi

AC_SUBST([MED_API_23])
AM_CONDITIONAL([ENABLE_API23],[test "x$med_api23" = "xyes" ])

])

