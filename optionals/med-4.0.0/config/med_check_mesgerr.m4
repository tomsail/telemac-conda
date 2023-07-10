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

AC_DEFUN([MED_CHECK_MESGERR],dnl
[

## Guess if we have to deactivate explicit error messages

AC_ARG_ENABLE([mesgerr],
               AC_HELP_STRING([--enable-mesgerr],
	                   [MED library display error messages @<:@default=yes@:>@]),
	       [med_check_mesgerr=$enableval],
	       [med_check_mesgerr=yes])

if test "x$med_check_mesgerr" = "xyes"
then					     
    AC_DEFINE(MESGERR,[1],
                   [MED library will display error messages.])
    AC_MSG_NOTICE([MED library display error messages is activated.])

else
## In case user explicitly ask not to use mesgerr !
    med_check_mesgerr="no"
    AC_DEFINE(MESGERR,[0],
                   [MED library will not display error messages.])
    AC_MSG_WARN([MED library display error messages is desactivated.])
fi
MESGERR="$med_check_mesgerr"
AC_SUBST(MESGERR)
])

