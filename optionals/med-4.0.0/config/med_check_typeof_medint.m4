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

AC_DEFUN([MED_CHECK_TYPEOF_MEDINT], [

## Guess where as med_int type is provided by --with-med_int option
## or auto-detection must be used

AC_ARG_WITH([med_int],
            AC_HELP_STRING([--with-med_int=<C type>],
	                   [Use <C type int or long> for med_int]),
	    [],
	    [withval=no])

test x"$withval" = xno && test x"$enable_fortran" = xno && withval=int

dnl Si aucune directive n'est donnée, test par compilation c/f quel
dnl type d'entier il faut utiliser pour med_int en utilisant les flags fortran
dnl Pour la cross-compilation, il ne faut pas lancer un test de compilation  
dnl  -> l'utilisateur spécifie le type à utiliser : int ou long
if test x"$withval" = xno
then

  ## Guess the fortran int size
  AC_CHECK_SIZEOF_FORTRAN(integer)
  if test "x$ac_cv_sizeof_fortran_integer" = "x8" ; then
     AC_DEFINE(HAVE_F77INT64,[],
                       [The size of a Fortran integer, as computed by sizeof.])
     AC_CHECK_SIZEOF(long)
     test "x$ac_cv_sizeof_long" = "x8" || AC_MSG_ERROR([Size of C type long expected to be eight bytes])
     LONG_OR_INT="long"
     #pour le module python qui est le meme pour int/long on a besoin des deux définitions 
     AC_CHECK_SIZEOF(int)
  elif test "x$ac_cv_sizeof_fortran_integer" = "x4" ; then
     AC_CHECK_SIZEOF(int)
     test "x$ac_cv_sizeof_int" = "x4" || AC_MSG_ERROR([Size of C type int expected to be four bytes])
     LONG_OR_INT="int"
     #pour le module python qui est le meme pour int/long on a besoin des deux définitions 
     AC_CHECK_SIZEOF(long)
  else
     AC_MSG_ERROR([Size of Fortran type integer is neither four nor eigth bytes])
  fi

else
  if test "x$withval" = "xlong" ; then
    AC_DEFINE(HAVE_F77INT64,[],
                       [The size of a Fortran integer, as computed by sizeof.])
    AC_MSG_WARN([Be careful that --with-med_int=long implies some FFLAGS && FCFLAGS settings for using integer*8 fortran integers.])
  elif test ! "x$withval" = "xint" ; then
     AC_MSG_ERROR([--with-med_int must be equal to int or long !])
  fi
  LONG_OR_INT="$withval" 
  AC_MSG_NOTICE([Using type $withval for med_int])
fi

AC_SUBST(LONG_OR_INT)

])
