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

AC_DEFUN([_MED_DEFINE_F77_ENABLE],[

 AC_ARG_ENABLE(fortran, [AS_HELP_STRING([--enable-fortran],
    [Compile the MED FORTRAN library.  Default: enabled])
 ],[
enable_fortran=$enableval
 ],[
 enable_fortran=yes
 ])
 AM_CONDITIONAL([ENABLE_FORTRAN],[test x"$enable_fortran" = xyes ])
 test x"$enable_fortran" = xyes && AC_DEFINE(MED_HAVE_FORTRAN,1,
   [Define if the Fortran part of MED library must be compiled.])
 test ! x"$enable_fortran" = xyes && AC_DEFINE(MED_HAVE_FORTRAN,0,
   [Define if the Fortran part of MED library must be compiled.])

])

AC_DEFUN([_MED_CALLING_F77], [
  AC_REQUIRE([_MED_DEFINE_F77_ENABLE])

#la macro de détection du compilateur f77 cherchera le compilateur séquentiel
#si mpi_wanted_test == no
  AX_PROG_F77_MPI(test "x$mpi_wanted_test" = "xyes",[],[
    if test x"$mpi_wanted_test" = xyes && test x"$enable_fortran" = xyes ; then
      AC_MSG_FAILURE([MPI F77 compiler requested, but couldn't use MPI.])
    fi
  ])

])


AC_DEFUN_ONCE([MED_CHECK_F77],dnl
[
#The macro for detecting Fortran compilers must not be called before
# testing mpi fortran compiler frontend
AC_BEFORE([$0],[AC_PROG_F77])
AC_REQUIRE([_MED_CALLING_F77])

AS_IF([test x"$enable_fortran" = xyes ],
[
## Looking for "$f77prog" presence 

# Pour l'affichage du Summary
   BUILD_F77TESTS_CONDITIONAL_TRUE=
   BUILD_F77TESTS_CONDITIONAL_FALSE='#'
],[
   FC="" 
# Pour l'affichage du Summary
   BUILD_F77TESTS_CONDITIONAL_TRUE='#'
   BUILD_F77TESTS_CONDITIONAL_FALSE=
])

AC_SUBST([BUILD_F77TESTS_CONDITIONAL_TRUE])
AC_SUBST([BUILD_F77TESTS_CONDITIONAL_FALSE])

])

