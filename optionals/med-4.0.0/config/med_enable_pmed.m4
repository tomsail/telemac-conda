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

dnl Ceci ne fonctionne pas car necessite le checkprogcc !
dnl AC_CHECK_DECL([H5_HAVE_PARALLEL], 
dnl  [
dnl  ],
dnl  [ 
dnl  ], 
dnl  [[#include <H5pubconf.h>]]
dnl   ],[])
dnl )

dnl Ceci ne fonctionne pas car necessite le checkprogcc !
dnl AC_EGREP_CPP ([MED_CATCH_H5_HAVE_PARALLEL],
dnl [AC_LANG_PUSH([C])
dnl  AC_LANG_CONFTEST([AC_LANG_SOURCE([[
dnl \#ifdef H5_HAVE_PARALLEL
dnl \#warning "MED_CATCH_H5_HAVE_PARALLEL"
dnl \#else
dnl \#error "I'M SO SORRY"
dnl \#endif
dnl ]]))]
dnl  AC_LANG_POP
dnl ],
dnl [mpi_wanted_test="yes"],
dnl [mpi_wanted_test="no"])

# This function use H5pubconf.h to detect serial/parallel version of hdf found in ${hdf5include}.
# No compiling is used within the test
AC_DEFUN([_MED_BEFORE_CC_MPI], [
AC_REQUIRE([AC_PROG_SED])
       
if test "x`sed -n '/^#define *H5_HAVE_PARALLEL *1\{0,1\}/{s/.*/MED_CATCH_H5_HAVE_PARALLEL/p;}' ${hdf5include}/H5pubconf.h`" = "xMED_CATCH_H5_HAVE_PARALLEL"
then
 mpi_wanted_test="yes"
else
 mpi_wanted_test="no"
fi
])

# If hdf5 is parallel we have to find mpicc frontend
AC_DEFUN([_MED_CALLING_CC_MPI], [
AC_REQUIRE([_MED_BEFORE_CC_MPI])

AX_PROG_CC_MPI(test "x$mpi_wanted_test" = "xyes",[],[
  if test x"$mpi_wanted_test" = xyes; then
    AC_MSG_FAILURE([MPI CC compiler requested, but couldn't use it.])
  fi
])
])

# This macro test if hdf5 is a parallel version, then
# call for mpi C/F compiler frontend detection, set WITHPMED conditionnal, substitute BUILD_WITH_PMED with yes or no and add a MED_HAVE_MPI define in med_config.h 
AC_DEFUN([MED_ENABLE_PMED],dnl
[
AC_REQUIRE([_MED_CALLING_CC_MPI])
#We must check if the user want the Fortran interface
#before testing a mpi fortran frontend
#On devrait pouvoir le désactiver ici car il a été
#requis ds le check f90
AC_REQUIRE([_MED_DEFINE_F77_ENABLE])

#The parallel fortran test must have a f90 compiler if fortran interface is activated
MED_CHECK_F90

if test "x$mpi_wanted_test" = "xyes"
then
 AM_CONDITIONAL([WITHPMED],[ true ])

#Mécanisme pour exporter le #define MED_HAVE_MPI 
#dans med.h.in et non dans med_config.h.in
 MED_DEFINE_MED_HAVE_MPI="#define MED_HAVE_MPI"
 BUILD_WITH_PMED="yes"
else
 BUILD_WITH_PMED="no"
 AC_MSG_NOTICE([HDF version is not a parallel one.])
 AM_CONDITIONAL([WITHPMED],[ false ])
fi
 AC_SUBST(MED_DEFINE_MED_HAVE_MPI)
 AC_SUBST(BUILD_WITH_PMED)
])
