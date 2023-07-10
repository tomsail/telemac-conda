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

AC_DEFUN([_MED_DEFINE_F90_ARGS],[
## Guess where as f90prog is defined by the F90 environement variable
## or by the --with-f90 option
 AC_ARG_WITH([f90],
             AC_HELP_STRING([--with-f90=<compiler>],
 	                   [Use <compiler> for F90 compiler (default:no)]),
 	    [
            ], [with_f90=no
 ])
])
 
AC_DEFUN([_MED_BEFORE_FC], [
  AC_REQUIRE([_MED_DEFINE_F90_ARGS])
#We must check if the user want the Fortran interface
#before testing a mpi fortran frontend
 AC_REQUIRE([_MED_DEFINE_F77_ENABLE])
#Pour des raisons historiques prend en compte la variable utilisateur F90
test x"$with_f90" = xyes && f90prog=${F90}
#La variable f90prog mémorise le choix de compilateur F90 fait par l'utilisateur, mais les variables MPIFC et FC restent proritaires
#Ces deux veriables sont utilisées dans ax__prog__fc__mpi
test ! x"$with_f90" = xyes && test ! x"$with_f90" = xno && f90prog=$with_f90
#Faut-il lancer la détection du compilateur (par défaut non)
med_check_f90="no"
#Si l'activation de f90 a été explicitement demandée ou si l'une des variables FC, MPIFC est positionée la détection du compilateur doit être activée
(test ! x"$with_f90" = xno||(test x"$with_f90" = xno &&(test ! x"$FC" = x||test ! x"$MPIFC" = x )))&&med_check_f90="yes"
#Si l'utilisateur a explicitement demandé de ne pas contruire l'interface Fortran par l'option --enable-fortran=no, il ne faut pas lance la détection du compilateur
test x"$enable_fortran" = xno && med_check_f90="no"
])

AC_DEFUN([_MED_CALLING_FC], [
#la macro de détection du compilateur f90 cherchera le compilateur séquentiel si mpi_wanted_test == no
#Elle prend en compte les variables f90prog, FC et MPIFC et mpi_wanted_test pour définir et tester le compilateur dont le nom sera forcément FC en retour (parallel ou séquentiel)  
  AX_PROG_FC_MPI(test "x$mpi_wanted_test" = "xyes",[],[
  if test x"$mpi_wanted_test" = xyes && test x"$med_check_f90" = xyes ; then
    AC_MSG_ERROR([MPI F90 compiler requested, but couldn't use MPI.])
  fi
  ])
#Si aucun compilateur n'a été trouvé et que le F90 est requis, on génère une erreur
  test x"$FC" = x && test x"$med_check_f90" = xyes && AC_MSG_ERROR([Can't find a valid FC compiler, check your configure switches or the FC var...])
])


AC_DEFUN_ONCE([MED_CHECK_F90],dnl
[
#The macro for detecting Fortran compilers must not be called before
# testing mpi fortran compiler frontend
AC_BEFORE([$0],[AC_PROG_FC])
AC_BEFORE([$0],[AC_PROG_F77])
AC_REQUIRE([_MED_BEFORE_FC])

_MED_CALLING_FC

AS_IF([test  x"$FC" != x && test x"$med_check_f90" = xyes ],
[
#Le compilateur FC étant défini, on positionne maintenant les LDFLAGS
  AC_FC_LIBRARY_LDFLAGS
  ##AC_FC_WRAPPERS Inutile car pas d'interface spécifique f90 

# Ce test semble obsolète : A vérifier.
  if test -z "$FC" ; then
    AC_MSG_ERROR([Can't find $f90prog command, please verify your env. var. PATH])
  fi
  
#Si HDF5 n'est pas parallel, la macro ax__prog__fc__mpi n'a pas testé de compilation en F90 
if test x"$mpi_wanted_test" = xno; then
    AC_LANG_PUSH([Fortran])
    AC_MSG_CHECKING([ a F90 compilation])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[[
      print *,"Testing a F90 compilation..."
]])],
        [ AC_MSG_RESULT(ok)],
        [ AC_MSG_RESULT(ko)
          AC_MSG_ERROR([Can't find a valid FC compiler, check your configure switches or the FC var...])
        ]
    )
    AC_LANG_POP([Fortran])
fi

#Test la capacité du compilateur F90 a gérer le pré-processing
   AC_FC_PP_SRCEXT(F90, [ AC_FC_PP_DEFINE( [AM_CONDITIONAL([WITHFCPP],[true])],
                                           [AM_CONDITIONAL([WITHFCPP],[false])] )
			], [AM_CONDITIONAL([WITHFCPP],[false])]
		  )

# Pour l'affichage du Summary
   BUILD_F90TESTS_CONDITIONAL_TRUE=
   BUILD_F90TESTS_CONDITIONAL_FALSE='#'
],[
   FC="" 
# Pour l'affichage du Summary
   BUILD_F90TESTS_CONDITIONAL_TRUE='#'
   BUILD_F90TESTS_CONDITIONAL_FALSE=
   AM_CONDITIONAL([WITHFCPP],[false])

])

AC_SUBST([BUILD_F90TESTS_CONDITIONAL_TRUE])
AC_SUBST([BUILD_F90TESTS_CONDITIONAL_FALSE])

])

