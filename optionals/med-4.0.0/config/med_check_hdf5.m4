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

AC_DEFUN([_MED_DEFINE_HDF5_ARGS],[
## Guess where as hdf5 is located by the HDF5HOME environement variable
## or by the --with-hdf5 option
 AC_ARG_WITH([hdf5],
             AC_HELP_STRING([--with-hdf5=<path>],
	                   [Use <path> for HDF5 path.]),
	    [],
[with_hdf5=yes])

 AC_ARG_WITH([hdf5-include],
             AC_HELP_STRING([--with-hdf5-include=<include path>],
	                   [Use <include path> for HDF5 include path.]),
	    [],
[with_hdf5_include=no])

 AC_ARG_WITH([hdf5-lib],
             AC_HELP_STRING([--with-hdf5-lib=<lib path>],
	                   [Use <lib path> for lib HDF5 path.]),
	    [],
[with_hdf5_lib=no])

 AC_ARG_WITH([hdf5-bin],
             AC_HELP_STRING([--with-hdf5-bin=<bin path>],
	                   [Use <bin path> for bin HDF5 path.]),
	    [hdf5bin=$with_hdf5_bin],
[with_hdf5_bin=no])

])

AC_DEFUN([_MED_BEFORE_ENABLE_PMED], [
AC_REQUIRE([_MED_DEFINE_HDF5_ARGS])

dnl ## If user hasn't provided hdf5 path try using "$HDF5HOME" 
    test "x$with_hdf5" = "xyes" && hdf5home=${HDF5HOME} || hdf5home=$with_hdf5

    if test ! "x$with_hdf5" = "xno"; then
dnl ## If user has  neither provided hdf5 path (--with-hdf5 or "$HDF5HOME"=="") nor --with-hdf5-include and --with-hdf5-lib then try to use system path /usr as HDF5 home
       test -z "$hdf5home"  &&  test "x$with_hdf5_include" = "xno" && test "x$with_hdf5_lib" = "xno"  && hdf5home="/usr/"

dnl #If user specified a specific hdf5home, be careful to not detect an hdf5 package in the system
dnl #Vérify that the hdf5 directory structure exist
       if test -z "$hdf5home" && (test "x$with_hdf5_include" = "xno" || test "x$with_hdf5_lib" = "xno") ; then
         AC_MSG_ERROR([The hdf5 path |$hdf5home| isn't specified, please verify either HDF5HOME or --with-hdf5=<path>. You may also use --with-hdf5-include and --with-hdf5-lib.])
       fi
       if test ! -d "$hdf5home" && (test "x$with_hdf5_include" = "xno" || test "x$with_hdf5_lib" = "xno") ; then
         AC_MSG_ERROR([The directory hdf5 home |$hdf5home| doesn't exist, please verify either HDF5HOME or --with-hdf5=<path>])
       fi

       found_H5pubconf_h=no
       hdf5home_save=$hdf5home
       #if user has provided a specific hdf5 path for include or lib don't try to guess specific system path
       if test "x$with_hdf5_include" = "xno" && test "x$with_hdf5_lib" = "xno"; then
         #Search for H5pubconf.h in $hdf5home and some other standard paths if user doesn't provide hdf5_include
       	# H5pubconf.h is used further to detect serial or parallel hdf5 
       	#Cette approche ne prend pas en compte CPPFLAGS, inconvénient mineur de  au vu du test d'arborescence précédent
         for hdf5home in "$hdf5home_save" "$hdf5home_save/lib/x86_64-linux-gnu/hdf5/serial/"  "$hdf5home_save/lib/x86_64-linux-gnu/hdf5/parallel/"
         do
          AC_MSG_NOTICE([Trying $hdf5home home path for searching H5pubconf.h file.])
          test -e $hdf5home/include/H5pubconf.h && found_H5pubconf_h=yes && break
         done
       fi

       test "x$with_hdf5_include" = "xno" && hdf5include=$hdf5home/include || hdf5include=$with_hdf5_include
       if test ! -d "$hdf5include" ; then
         AC_MSG_ERROR([The directory hdf5 include |$hdf5include| doesn't exist, please verify either HDF5HOME, --with-hdf5=<path> or --with-hdf5-include])
       fi
       
       test ! "x$found_H5pubconf_h" = "xyes" || test -e ${hdf5include}/H5pubconf.h || AC_MSG_ERROR([Can't find H5pubconf.h file, please verify either HDF5HOME, --with-hdf5=<path> or --with-hdf5-include.])
	
       test "x$with_hdf5_lib" = "xno" && hdf5lib=$hdf5home/lib || hdf5lib=$with_hdf5_lib
       if test ! -d "$hdf5lib" ; then
         AC_MSG_ERROR([The directory hdf5 lib |$hdf5lib| doesn't exist, please verify either HDF5HOME, --with-hdf5=<path> or --with-hdf5-lib])
       fi

       test "x$with_hdf5_bin" = "xno" && hdf5bin=$hdf5home_save/bin || hdf5bin=$with_hdf5_bin
       if test ! -d "$hdf5bin" ; then
         AC_MSG_ERROR([The directory hdf5 bin |$hdf5bin| doesn't exist, please verify either HDF5HOME, --with-hdf5=<path> or --with-hdf5-bin])
       fi

    else
## In case user explicitly ask to not use hdf5 !
      AC_MSG_WARN([Can't compile MED without hdf5])
      AC_MSG_ERROR([either use HDF5HOME env. var. or --with-hdf5=<path> even if you use all the specific --with-hdf5-... paths.])
    fi
])


AC_DEFUN([MED_CHECK_HDF5],dnl
[
#La localisation d'hdf5 doit être établie avant
AC_REQUIRE([_MED_BEFORE_ENABLE_PMED])
#de tester l'aspect parallel de la bibliothèque (H5pubconf.h).
AC_REQUIRE([MED_ENABLE_PMED])

AC_REQUIRE([AC_PROG_SED])
AC_REQUIRE([AC_PROG_GREP])


      LDFLAGS_save="$LDFLAGS"
      LIBS_save="$LIBS"
      CPPFLAGS_save="$CPPFLAGS"

      #Cette macro a au minimum besoin du préprocesseur dont la detection appelle malheureusement aussi la detection du compilateur
      #Elle ne peut donc pas être appelée avant de connaître l'aspect serial/parallel d'hdf (pour le choix du frontend mpi ou compilateur direct).
      #On ne test donc la version d'HDF qu'après l'aspect // ou séquentiel
      LDFLAGS="-L$hdf5lib $LDFLAGS"
      CPPFLAGS="-I$hdf5include $CPPFLAGS"
      AX_ABSOLUTE_HEADER([H5public.h])

      #Use H5public.h in order to find the HDF version we are using.
      HDF5_ABS_PATH="$gl_cv_absolute_H5public_h"
      HDF5_USED_VER="unknown"
      HDF5_VERSION="0"
      if test -n "$HDF5_ABS_PATH" ; then 
         HDF5_USED_VER=` grep H5_VERS_INFO $HDF5_ABS_PATH | sed  's/\([[^"]]*\)\(".*"\)\([[^"]]*\)/\2/g' `
         H5_VER_MAJOR=`  grep '#define *H5_VERS_MAJOR' $HDF5_ABS_PATH | sed  's/^.*H5_VERS_MAJOR[[ \t]]*\([0-9]*\)[[ \t]]*.*$/\1/g' `
         H5_VER_MINOR=`  grep '#define *H5_VERS_MINOR' $HDF5_ABS_PATH | sed  's/^.*H5_VERS_MINOR[[ \t]]*\([0-9]*\)[[ \t]]*.*$/\1/g' `
         H5_VER_RELEASE=`grep '#define *H5_VERS_RELEASE' $HDF5_ABS_PATH | sed  's/^.*H5_VERS_RELEASE[[ \t]]*\([0-9]*\)[[ \t]]*.*$/\1/g' `
         HDF5_VERSION=`  expr 10000 \* ${H5_VER_MAJOR} + 100 \* ${H5_VER_MINOR} + ${H5_VER_RELEASE} `
         test "0${HDF5_VERSION}" -gt "11100" || test "0${HDF5_VERSION}" -lt "11002" && AC_MSG_ERROR([
This HDF5 version ${H5_VER_MAJOR}.${H5_VER_MINOR}.${H5_VER_RELEASE} must not be used with med-fichier${MED_NUM_MAJEUR}.${MED_NUM_MINEUR}.${MED_NUM_RELEASE}.
The HDF5 library version used by med-fichier${MED_NUM_MAJEUR}.y.z MUST NOT be > 1.10 and have to be at least HDF${HDF_VERSION_REF}.
DO NOT TRY TO COMPILE med-fichier${MED_NUM_MAJEUR}.${MED_NUM_MINEUR}.${MED_NUM_RELEASE} version with an HDF5 library which would generate an hdf5 file not compliant with HDF5-${HDF_VERSION_MAJOR_REF}.${HDF_VERSION_MINOR_REF}.z library.
This would BREAK med-fichier compatibility between files with the same revision number !
      ])
     else
## In case user explicitly ask to not use hdf5 !
      AC_MSG_WARN([Can't compile MED without hdf5])
      AC_MSG_ERROR([either use HDF5HOME env. var. or --with-hdf5=<path>.])
     fi

      HDF5_TYPEDEF_HERR_T=` sed -n '/^[[ \t]]*typedef .*herr_t[[ \t]]*;/p' $HDF5_ABS_PATH `
      HDF5_TYPEDEF_HSIZE_T=` sed -n '/^[[ \t]]*typedef .*hsize_t[[ \t]]*;/p' $HDF5_ABS_PATH `
      AX_ABSOLUTE_HEADER([H5Ipublic.h])
      HDF5_TYPEDEF_HID_T=` sed -n '/^[[ \t]]*typedef .*hid_t[[ \t]]*;/p' $gl_cv_absolute_H5Ipublic_h `

#      AC_CHECK_LIB([m],[sin],[],[AC_MSG_ERROR([Can't find C math library.])],[])
#      AC_CHECK_LIB([z],[compress],[],[AC_MSG_ERROR([Can't find z library.])],[])
      AC_CHECK_LIB([hdf5],[H5open],[],[AC_MSG_ERROR([Can't find hdf5 library, either use HDF5HOME env. var. , --with-hdf5=<path> or --with-hdf5-lib=<libpath>])],[-lm -lz])
      test "x$mpi_wanted_test" = "xyes" &&  AC_CHECK_FUNCS(H5Pset_fapl_mpio, [],[AC_MSG_ERROR([HDF5 is detected as a parallel version but can't find H5Pset_fapl_mpio function.])] )

      if test "x$with_hdf5_include" = "xno"; then
        AC_PATH_PROG([H5DUMP],[h5dump],[no],[$hdf5bin$PATH_SEPARATOR$PATH$PATH_SEPARATOR])
      else
        AC_PATH_PROG([H5DUMP],[h5dump],[no],[$hdf5bin])
      fi
      if test "X$H5DUMP" = "Xno"; then
         AC_MSG_ERROR([Can't find h5dump, please verify your env. var. PATH or use of --with-hdf5bin=<binpath>.])
      fi


#      AC_DEFINE(H5_USE_16_API,[],[Using HDF5-1.6 API compatibility with HDF5 1.8 libraries])
      HDF5_CPPFLAGS="-DH5_USE_16_API"
      HDF5_CPPFLAGS="$HDF5_CPPFLAGS -I$hdf5include"
      HDF5_LDFLAGS="-L$hdf5lib"
      LDFLAGS="$LDFLAGS_save"
      CPPFLAGS="$CPPFLAGS_save"
      LIBS="$LIBS_save"

      AC_SUBST(HDF5_CPPFLAGS)
      AC_SUBST(HDF5_LDFLAGS)
#La ligne suivante est inutile car effectuée par AC_CHECK_PROG 
#      AC_SUBST(H5DUMP)
      AC_SUBST([HDF5_USED_VER])
      HDF5_LIBS="-lhdf5"
      AC_SUBST([HDF5_LIBS])

      AC_SUBST(HDF5_TYPEDEF_HERR_T)
      AC_SUBST(HDF5_TYPEDEF_HID_T)
      AC_SUBST(HDF5_TYPEDEF_HSIZE_T)
])

