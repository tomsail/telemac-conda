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

AC_DEFUN([MED_CHECK_TYPEOF_HIDT], [
AC_REQUIRE([MED_CHECK_HDF5])

## Guess where as hid_t type is provided by --with-hid_t option
## or auto-detection must be used

AC_ARG_WITH([hid_t],
            AC_HELP_STRING([--with-hid_t=<C type>],
	                   [Use <C type int or long> for hid_t (autodetect by default)]),
	    [],
	    [withval=no])

#Si l'interface fortran n'est pas générée ne lance pas les tests avec compilation
#La taille du hid_t est déduite de la version d'HDF.
#A partir d'HDF 1.10 hid_t est 64 bits
test x"$withval" = xno && test x"$enable_fortran" && ( test "0${HDF5_VERSION}" -gt "10899" && withval=long || withval=int )

dnl Si aucune directive n est donnée, on vérifie la taille du hid_t induit de la version HDF utilisée
dnl et on vérifie la cohérence avec la taille réelle de l entier Fortran.
dnl Pour la cross-compilation, il ne faut pas lancer un test de compilation  
dnl  -> l utilisateur spécifie le type à utiliser : int ou long
if test x"$withval" = xno
then
  ## Guess the fortran int size
  CPPFLAGS_save="$CPPFLAGS"
  CPPFLAGS="-I$hdf5include $CPPFLAGS "
  AC_CHECK_SIZEOF(hid_t,[],[#include <H5Ipublic.h>])
  CPPFLAGS="$CPPFLAGS_save"
  if test "x$ac_cv_sizeof_hid_t" = "x8" ; then
     test "0${HDF5_VERSION}" -lt "11000" && AC_MSG_ERROR([Size of the hid_t HDF5 type should not be eight bytes for HDF5 version < 1.10])
     AC_DEFINE(HAVE_F77HIDT64,[],
                       [The size of a Fortran integer for hid_t C type, as computed by sizeof.])
     AC_CHECK_SIZEOF_FORTRAN(integer*8)
     test "x$ac_cv_sizeof_fortran_integerp8" = "x8" || AC_MSG_ERROR([Size of integer*8 F type is expected to be eight bytes])
     HIDT_I8_OR_I4="integer*8"
  elif test "x$ac_cv_sizeof_hid_t" = "x4" ; then
     test "0${HDF5_VERSION}" -ge "11000" && AC_MSG_ERROR([Size of the hid_t HDF5 type should not be four bytes for HDF5 version >= 1.10])
     AC_CHECK_SIZEOF_FORTRAN(integer*4)
     test "x$ac_cv_sizeof_fortran_integerp4" = "x4" || AC_MSG_ERROR([Size of integer*4 F type is expected to be four bytes])
     HIDT_I8_OR_I4="integer*4"
  else
     AC_MSG_ERROR([Size of hid_t HDF5 type is neither four nor eight bytes])
  fi

else
  if test "x$withval" = "xlong" ; then
    test "0${HDF5_VERSION}" -lt "11000" && AC_MSG_ERROR([Size of the hid_t HDF5 type should not be eight bytes for HDF5 version < 1.10])
    AC_DEFINE(HAVE_F77HIDT64,[],
                       [The size of a Fortran integer for hid_t C type, as computed by sizeof.])
    AC_MSG_WARN([Be careful that --with-hid_t=long implies using HDF 1.10.z])
  elif test ! "x$withval" = "xint" ; then
     AC_MSG_ERROR([--with-hid_t must be equal to int or long !])
  fi
  test ! "x$withval" = "xint" && test "0${HDF5_VERSION}" -ge "11000" && AC_MSG_ERROR([Size of the hid_t HDF5 type should not be four bytes for HDF5 version >= 1.10])

  HID_LONG_OR_INT="$withval" 
  AC_MSG_NOTICE([Using type $withval for hid_t])
fi

AC_SUBST(HIDT_I8_OR_I4)

])
