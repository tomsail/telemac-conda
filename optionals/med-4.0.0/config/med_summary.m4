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

AC_DEFUN([MED_SUMMARY],dnl
[

dnl hdf configuration file has been used to defined the following macros.

AC_SUBST([CONFIG_DATE]) CONFIG_DATE="`date`"
AC_SUBST([CONFIG_USER]) CONFIG_USER="`whoami`@`hostname`"
AC_SUBST(UNAME_INFO) UNAME_INFO=`uname -a`
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

dnl Compiler with version information. This consists of the full path
dnl name of the compiler and the reported version number.
AC_SUBST([CC_VERSION])
dnl Strip anything that looks like a flag off of $CC
CC_NOFLAGS=`echo $CC | sed 's/ -.*//'`

if `echo $CC_NOFLAGS | grep ^/ >/dev/null 2>&1`; then
  CC_VERSION="$CC"
else
  CC_VERSION="$CC";
  for x in `echo $PATH | sed -e 's/:/ /g'`; do
    if test -x $x/$CC_NOFLAGS; then
      CC_VERSION="$x/$CC"
      break
    fi
  done
fi
if test -n "$cc_vendor" && test -n "$cc_version"; then
  CC_VERSION="$CC_VERSION ($cc_vendor-$cc_version)"
fi

dnl Are we compiling static libraries, shared libraries, or both?  This
dnl is only used for the libhdf5.settings file. We can't just look at
dnl $enable_static and $enable_shared because if they're yes the ltconfig
dnl might have decided that one or the other is simply not possible.
dnl Therefore we have to ask the generated `libtool' shell script
dnl which 'features' it has enabled.
if (./libtool --features | grep '^enable shared libraries' > /dev/null); then
  enable_shared=yes
else
  enable_shared=no
dnl Ce n'est pas le meilleur emplacement pour réaliser ce test 
dnl mais on ne peut pas avoir une version finalisée de ./libtool avant l'appel à med_summary
  test x"$enable_python" = xyes && AC_MSG_ERROR([Building the python interface require the abilility to build shared library. Either activate --enable-shared or deactivate python interface --disable-python.])
fi

if (./libtool --features | grep '^enable static libraries' > /dev/null); then
  enable_static=yes
else
  enable_static=no
fi

if test "X$enable_static" = "Xyes" && test "X$enable_shared" = "Xyes"; then
  STATIC_SHARED="static, shared"
elif test "X$enable_static" = "Xyes"; then
  STATIC_SHARED="static"
elif test "X$enable_shared" = "Xyes"; then
  STATIC_SHARED="shared"
else
  STATIC_SHARED="none"
fi

AC_SUBST([STATIC_SHARED])
])
