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

AC_DEFUN([AC_CHECK_SIZEOF_FORTRAN], [
  AC_REQUIRE([AC_F77_WRAPPERS])
  AC_CACHE_CHECK([size of Fortran [$1]], [ac_cv_sizeof_fortran_[]translit($1, [A-Z *], [a-z_p])],[
    AC_LANG_SAVE()
    AC_LANG([Fortran 77])
    AC_COMPILE_IFELSE([
      AC_LANG_SOURCE([
        subroutine fsize()
        $1 i(2)
        call csize(i(1), i(2))
        end
      ])
    ],[
      AC_LANG([C])
      cp conftest.$ac_objext conftestf.$ac_objext
      old_LDFLAGS="$LDFLAGS"
      LDFLAGS="conftestf.$ac_objext $LDFLAGS"
#"$ac_cv_f77_libs"
      AC_TRY_RUN([
#       include <stdio.h>
#         define CSIZE F77_FUNC(csize,CSIZE)
#         define FSIZE F77_FUNC(fsize,FSIZE)
	void FSIZE();
        static long size_val;
        /* Called by Fortran */
        void CSIZE (i1p, i2p)
        char *i1p, *i2p;
        {
          size_val = (i2p - i1p);
        }
        int main() {
          FILE *f=fopen ("conftestval","w");
          if (!f) return 1;
          /* Call the Fortran function */
          FSIZE ();
          fprintf (f, "%ld\n", size_val);
          return 0;
        }
     ])
     LDFLAGS="$old_LDFLAGS"
     ac_cv_sizeof_fortran_[]translit($1, [A-Z *], [a-z_p])=`cat conftestval`
     rm -f conftestval conftestf.$ac_objext
    ])
    AC_LANG_RESTORE()
  ])
  AC_DEFINE_UNQUOTED(SIZEOF_FORTRAN_[]translit($1, [a-z *], [A-Z_p]),
                     [$ac_cv_sizeof_fortran_[]translit($1, [A-Z *], [a-z_p])],
                     [The size of a Fortran `$1', as computed by sizeof.])

])
