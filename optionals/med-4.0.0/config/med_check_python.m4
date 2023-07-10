#
# MED_CHECK_PYTHON
# ---------------
#

AC_DEFUN([_MED_DEFINE_PYTHON_ENABLE],[

 AC_ARG_ENABLE(python, [AS_HELP_STRING([--enable-python],
    [Install the MED python library.  Default: enabled])
 ],[
  enable_python=$enableval
 ],[
  enable_python=yes
 ])
])

AC_DEFUN([MED_CHECK_PYTHON], [
 AC_REQUIRE([_MED_DEFINE_PYTHON_ENABLE])
 AC_BEFORE([$0],[AM_PATH_PYTHON])

# Si swig n'est pas activé mais l'utilisation de python oui,
# il faut appeler la macro MEDCHECK_PYTHON_DEVEL (également appelée par MEDCHECK_SWIG si le swig est activée).
# Elle permet de positionner les flags de compilation/édition des liens (python devel)
# pour compiler les interfaces python embarquées en essayant de trouver la version PYTHON_VERSION_REF
# ou au minimum  PYTHON_VERSION_MIN
# Elle définie  également la version de python utilisée via ac_python_version, les tests suivants ont besoin
# La version est codée  "x.y" (pas de release).
# Ne pas utiliser PYTHON_VERSION qui est une variable préciseuse
 AS_IF([test x$enable_python = xyes && test -z "$ac_python_version"],[MED_CHECK_PYTHON_DEVEL])

 test x"$WITH_SWIG" = x1 && test x"$enable_python" = xno && AC_MSG_ERROR([python is needed because swig is activated.])
#La gestion des modules précompilés existe désormais en med_int=int|long
# test x"$WITH_SWIG" = x0 && test x"$enable_python" = xyes && test ! x"$LONG_OR_INT" == "xint" && AC_MSG_ERROR([Either use --disable-python or --with-swig=[yes|<swigdir>]. Swig is needed because embedded python interfaces has been generated for --with-med_int=int.])
 test x"$WITH_SWIG" = x0 && test x"$enable_python" = xyes && test x"$BUILD_WITH_PMED" == "xyes" && AC_MSG_ERROR([Either use --disable-python or --with-swig=[yes|<swigdir>]. Swig is needed because embedded python interfaces has been generated without MPI.])
#Si la version de python trouvée n'est pas compatible avec les interfaces générées embarquées, arrête la configuration
# PYTHON_IS_VERSION_REF est en fait >= VERSION_REF
 test x"$WITH_SWIG" = x0 && test x"$enable_python" = xyes && test x"$PYTHON_IS_VERSION_REF" == "xno" && AC_MSG_ERROR([Either use --disable-python, PYTHON_VERSION=, or --with-swig=[yes|<swigdir>]. Swig is needed because the detected python version is not compatible with the version used when generating the embedded interfaces.])
#Si la version de swig trouvée n'est pas suffisante pour gérer la version de python, arrête la configuration
#En particulier Python3 et swig 2.0.8 ne fonctionne pas pour les absolute import
#mais fontionne à partir de swig 2.0.12
#version_compare : si > -> 1, si < -> 1, si == -> 0
#SWIG_IS_VERSION_REF==yes --> swig is >= swig_version_ref
#                     [echo "$ac_python_version < 3.0.0"]


test x"$WITH_SWIG" = x1 && test x"$enable_python" = xyes &&\
{
  AX_COMPARE_VERSION($ac_python_version,[lt],[3.0.0],
                     # [echo "$ac_python_version < 3.0.0"]
		     [:]
		     ,
		     # [echo "$ac_python_version >= 3.0.0"]
		     [
		      AX_COMPARE_VERSION($available_swig_vernum,[lt],[20012],
		      [
#		      echo "available_swig_vernum($available_swig_vernum) < 2.0.12"
		       AC_MSG_ERROR([Either use --disable-python or --with-swig=[yes|<swigdir>]. The Swig version needed must be at least 2.0.12 if you want to use python version >= 3.])  
		      ],
		      [
		      :
#		      echo "available_swig_vernum($available_swig_vernum) >= 2.0.12"
		      ])
		     ]
		    )	
}
 
# AM_CONDITIONAL([MED_HAVE_PYTHON3],[test x"$PYTHON_IS_VERSION_REF" == "xyes"])
AM_CONDITIONAL([MED_HAVE_PYTHON3],[test x`echo $ac_python_version |sed -n 's/^\(.\).*/\1/p'`  == "x3"])

 AS_IF([test x$enable_python = xyes],
 [

# L'appel à la macro PATH_PYTHON positionne :
# ‘PYTHON_VERSION’
# ‘PYTHON_PREFIX’
# ‘PYTHON_EXEC_PREFIX’
# ‘PYTHON_PLATFORM’
# ‘pythondir’
# ‘pkgpythondir’
# ‘pyexecdir’
# ‘pkgpyexecdir’
##
# 'PYTHON’ variable is used

AM_PATH_PYTHON([$ac_python_version],[
      # AC_SUBST([python_bin],
      #          [`basename ${PYTHON}`] )
               
      # AC_SUBST([PYTHON_PREFIX],
      #          [`${PYTHON} -c "import sys; print (sys.prefix)"`] )
      
      # AC_SUBST([python_includedir],
      #          [`${PYTHON} -c "import sys; print (sys.prefix + \"/include/python\" + str(sys.version_info[[0]]) + \".\" + str(sys.version_info[[1]]))"`] )
      
      # AC_SUBST([python_libdir],
      #          [`${PYTHON} -c "import sys; print (sys.prefix + \"/lib/python\" + str(sys.version_info[[0]]) + \".\" + str(sys.version_info[[1]]) + \"/m4\")"`] )
      
      # AC_SUBST([python_version],
      #          [`${PYTHON} -c "import sys; print (str(sys.version_info[[0]]) + \".\" + str(sys.version_info[[1]]))"`] )
  ],[AC_MSG_FAILURE([PYTHON interpreter requested, but couldn't find python >=M4MED_PYTHON_VERSION_REF
  ])
  ])

  # Pour l'affichage du Summary
  BUILD_PYTHONTESTS_CONDITIONAL_TRUE=
  BUILD_PYTHONTESTS_CONDITIONAL_FALSE='#'
 ],[
  # Pour l'affichage du Summary
  BUILD_PYTHONTESTS_CONDITIONAL_TRUE='#'
  BUILD_PYTHONTESTS_CONDITIONAL_FALSE=
  PYTHON="Python not available"
 ])
 
 AC_SUBST([BUILD_PYTHONTESTS_CONDITIONAL_TRUE])
 AC_SUBST([BUILD_PYTHONTESTS_CONDITIONAL_FALSE])
 AC_SUBST(PYTHON)

AM_CONDITIONAL([WITH_PYTHON],[test "x$enable_python" = "xyes" ])
 test x"$enable_python" = xyes && AC_DEFINE(MED_HAVE_PYTHON,1,
   [Define if the Python part of MED library must be installed.])
 test ! x"$enable_python" = xyes && AC_DEFINE(MED_HAVE_PYTHON,0,
   [Define if the Python part of MED library must be installed.])

])
