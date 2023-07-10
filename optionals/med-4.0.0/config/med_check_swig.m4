#
# MED_CHECK_SWIG( version, path = /usr )
# -------------------------------------
#
AC_DEFUN([_MED_DEFINE_SWIG_WITH],[

  AC_ARG_WITH([swig],
    AC_HELP_STRING([--with-swig@<:@=DIR@:>@], [set SWIG installation directory. @<:@]m4_default([$2], /usr)[@:>@]),
    [with_swig=$withval], [with_swig=no])
  AC_ARG_VAR([SWIGFLAGS],[The list of flags that should be passed to SWIG.])

])

# CHECK_SWIG et CHECK_PYTHON sont appelés dans configure.ac
AC_DEFUN([MED_CHECK_SWIG],
[
 AC_REQUIRE([_MED_DEFINE_SWIG_WITH])
 AC_BEFORE([$0],[MED_CHECK_PYTHON])
 AC_BEFORE([$0],[MED_CHECK_PYTHON_DEVEL])
 AC_BEFORE([$0],[AX_PKG_SWIG])
 AC_BEFORE([$0],[AX_SWIG_PYTHON])
 AC_BEFORE([$0],[AX_PYTHON_DEVEL])

 WITH_SWIG=0
 available_swig_vernum="00000"
 
 SWIG=`noswig() { echo "SWIG not available." ; return 1 ; } ; noswig`

 swig_default_path=m4_default([$2], /usr)
 

AS_IF([test ! x${with_swig} = xno ],
 [  
    # we're trying to find the correct SWIG installation path
    swig_install_path=$swig_default_path
    if test ! x${with_swig} = xyes
    then
      swig_install_path=$with_swig
      PATH="${swig_install_path}/bin:${PATH}"
    fi

    #SWIG_IS_VERSION_REF==yes --> swig is >= swig_version_ref
    AX_PKG_SWIG( m4_default([$$1]),
    		 [SWIG_IS_VERSION_REF=yes],
    		 [SWIG_IS_VERSION_REF=no && (AC_MSG_WARN([SWIG has been enable but can't find attended version : [$$1]])) ]
    		 [AX_PKG_SWIG(m4_default([M4MED_SWIG_MIN_VERSION_REF]),
    		              [SWIG_IS_VERSION_MIN=yes],
    		              [SWIG_IS_VERSION_MIN=no]
    		              [AC_MSG_ERROR([SWIG has been enable but can't find minimal version : M4MED_SWIG_MIN_VERSION_REF])]
    			     )]
                )
    		
      #AX_SWIG_ENABLE_CXX
      #AX_SWIG_MULTI_MODULE_SUPPORT

# L'appel suivant med_check_python_devel/axpythondevel doit devancer l'appel à 
# axswigpython qui appelle aussi cette macro sans argument si aucun appel préalable n'a été effectué 
# PYTHON_CPPFLAGS   et PYTHON_LDFLAGS         for python C/API
# PYTHON_EXTRA_LIBS et PYTHON_EXTRA_LDFLAGS   for embedding Python in your code.
# PYTHON_SITE_PKG

    MED_CHECK_PYTHON_DEVEL

    AX_SWIG_PYTHON

    # test -n "${SWIG}" && WITH_SWIG=1
    eval "${SWIG} -help >/dev/null 2>&1" && WITH_SWIG=1
 ],[
 ])

  AC_SUBST(SWIG)

  # Propagate test into Makefiles
  AM_CONDITIONAL([WITH_SWIG],[test $WITH_SWIG = 1])
  AM_CONDITIONAL([MED_SWIG_GT_30008],[test $available_swig_vernum -gt "30008"])

])

# _MED_SWIG_VERSION
# ----------------
#
AC_DEFUN([_MED_SWIG_VERSION],
[
  AC_REQUIRE([AC_PROG_AWK])
  AC_MSG_CHECKING([Swig version])
  med_swig_version="undefined"
  if ${SWIG} -version >conftest.swigversion 2>&1; then
    med_swig_version=`grep Version conftest.swigversion | cut -d " " -f 3`
  fi
  AC_MSG_RESULT([$med_swig_version])
])
