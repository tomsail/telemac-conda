AC_DEFUN([MED_CHECK_PYTHON_DEVEL],[

  PYTHON_IS_VERSION_REF="no" 
  PYTHON_IS_VERSION_MIN="no"
  
# L'appel à axpythondevel doit devancer l'appel à 
# axswigpython qui appelle aussi cette macro sans argument si aucun appel préalable n'a été effectué 
# PYTHON_CPPFLAGS   et PYTHON_LDFLAGS         for python C/API
# PYTHON_EXTRA_LIBS et PYTHON_EXTRA_LDFLAGS   for embedding Python in your code.
# PYTHON_SITE_PKG

  AX_PYTHON_DEVEL([>= 'M4MED_PYTHON_VERSION_REF'])
  # echo " ------ python_arg_version_not_found : $python_arg_version_not_found --------- "
  AS_IF([test x"$python_arg_version_not_found" == "xTrue"],[
     AX_PYTHON_DEVEL([>= 'M4MED_PYTHON2_VERSION_REF'])
     PYTHON_IS_VERSION_MIN="yes"],
     [PYTHON_IS_VERSION_REF="yes"])
  # echo "<---- python_arg_version_not_found : $python_arg_version_not_found --->"
  test x"$python_arg_version_not_found" == "xTrue" &&  AC_MSG_ERROR([SWIG has been enabled, then we need python devel package with C development libraries. Neither the minimal version : >= 'M4MED_PYTHON2_VERSION_REF' nor a version >= 'M4MED_PYTHON_VERSION_REF' has been found !])
   # echo "--- PYTHON_IS_VERSION_MIN=$PYTHON_IS_VERSION_MIN ; PYTHON_IS_VERSION_REF=$PYTHON_IS_VERSION_REF ---"

])
