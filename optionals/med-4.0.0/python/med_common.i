# -*- coding:utf-8 -*-
%include "typemaps.i"
%include "exception.i"
%include "cstring.i"

//TODO: Cf MEDerreur.hxx
//TODO: Est-il utile d'avoir une classe d'exception mère MED
//      à toutes les exceptions spécifiques des fonctions ?
%define %MedException(funcname)
%exception funcname {
  $action
    if ( result < 0 ) {
      /* fprintf(stderr,"Code erreur MED : %2d\n",result); */
      /* SWIG_exception(SWIG_RuntimeError,"Error returned from MEDfichier API (funcname)."); */
      PyObject* exobj = PyTuple_New(2);
      PyTuple_SetItem(exobj,0,PyString_FromString("Error returned from MEDfichier API (funcname)."));
      PyTuple_SetItem(exobj,1,PyInt_FromLong((long) result));
      SWIG_Python_SetErrorObj(PyExc_RuntimeError,exobj);
      /* PyErr_SetString(PyExc_Exception, str(result));  */
      return NULL;
    }
}
%enddef


%{
#include "med.h"
%}

//Ce fichier est généré : H5public_extract.h
%include "H5public_extract.h"

%include "med_config.h"
%include "medC_win_dll.h"
%include "med.h"

/*Il faut avoir inclu med_config.h pour définir HAVE_SDTINT_H */
#ifdef HAVE_STDINT_H
%include "stdint.i"
#endif

%import "medenum_module.i"

/* Inclusion du std_vector.i pour éviter le message : */
/* error: 'SWIGPY_SLICE_ARG' was not declared in this scope */
/* cf #881 Multiple modules leads to unhandled exeption */
/* import sys,ctypes */
/* sys.setdlopenflags(sys.getdlopenflags() | ctypes.RTLD_LOCAL) */
/* %include "std_vector.i" */
/* %{ */
/* /\* #include <vector> *\/ */
/* /\**\/ */
/* class MED_MESH_TYPE; */
/* %} */
/* %import  "medenumtest_module.i" */

%include "med_bool_typemap.i"
%include "med_array_typemap.i"
%include "med_char_typemap.i"

// Par défaut demande à swig de considérer les types C med_float * et med_int *
// comme des MEDARRAY, chacun des modules devra définir spécifiquement ses types scalaires.
// A définir avant les définitions d'un typemap med_float * scalaire
%med_array_typemaps(med_float,MEDFLOAT, \  )
// A définir avant les définitions d'un typemap med_float32 * scalaire
%pythoncode{
MEDFLOAT64=MEDFLOAT
MEDDOUBLE=MEDFLOAT
}

// A définir avant les définitions d'un typemap med_float32 * scalaire
%med_array_typemaps(med_float32,MEDFLOAT32, \  )

// A définir avant les définitions d'un typemap med_int * scalaire
%med_array_typemaps(med_int,MEDINT, \   )
#if defined(HAVE_F77INT64)
%pythoncode{
MEDINT64=MEDINT
}
%med_array_typemaps(med_int32,MEDINT32, \   )
#else
%pythoncode{
MEDINT32=MEDINT
}
#if defined(MED_SWIG_INT64)
%med_array_typemaps(med_int64,MEDINT64, \   )
#else
%pythoncode{
MEDINT64=None
}
#endif
#endif
// A définir avant les définitions d'un typemap med_int * scalaire

// Définir les MEDCHAR avant les définitions d'un typemap char *
// STRING ARRAY OUT :
%med_array_typemaps(char,MEDCHAR,name)
%med_array_typemaps(char,MEDCHAR,elementname)
%med_array_typemaps(char,MEDCHAR,nodename)

//Désactive toutes les sorties d'erreur
//les erreurs sont gérées par les exceptions
%typemap(out) med_err {
  Py_INCREF(Py_None);
  $result=Py_None;
}

//Ajoute la fonctionnalité de passage d'arguments par mot clés
//sur toutes les fonctions MED
%feature ("kwargs");
//Autodocumente les fonctions MED.
%feature("autodoc", "2");
%typemap(doc) med_err "Runtime Exception";


