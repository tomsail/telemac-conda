%include "med_config.h"
%include "medC_win_dll.h"
%include "med.h"

// Now list ANSI C/C++ declarations

%typemap(argout) med_bool * {
   PyObject *o, *o2, *o3;
   o = PyBool_FromLong(*$1);
   /* if ((!$result) || ($result == Py_None)) { */
   /*      $result = o; */
   /*  } else { */
   /*      if (!PyTuple_Check($result)) { */
   /*          PyObject *o2 = $result; */
   /*          $result = PyTuple_New(1); */
   /*          PyTuple_SetItem($result,0,o2); */
   /*      } */
   /*      o3 = PyTuple_New(1); */
   /*      PyTuple_SetItem(o3,0,o); */
   /*      o2 = $result; */
   /*      $result = PySequence_Concat(o2,o3); */
   /*      Py_DECREF(o2); */
   /*      Py_DECREF(o3); */
   /*  } */
  $result=SWIG_Python_AppendOutput($result, o);
}

%typemap(in,numinputs=0) med_bool *(med_bool temp) {
    $1 = &temp;
}

