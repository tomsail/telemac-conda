
#define AFTERX(x) SwigPyIterator_ ## x
#define XAFTERX(x) AFTERX(x)

%define %SwigPyIteratorDef(Module)
#define SwigPyIterator XAFTERX(Module)
/* cf #881 Multiple modules leads to unhandled exeption */
%include "pyiterators_881.i"
%include "std_vector.i"

%enddef
