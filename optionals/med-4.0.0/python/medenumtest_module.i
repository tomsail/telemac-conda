%module (package="med") medenumtest

%include "typemaps.i"

%{
#include "med.h"
#include <utility>
%}

%include "H5public_extract.h"

%include "med_config.h"
%include "medC_win_dll.h"
%include "med.h"


%include "med_enumtest_typemap.i"

//Gestion des types enums en ARGOUT
//Swig crée les MOT CLES/VARIABLES pour les paramètres IN
%{
  typedef std::pair<int, const char * const > enum_;
%}

/* %{ */
/*   const enum_ MED_MESH_TYPE_init[] = { */
/*     enum_(0 ,"MED_UNSTRUCTURED_MESH"), */
/*     enum_(1 ,"MED_STRUCTURED_MESH"), */
/*     enum_(-1,"MED_UNDEF_MESH_TYPE") */
/*   }; */
/* %} */
/* %med_enumtest_typemaps(medenumtest,med_mesh_type,MED_MESH_TYPE) */

/* %{ */
/*   med_mesh_type f1(int i) { return med_mesh_type(i);} */
/*   med_err f2(int i, med_mesh_type * mtype) {*mtype=med_mesh_type(i);return 0;} */
/* %} */

/* med_mesh_type f1(int i); */
/* med_err f2(int i, med_mesh_type * const mtype); */
