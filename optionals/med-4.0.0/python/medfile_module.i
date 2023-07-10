# -*- coding:utf-8 -*-
%module (package="med") medfile

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medfile)

%include "med_common.i"
%ignore MEDmemFileOpen;

%{
#include "medfile.h"
%}

%apply int *OUTPUT { med_int *const major, med_int* const minor, med_int* const release };
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const ) {}

// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// Définition des typemaps char * :
%med_char_typemaps(medmesh,\           ,MED_NAME_SIZE)
%med_char_typemaps(medmesh,comment ,MED_COMMENT_SIZE)

// SWIG SPECIFIQUE A MEDfileName : 
// Seule fonction n'utilisant pas une chaïne fixe.
// Réflechir à une solution plus générique sur la base du couple char * const filename, med_int filenamesize
%typemap(in,noblock=1,numinputs=1)  (med_idt fid, char * const filename, med_int filenamesize) ( med_idt arg=0 ,long val=0, int ecode = 0 ) {
  ecode = SWIG_AsVal_long($input, &val);
  if (!SWIG_IsOK(ecode)) {
    SWIG_exception_fail(SWIG_ArgError(ecode), "in method '" "MEDfileName" "', argument " "1"" of type '" "med_idt""'");
  } 
  $1 = static_cast< med_idt >(val);
  $3 = MEDfileName($1,NULL,0);
  $2 = (char *) malloc(sizeof(char)*$3);
}
%typemap(freearg,noblock=1)  (med_idt fid, char * const filename, med_int filenamesize) {
  free($2);
}
%typemap(out,noblock=1)  (med_idt fid, char * const filename, med_int filenamesize) {
}
%typemap(argout,noblock=1)  (med_idt fid, char * const filename, med_int filenamesize) {
  $result = SWIG_FromCharPtrAndSize($2,$3);
}
%typemap(out,noblock=1) med_int MEDfileName {}
  
%feature("autodoc", "MEDfileName(fid) -> str") MEDfileName;
// FIN SPECIFIQUE MEDfileName


/* %cstring_bounded_output(char* const version,MED_NAME_SIZE); */
/* %cstring_bounded_output(char* const comment,MED_COMMENT_SIZE); */
/* //%cstring_bounded_output(char* const filename,MED_PATHNAME_SIZE); */
/* %cstring_bounded_mutable(const char * const comment,MED_COMMENT_SIZE); */

//TODO : MED_MAX_CHFID_PATH
//%cstring_bounded_mutable(const char * const chpath  ,MED_PATHNAME_SIZE);

%include "medfile_exception.i"

%include "medfile.h"

// Fin Du Mapping.

// Liste des arguments apparaissant dans medfile.h
//char* const comment
//char* const version
//char* const filename
//const char * const chpath
//const char* const comment
//const char* const filename
//const char* const objectname,
//const med_access_mode accessmode
//const med_class medclass
//const med_idt chfid
//const med_idt fid
//const med_idt mid
//med_bool* const hdfok
//med_bool* const medok
//med_bool* const objectexist 
//med_idt fid
//med_int* const major
//med_int* const minor
//med_int* const release
//
