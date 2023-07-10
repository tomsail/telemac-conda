# -*- coding:utf-8 -*-
%module (package="med") medlibrary

%include "med_881.i"
%SwigPyIteratorDef(medlibrary_module)
%include "med_common.i"

%{
#include "medlibrary.h"
%}

%apply int *OUTPUT { med_int *const major, med_int* const minor, med_int* const release };
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const) {}


/* %cstring_bounded_output(char* const version,MED_NAME_SIZE); */
/* %cstring_bounded_output(char* const comment,MED_COMMENT_SIZE); */
/* %cstring_bounded_mutable(const char * const comment,MED_COMMENT_SIZE); */

// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// Définition des typemaps char * :
%med_char_typemaps(medmesh,\           ,MED_NAME_SIZE)
%med_char_typemaps(medmesh,comment ,MED_COMMENT_SIZE)

%include "medlibrary_exception.i"

%include "medlibrary.h"


// Fin Du Mapping.

// Liste des arguments apparaissant dans med_medlibrary.h
//char* const medversion
//char* const version

//med_int* const major
//med_int* const minor
//med_int* const release
//
