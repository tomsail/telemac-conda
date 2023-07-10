# -*- coding:utf-8 -*-

%module (package="med") medstructelement

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medstructelement_module)
%include "med_common.i"

%{
#include "medstructelement.h"
%}

//%apply double * OUTPUT {med_float * const dt}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
//%typemap(freearg) (med_float * const dt) {}

// [const] med_int paramètre scalaire en IN est tranmis par recopie
// --> const med_int * const est forcément un tableau (cf. %med_array_typemaps(medstructelement...) de med_common.i)
// med_int * const peut être scalaire ou array --> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

//SCALAIRE med_int OUT
%define MED_STRUCTELEMENT_MED_INT_SCALAR_LIST \
med_int* const ncomponent,
med_int* const nconstantattribute,
med_int* const nvariableattribute,
med_int* const profilesize, 
med_int* const sncell,
med_int* const snnode
%enddef

%apply int *OUTPUT { med_int * const modeldim };
%typemap(freearg) med_int * const modeldim {}
%apply med_int * const modeldim { MED_STRUCTELEMENT_MED_INT_SCALAR_LIST }

// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const

%include "medstructelement_exception.i"

%include "medstructelement.h"


// Liste des arguments apparaissant dans med_structelement.h :

// STRINGS :

//OUT (taille standard) :
// char* const constattname 
// char* const modelname 
// char* const profilename 
// char* const supportmeshname 
// char* const varattname 

//IN :
// const char* const constattname 
// const char* const modelname 
// const char* const profilename 
// const char* const supportmeshname 
// const char* const varattname 

//BOOL IN:
// Pas d'ambiguité, transmission par recopie
//BOOL OUT:
// med_bool* const anyprofile 

// Pas d'ambiguité possible pour const med_float * const : ARRAY

//Défini dans medfile
// const med_idt fid 

// Pas d'ambiguité, transmission par recopie SCALAIRE
// const int attit 
// const int mit 
// const med_int modeldim 
// const med_int ncomponent 
// const med_int ncomponent  

// SCALAIRE EN OUT :
// med_int* const modeldim 
// med_int* const ncomponent 
// med_int* const nconstantattribute 
// med_int* const nvariableattribute  
// med_int* const profilesize  
// med_int* const sncell 
// med_int* const snnode 

//LES ENUMS IN/OUT sont gérés par la même macros

//ENUMS IN :
// const med_attribute_type constatttype 
// const med_attribute_type varatttype 
// const med_entity_type    sentitytype 
// const med_geometry_type  mgeotype 
// const med_geometry_type  sgeotype 
// const med_attribute_type atttype 

//ENUMS OUT :
// med_attribute_type* const constatttype 
// med_attribute_type* const varatttype 
// med_entity_type*    const sentitytype 
// med_geometry_type * const mgeotype 
// med_geometry_type*  const sgeotype 

//TODO :
//IN:
// const void* const value 
//OUT:
// void* const value  
