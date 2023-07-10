# -*- coding:utf-8 -*-
%module (package="med") medinterp

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medinterp_module)
%include "med_common.i"

%{
#include "medinterp.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medinterp...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

//SCALAIRE med_int OUT
%define MED_INTERP_MED_INT_SCALAR_LIST \
med_geometry_type* const geotype,
med_int* const maxdegree,
med_int* const nbasisfunc,
med_int* const nmaxcoef,
med_int* const nvariable,
med_int* const ncoef
%enddef

%apply med_int * OUTPUT {med_int* const ncoef};
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const ncoef) {}
%apply med_int * const ncoef { MED_INTERP_MED_INT_SCALAR_LIST }


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medinterp,\           ,MED_NAME_SIZE)
/* %med_char_typemaps(medmesh,description ,MED_COMMENT_SIZE) */

%include "medinterp_exception.i"

%include "medinterp.h"

//MEDinterpCr(const med_idt fid,const char* const interpname,const med_geometry_type geotype,const med_bool cellnodes,const med_int nvariable,const med_int maxdegree,const med_int nmaxcoef );
//MEDinterpBaseFunctionWr( const med_idt fid,const char* const interpname,const med_int basisfuncit,const med_int ncoef,const med_int* const power,const med_float* const coefficient);
//MEDinterpInfo(const med_idt fid,const int interpit,char* const interpname,med_geometry_type* const geotype,med_bool* const cellnode,med_int* const nbasisfunc,med_int* const nvariable,med_int* const maxdegree,med_int* const nmaxcoef );
//MEDinterpInfoByName(const med_idt fid,const char* const interpname,med_geometry_type* geotype,med_bool* const cellnode,med_int* const nbasisfunc,med_int* const nvariable,med_int* const maxdegree,med_int* const nmaxcoef );
//MEDinterpBaseFunctionRd( const med_idt fid,const char* const interpname,const int basisfuncit,med_int* ncoef,med_int* const power,med_float* const coefficient);
//MEDinterpBaseFunctionCoefSize( const med_idt fid,const char* const interpname,const med_int basisfuncit);
//
// Liste des arguments apparaissant dans med_interp.h :

//Chaînes en OUT (taille standard) :
//char* const interpname

//Chaînes en OUT (taille particulière) :

//Chaînes en IN (taille particulière) :

//Chaînes en IN :
//const char* const interpname

//
//Scalaire en IN:
//const int basisfuncit
//const int interpit
//const med_bool cellnodes
//const med_geometry_type geotype
//const med_idt fid
//const med_int basisfuncit
//const med_int maxdegree
//const med_int ncoef
//const med_int nmaxcoef
//const med_int nvariable
//

//Scalaire en OUT:
//med_bool* const cellnode
//med_geometry_type* const geotype
//med_int* const maxdegree
//med_int* const nbasisfunc
//med_int* const nmaxcoef
//med_int* const nvariable
//med_int* const ncoef
//
//Enums en IN :

//Enums en OUT :

//Tableaux en IN
//const med_float* const coefficient
//const med_int* const power

//Tableaux en OUT
//med_int * const correspondence
//med_float* const coefficient
//med_int* const power
