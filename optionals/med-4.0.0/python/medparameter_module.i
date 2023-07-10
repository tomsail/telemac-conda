# -*- coding:utf-8 -*-
%module (package="med") medparameter

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medparameter_module)
%include "med_common.i"

%{
#include "medparameter.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medparameter...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)
//SCALAIRE med_int OUT
%define MED_PARAMETER_MED_INT_SCALAR_LIST \
med_int * const numdt,
med_int * const numit,
med_int * const nstep,
med_float * const dt
%enddef

%apply med_int * OUTPUT {med_int * const numdt}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const numdt) {}
%apply med_int * const numdt { MED_PARAMETER_MED_INT_SCALAR_LIST }

%apply med_float * OUTPUT {med_float * const dt}

// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medparameter,\           ,MED_NAME_SIZE)
%med_char_typemaps(medparameter,description ,MED_COMMENT_SIZE)
%med_char_typemaps(medparameter,dtunit      ,MED_SNAME_SIZE)


%include "medparameter_exception.i"

%include "medparameter.h"

// Liste des arguments apparaissant dans med_parameter.h :

/* MEDparameterCr(const med_idt fid,const char * const paramname,const med_parameter_type paramtype,const char* const description,const char * const dtunit ); */
/* MEDparameterInfo(const med_idt fid,const int paramit,char * const paramname,med_parameter_type * const paramtype,char * const description,char * const dtunit,med_int * const nstep); */
/* MEDparameterInfoByName(const med_idt fid,const char * const paramname,med_parameter_type * const paramtype,char * const description,char * const dtunit,med_int * const nstep); */
/* MEDparameterValueWr(const med_idt fid,const char* const paramname,const med_int numdt,const med_int numit,const med_float dt,const unsigned char* const value); */
/* MEDparameterValueRd(const med_idt fid,const char* const paramname,const med_int numdt,const med_int numit,unsigned char* const value); */
/* MEDparameterComputationStepInfo(const med_idt fid,const char * const paramname,const int csit,med_int * const numdt,med_int * const numit,med_float * const dt ); */





//Chaînes en OUT (taille standard) :
//char * const paramname

//Chaînes en OUT (taille particulière) :
//char * const description
//char * const dtunit

//Tableau de Chaînes en OUT (taille particulière ) :
//Nil

//Chaînes en IN :
//const char * const paramname

//Chaînes en OUT (taille particulière  :
//const char* const description
//const char* const dtunit

//Tableau de Chaînes en IN (taille particulière MED_LNAME_SIZE) :
// Nil

//Bool IN (recopie)
//Nil
//Bool OUT
//Nil

//Scalaire IN :
// const int csit
// const int paramit
// const med_idt fid
// const med_int numdt
// const med_int numit
// const med_float dt

//Scalaire en OUT:
/* med_int * const numdt */
/* med_int * const numit */
/* med_int * const nstep */
/* med_float * const dt */

//Tableaux en OUT
/* unsigned char* const value */

//Tableaux en IN
/* const unsigned char* const value */

//enums
/* const med_parameter_type paramtype */
/* med_parameter_type *const paramtype */
