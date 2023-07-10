# -*- coding:utf-8 -*-
%module (package="med") medlocalization

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medlocalization_module)
%include "med_common.i"

%{
#include "medlocalization.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medlocalization...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

//SCALAIRE med_int OUT
%define MED_LOCALIZATION_MED_INT_SCALAR_LIST \
med_int * const nsectionmeshcell,
med_int * const spacedimension,
med_geometry_type * const geotype,
med_geometry_type * const sectiongeotype
%enddef

%apply med_int * OUTPUT {med_int * const nipoint}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const nipoint) {}
%apply med_int * const nipoint { MED_LOCALIZATION_MED_INT_SCALAR_LIST }


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medlocalization,\           ,MED_NAME_SIZE)


%include "medlocalization_exception.i"

%include "medlocalization.h"

// Liste des arguments apparaissant dans med_localization.h :
//MEDlocalizationWr(const med_idt fid,const char * const localizationname,const med_geometry_type geotype,const med_int spacedimension,const med_float* const elementcoordinate,const med_switch_mode switchmode,const med_int nipoint,const med_float* const ipointcoordinate,const med_float* const weight,const char * const geointerpname,const char * const ipointstructmeshname);
//MEDlocalizationRd(const med_idt fid,const char* const localizationname,const med_switch_mode switchmode,med_float* const elementcoordinate,med_float* const ipointcoordinate,med_float* const weight);
//MEDlocalizationInfo(const med_idt fid,const int localizationit,char * const localizationname,med_geometry_type * const geotype,med_int * const spacedimension,med_int * const nipoint,char * const geointerpname,char * const sectionmeshname,med_int * const nsectionmeshcell,med_geometry_type * const sectiongeotype);
//MEDlocalizationInfoByName(const med_idt fid,const char * const localizationname,med_geometry_type * const geotype,med_int * const spacedimension,med_int * const nipoint,char * const geointerpname,char * const sectionmeshname,med_int * const nsectionmeshcell,med_geometry_type * const sectiongeotype);
//


//Chaînes en OUT (taille standard) :
//char * const localizationname
//char * const geointerpname
//char * const sectionmeshname

//Chaînes en OUT (taille particulière) :
// Nil
//Tableau de Chaînes en OUT (taille particulière MED_LNAME_SIZE) :
//Nil

//Chaînes en IN :
//const char * const localizationname
//const char * const geointerpname
//const char * const ipointstructmeshname
//
//Tableau de Chaînes en IN (taille particulière MED_LNAME_SIZE) :
// Nil

//Bool IN (recopie)
//Nil
//Bool OUT
//Nil

//Scalaire IN :
// const int localizationit
// const med_idt fid
// const med_int nipoint
// const med_int spacedimension

//Scalaire en OUT:
//med_int * const nipoint
//med_int * const nsectionmeshcell
//med_int * const spacedimension
//med_geometry_type * const geotype
//med_geometry_type * const sectiongeotype

//
//Tableaux en OUT
//med_float* const elementcoordinate
//med_float* const ipointcoordinate
//med_float* const weight

//Tableaux en IN
//const med_float* const elementcoordinate
//const med_float* const ipointcoordinate
//const med_float* const weight

//enum :
//const med_geometry_type geotype
//const med_switch_mode switchmode
