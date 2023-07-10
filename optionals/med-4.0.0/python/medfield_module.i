# -*- coding:utf-8 -*-
%module (package="med") medfield

%include "med_881.i"
%SwigPyIteratorDef(medfield_module)
%include "med_common.i"

%{
#include "medfield.h"
%}

// A définir avant les définitions d'un typemap med_float * scalaire (med_common.i)
/* %med_array_typemaps(medfield,med_float,MEDFLOAT, \  ) */
%apply double * OUTPUT {med_float * const dt}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps
%typemap(freearg) (med_float * const dt) {}

// A définir avant les définitions d'un typemap med_int * scalaire (med_common.i)
/* %med_array_typemaps(medfield,med_int,MEDINT, \   ) */
// [const] med_int paramètre scalaire en IN est tranmis par recopie
// --> const med_int * const est forcément un tableau (cf. %med_array_typemaps(medmesh...) 
// med_int * const peut être scalaire ou array --> Attention à l'ordre de déclaration SWIG
//WISH: On pourrait inverser la déclaration par défaut sur des scalaires et spécifier les tableaux (moins de varaible à nommer)

//SCALAIRE med_int OUT
%define MED_FIELD_MED_INT_SCALAR_LIST \
med_int * const meshnumdt,
med_int * const meshnumit,
med_int * const ncstp,
med_int * const nintegrationpoint,
med_int * const nmesh,
med_int * const numdt,
med_int * const numit,
med_int * const profilesize
%enddef

%apply int *OUTPUT { med_int * const meshnumdt };
%typemap(freearg) med_int * const meshnumdt {}
%apply med_int * const meshnumdt { MED_FIELD_MED_INT_SCALAR_LIST }

// A définir avant les définitions d'un typemap char *
// STRING ARRAY OUT (aucun) :
/* %med_array_typemaps(medfield,char,MEDCHAR,name) */

%med_char_typemaps(medfield,\           ,MED_NAME_SIZE)
//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medfield,dtunit      ,MED_SNAME_SIZE)
//TODO : Taille de axisname,axisunit en fonction de ncomponent pour le argout ?
 //Pas forcément (réallocation dans le wrappeur d'une nouvelle chaîne)
%med_char_typemaps(medfield,componentname      ,MED_SNAME_SIZE*300)
%med_char_typemaps(medfield,componentunit      ,MED_SNAME_SIZE*300)


//TODO : Générer automatiquement ce fichiers.
%include "medfield_exception.i"

%include "medfield.h"

/* MEDfieldCr( const med_idt fid,const char * const fieldname,const med_field_type fieldtype,const med_int ncomponent,const char * const componentname,const char * const componentunit,const char * const dtunit,const char * const meshname); */
/* MEDfieldValueAdvancedWr(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_float dt,const med_entity_type entitype,const med_geometry_type geotype,const char * const localizationname,const med_filter* const filter,const unsigned char* const value); */
/* MEDfieldValueWithProfileWr(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_float dt,const med_entity_type entitype,const med_geometry_type geotype,const med_storage_mode storagemode,const char * const profilename,const char * const localizationname,const med_switch_mode switchmode,const med_int componentselect,const med_int nentity,const unsigned char* const value); */
/* MEDfieldValueWr(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_float dt,const med_entity_type entitype,const med_geometry_type geotype,const med_switch_mode switchmode,const med_int componentselect,const med_int nentity,const unsigned char* const value); */
/* MEDfieldValueAdvancedRd(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const med_filter* const filter,unsigned char* const value); */
/* MEDfieldValueWithProfileRd(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const med_storage_mode storagemode,const char * const profilename,const med_switch_mode switchmode,const med_int componentselect,unsigned char* const value); */
/* MEDfield23ValueWithProfileRd(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const char* const meshname,const med_storage_mode storagemode,const char * const profilename,const med_switch_mode switchmode,const med_int componentselect,unsigned char* const value); */
/* MEDfieldValueRd(const med_idt fid,const char* const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const med_switch_mode switchmode,const med_int componentselect,unsigned char* const value); */
/* MEDfieldnComponentByName(const med_idt fid,const char * const fieldname); */
/* MEDC_EXPORT med_intMEDfieldnComponent(const med_idt fid,const int ind); */
/* MEDfieldInfo(const med_idt fid,const int ind,char * const fieldname,char * const meshname,med_bool * const localmesh,med_field_type * const fieldtype,char * const componentname,char * const componentunit,char * const dtunit,med_int * const ncstp); */
/* MEDfieldComputingStepInfo(const med_idt fid,const char * const fieldname,const int csit,med_int * const numdt,med_int * const numit,med_float * const dt); */
/* MEDfieldComputingStepMeshInfo(const med_idt fid,const char * const fieldname,const int csit,med_int * const numdt,med_int * const numit,med_float * const dt,med_int * const meshnumdt,med_int * const meshnumit); */
/* MEDfield23ComputingStepMeshInfo(const med_idt fid,const char * const fieldname,const int csit,med_int * const numdt,med_int * const numit,med_float * const dt,med_int * const nmesh,char * const meshname,med_bool* const localmesh,med_int * const meshnumdt,med_int * const meshnumit); */
/* MEDfieldComputingStepMeshWr(const med_idt fid,const char * const fieldname,med_int numdt,med_int numit,med_int meshnumdt,med_int meshnumit); */
/* _MEDfieldComputingStepCheck236(med_idt fid,const char * const fieldname,med_size * const ncpst,med_bool checkmultiplemesh,med_bool * const multiplemesh,med_bool checkmeshname,med_bool * const samedefaultmeshname); */
/* MEDfieldInfoByName(const med_idt fid,const char * const fieldname,char * const meshname,med_bool * const localmesh,med_field_type * const fieldtype,char * const componentname,char * const componentunit,char * const dtunit,med_int * const ncstp); */
/* _MEDfieldnValue(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,char * const profilename,const int profileit,const med_storage_mode storagemode,med_int * const profilesize,char * const localizationname,med_int * const nintegrationpoint); */
/* _MEDfield23nValue(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const char * const meshname,char * const profilename,const int profileit,const med_storage_mode storagemode,med_int * const profilesize,char * const localizationname,med_int * const nintegrationpoint); */
/* MEDfieldnValueWithProfileByName(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const char * const profilename,const med_storage_mode storagemode,med_int * const profilesize,char * const localizationname,med_int * const nintegrationpoint); */
/* MEDfieldnValueWithProfile(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const int profileit,const med_storage_mode storagemode,char * const profilename ,med_int * const profilesize,char * const localizationname,med_int * const nintegrationpoint); */
/* MEDfieldnValue(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype); */
/* MEDfield23nValueWithProfile(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const char * const meshname,const int profileit,const med_storage_mode storagemode,char * const profilename ,med_int * const profilesize,char * const localizationname,med_int * const nintegrationpoint); */
/* MEDfieldnProfile(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type enttype,const med_geometry_type geotype,char * const defaultprofilename,char * const defaultlocalizationname); */
/* MEDfield23nProfile(const med_idt fid,const char * const fieldname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const int meshit,char * const meshname,char * const defaultprofilename,char * const defaultlocalizationname); */
/* MEDfieldInterpWr(const med_int fid,const char* const fieldname,const char* interpname); */
/* MEDfieldnInterp(const med_int fid,const char* const fieldname); */
/* MEDfieldInterpInfo( const med_int fid,const char* const fieldname,int interpit,char* const interpname ); */

// Liste des arguments apparaissant dans med_field.h :

// STRINGS :
//OUT (taille particulière) :
//char * const componentname
//char * const componentunit
//char * const dtunit
//
//OUT (taille standard) :
//char * const defaultlocalizationname
//char * const defaultprofilename
//char * const fieldname
//char * const interpname
//char * const localizationname
//char * const meshname
//char * const profilename
//
//IN :
//const char * const componentname
//const char * const componentunit
//const char * const dtunit
//const char * const fieldname
//const char * const localizationname
//const char * const meshname
//const char * const profilename
//const char * const interpname

//BOOL IN:
// Pas d'ambiguité, transmission par recopie
//med_bool checkmeshname
//med_bool checkmultiplemesh
//BOOL OUT:
//med_bool * const localmesh
//med_bool * const multiplemesh
//med_bool * const samedefaultmeshname
//
// Pas d'ambiguité, transmission par recopie
//const med_float dt
// SCALAIRE(OUT)
//med_float * const dt

// OPAQUE :
//const med_idt fid

// Pas d'ambiguité possible pour const med_int * const : ARRAY IN
//AUCUN
// Pas d'ambiguité, transmission par recopie SCALAIRE
//const med_int componentselect
//const med_int ncomponent
//const med_int nentity
//const med_int numdt
//const med_int numit
//
// SCALAIRE OUT ;
//med_int * const meshnumdt
//med_int * const meshnumit
//med_int * const ncstp
//med_int * const nintegrationpoint
//med_int * const nmesh
//med_int * const numdt
//med_int * const numit
//med_int * const profilesize
//
// ITERATEUR DE TYPE INT : RIEN A FAIRE (par valeur)
//const int csit
//const int ind
//const int meshit
//const int profileit
//const int interpit

// ARRAY OUT :
//AUCUN

//LES ENUMS IN/OUT sont gérés par la même macros

//ENUMS IN :

//const med_entity_type entitype
//const med_entity_type enttype
//const med_field_type fieldtype
//const med_storage_mode storagemode
//const med_switch_mode switchmode

//ENUMS OUT :
/* med_field_type * const fieldtype */

//DEFINE
//const med_geometry_type geotype

//STRUCT
//const med_filter * const filter

//IN :
//const unsigned char* const value
//OUT :
//unsigned char* const value
// TODO: A VERIFIER :
//med_size * const ncpst
