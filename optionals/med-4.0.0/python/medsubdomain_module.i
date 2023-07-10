# -*- coding:utf-8 -*-
%module (package="med") medsubdomain

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medsubdomain_module)
%include "med_common.i"

%{
#include "medsubdomain.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medsubdomain...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

//SCALAIRE med_int OUT
%define MED_SUBDOMAIN_MED_INT_SCALAR_LIST \
med_int * const domainnumber,
med_int * const ncorrespondence ,
med_int * const nentity,
med_int * const nocstpncorrespondence,
med_int * const nstep,
med_int * const numdt,
med_int * const numit,
med_geometry_type * const localgeotype,
med_geometry_type * const remotegeotype
%enddef

%apply med_int * OUTPUT {med_int * const nentity};
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const nentity) {}
%apply med_int * const nentity { MED_SUBDOMAIN_MED_INT_SCALAR_LIST }


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medsubdomain,\           ,MED_NAME_SIZE)
%med_char_typemaps(medmesh,description ,MED_COMMENT_SIZE)

%include "medsubdomain_exception.i"

%include "medsubdomain.h"

//MEDsubdomainJointCr(const med_idt fid,const char * const localmeshname,const char * const jointname,const char * const description,const med_int domainnumber,const char * const remotemeshname);
//MEDsubdomainCorrespondenceWr(const med_idt fid,const char * const meshname,const char * const jointname,const med_int numdt,const med_int numit,const med_entity_type localentitytype,const med_geometry_type localgeotype,const med_entity_type remoteentitytype,const med_geometry_type remotegeotype,const med_int nentity,const med_int * const correspondence);
//MEDsubdomainJointInfo(const med_idt fid,const char * const meshname,const int jointit,char * const jointname,char * const description,med_int * const domainnumber,char * const remotemeshname,med_int * const nstep,med_int * const nocstpncorrespondence);
//MEDsubdomainComputingStepInfo(const med_idt fid,const char * const meshname,const char * const jointname,const int csit,med_int * const numdt,med_int * const numit,med_int * const ncorrespondence );
//MEDsubdomainCorrespondenceSizeInfo(const med_idt fid,const char * const meshname,const char * const jointname,const med_int numdt,const med_int numit,const int corit,med_entity_type * const localentitytype,med_geometry_type * const localgeotype,med_entity_type * const remoteentitytype,med_geometry_type * const remotegeotype,med_int * const nentity );
//MEDsubdomainCorrespondenceSize(const med_idt fid,const char * const meshname,const char * const jointname,const med_int numdt,const med_int numit,const med_entity_type localentitytype,const med_geometry_type localgeotype,const med_entity_type remoteentitytype,const med_geometry_type remotegeotype,med_int * const nentity );
//MEDsubdomainCorrespondenceRd(const med_idt fid,const char * const meshname,const char * const jointname,const med_int numdt,const med_int numit,const med_entity_type localentitytype,const med_geometry_type localgeotype,const med_entity_type remoteentitytype,const med_geometry_type remotegeotype,med_int * const correspondence);
//


// Liste des arguments apparaissant dans med_subdomain.h :

//Chaînes en OUT (taille standard) :
//char * const jointname
//char * const remotemeshname

//Chaînes en OUT (taille particulière) :
//char * const description

//Chaînes en IN (taille particulière) :
//const char * const description

//Chaînes en IN :
//const char * const jointname
//const char * const localmeshname
//const char * const meshname
//const char * const remotemeshname

//
//Scalaire en IN:
//const int corit
//const int csit
//const int jointit
//const med_geometry_type localgeotype
//const med_geometry_type remotegeotype
//const med_idt fid
//const med_int nentity
//const med_int numdt
//const med_int numit
//const med_int domainnumber


//Scalaire en OUT:
//med_geometry_type * const localgeotype
//med_geometry_type * const remotegeotype
//
//med_int * const domainnumber
//med_int * const ncorrespondence 
//med_int * const nentity 
//med_int * const nocstpncorrespondence
//med_int * const nstep
//med_int * const numdt
//med_int * const numit


//Enums en IN :
//const med_entity_type remoteentitytype

//Enums en OUT :
//med_entity_type * const localentitytype
//med_entity_type * const remoteentitytype

//Tableaux en IN
//const med_int * const correspondence

//Tableaux en OUT
//med_int * const correspondence
