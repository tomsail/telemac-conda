# -*- coding:utf-8 -*-
%module (package="med") medequivalence

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medequivalence_module)
%include "med_common.i"

%{
#include "medequivalence.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medequivalence...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

//SCALAIRE med_int OUT
%define MED_EQUIVALENCE_MED_INT_SCALAR_LIST \
med_int * const ncorrespondence,
med_int * const nocstpncorrespondence,
med_int * const nstep,
med_int * const numdt,
med_int * const numit
%enddef

%apply med_int * OUTPUT {med_int * const nentity};
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const nentity) {}
%apply med_int * const nentity { MED_EQUIVALENCE_MED_INT_SCALAR_LIST }


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
%med_array_typemaps(char,MEDCHAR,groupname)
%med_array_typemaps(char,MEDCHAR,attributedes)

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medequivalence,\           ,MED_NAME_SIZE)
%med_char_typemaps(medmesh,description ,MED_COMMENT_SIZE)
%med_char_typemaps(medmesh,equivdescription ,MED_COMMENT_SIZE)

%include "medequivalence_exception.i"

%include "medequivalence.h"


//MEDequivalenceCr(const med_idt fid,const char * const meshname,const char * const equivname,const char * const description);
//MEDequivalenceCorrespondenceWr(const med_idt fid,const char * const meshname,const char * const equivname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,const med_int nentity,const med_int * const correspondence);
//MEDequivalenceCorrespondenceRd(const med_idt fid,const char * const meshname,const char * const equivname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,med_int * const correspondence);
//MEDequivalenceInfo(const med_idt fid,const char * const meshname,const int equivit,char * const equivname,char * const equivdescription,med_int * const nstep,med_int * const nocstpncorrespondence);
//MEDequivalenceComputingStepInfo(const med_idt fid,const char * const meshname,const char * const equivname,const int csit,med_int * const numdt,med_int * const numit,med_int * const ncorrespondence );
//MEDequivalenceCorrespondenceSizeInfo(const med_idt fid,const char * const meshname,const char * const equivname,const med_int numdt,const med_int numit,const int corit,med_entity_type * const entitype,med_geometry_type* const geotype,med_int * const nentity );
///* MEDequivalenceInfoByName(const med_idt fid,*//*  const char * const meshname,*//*  const char * const equivname,*//*  char * const equivdescription,*//*  med_int * const nstep,*//*  med_int * const nocstpncorrespondence); */
//MEDequivalenceCorrespondenceSize(const med_idt fid,const char * const meshname,const char * const equivname,const med_int numdt,const med_int numit,const med_entity_type entitype,const med_geometry_type geotype,med_int * const nentity );
//
// Liste des arguments apparaissant dans med_equivalence.h :

//Chaînes en OUT (taille standard) :
//char * const equivname

//Chaînes en OUT (taille particulière) :
//char * const equivdescription

//Chaînes en IN :
//const char * const equivname
//const char * const meshname
//const char * const description
//
//Scalaire en IN:
//const med_idt fid
//const int corit
//const int csit
//const int equivit
//const med_int nentity
//const med_int numdt
//const med_int numit

//Scalaire en OUT:
//med_int * const nentity
//med_int * const ncorrespondence
//med_int * const nocstpncorrespondence
//med_int * const nstep
//med_int * const numdt
//med_int * const numit

//Enums en IN :
//const med_entity_type entitype
//const med_geometry_type geotype

//Enums en OUT :
//med_entity_type * const entitype
//med_geometry_type* const geotype

//Tableaux en IN
//const med_int * const correspondence

//Tableaux en OUT
//med_int * const correspondence
