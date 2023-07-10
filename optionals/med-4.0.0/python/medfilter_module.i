# -*- coding:utf-8 -*-
%module (package="med") medfilter

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medfilter_module)
%include "med_common.i"

%{
#include "medfilter.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medfilter...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)
//SCALAIRE med_int OUT

/* %apply med_int * OUTPUT {med_int * const numdt} */
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
/* %typemap(freearg) (med_int * const numdt) {} */
/* %apply med_int * const numdt { MED_FILTER_MED_INT_SCALAR_LIST } */

/* %apply med_float * OUTPUT {med_float * const dt} */

// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medfilter,\           ,MED_NAME_SIZE)


%include "medfilter_exception.i"

%ignore MEDfilterAllocate;
%ignore MEDfilterDeAllocate;

%include "medfilter.h"

// Liste des arguments apparaissant dans med_filter.h :

//MEDfilterEntityCr(const med_idt fid,const med_int nentity,const med_int nvaluesperentity,const med_int nconstituentpervalue,const med_int constituentselect,const med_switch_mode switchmode,const med_storage_mode storagemode,const char * const profilename,const med_int filterarraysize,const med_int *filterarray,med_filter* const filter);
//MEDfilterBlockOfEntityCr(const med_idt fid,const med_int nentity,const med_int nvaluesperentity,const med_int nconstituentpervalue,const med_int constituentselect,const med_switch_mode switchmode,const med_storage_mode storagemode,const char * const profilename,const med_size start,const med_size stride,const med_size count,const med_size blocksize,const med_size lastblocksize,med_filter* const filter);
//MEDfilterDeAllocate(const int nfilter,med_filter * filter);
//MEDfilterAllocate(const int nfilter);
//MEDfilterClose( med_filter * const filter);
//



//Chaînes en OUT (taille standard) :

//Chaînes en OUT (taille particulière) :

//Tableau de Chaînes en OUT (taille particulière ) :
//Nil

//Chaînes en IN :
//const char * const profilename

//Chaînes en OUT (taille particulière  :

//Tableau de Chaînes en IN (taille particulière MED_LNAME_SIZE) :
// Nil

//Bool IN (recopie)
//Nil
//Bool OUT
//Nil

//Scalaire IN :
//const int nfilter
//const med_idt fid
//const med_int constituentselect
//const med_int filterarraysize
//const med_int nconstituentpervalue
//const med_int nentity
//const med_int nvaluesperentity
//const med_size blocksize
//const med_size count
//const med_size lastblocksize
//const med_size start
//const med_size stride

//Scalaire en OUT:

//Tableaux en OUT

//Tableaux en IN
//const med_int *filterarray

//enums
//const med_storage_mode storagemode
//const med_switch_mode switchmode

//struct opaque
//med_filter* const filter
//med_filter * filter
