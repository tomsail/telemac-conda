# -*- coding:utf-8 -*-
%module (package="med") medfamily

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medfamily_module)
%include "med_common.i"

%{
#include "medfamily.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medfamily...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

%apply med_int * OUTPUT {med_int * const familynumber}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const familynumber) {}


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
%med_array_typemaps(char,MEDCHAR,groupname)
%med_array_typemaps(char,MEDCHAR,attributedes)

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medfamily,\           ,MED_NAME_SIZE)


%include "medfamily_exception.i"

%include "medfamily.h"

// Liste des arguments apparaissant dans med_family.h :

// MEDfamilyCr(const med_idt fid,const char * const meshname,const char * const familyname,const med_int familynumber,const med_int ngroup,const char * const groupname);
// MEDfamilyInfo(const med_idt fid,const char * meshname,const int famit,char * const familyname,med_int *const familynumber,char *const groupname);
// MEDfamily23Info( const med_idt fid,const char* const meshname,const int famit,char* const familyname,med_int* const attributenumber,med_int* const attributevalue,char* const attributedes,med_int* const familynumber,char* const groupname);

// MEDnFamily(const med_idt fid,const char * const meshname);
// MEDnFamilyGroup(const med_idt fid,const char * const meshname,const int famit);
// MEDnFamily23Attribute(const med_idt fid,const char * const meshname,const int famit);


//Chaînes en OUT (taille standard) :
//char * const familyname

//Chaînes en OUT (taille particulière) :
//Tableau de Chaînes en OUT (taille particulière MED_COMMENT_SIZE) :
//char * const attributedes
//Tableau de Chaînes en OUT (taille particulière MED_LNAME_SIZE) :
//char * const groupname

//Chaînes en IN :
//const char * const meshname
//const char * const familyname
//Tableau de Chaînes en IN (taille particulière MED_LNAME_SIZE) :
// const char * const groupname

//Bool IN (recopie)
//Nil
//Bool OUT
//Nil

//Scalaire IN :
// const int famit
// const med_idt fid
// const med_int familynumber
// const med_int ngroup

//Scalaire en OUT:
// med_int *const familynumber

//Tableaux en OUT
// med_int * const attributenumber
// med_int * const attributevalue

