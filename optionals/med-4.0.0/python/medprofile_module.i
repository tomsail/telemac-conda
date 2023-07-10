# -*- coding:utf-8 -*-
%module (package="med") medprofile

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medprofile_module)
%include "med_common.i"

%{
#include "medprofile.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medprofile...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

%apply med_int * OUTPUT {med_int * const profilesize}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const profilesize) {}


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medprofile,\           ,MED_NAME_SIZE)


%include "medprofile_exception.i"

%include "medprofile.h"

// Liste des arguments apparaissant dans med_profile.h :
/* MEDprofileInfo(const med_idt fid,const int profileit,char * const profilename,med_int * const profilesize ); */
/* MEDprofileWr(const med_idt fid,const char* const profilename,const med_int profilesize,const med_int* const profilearray); */
/* MEDprofileSizeByName(const med_idt fid,const char * const profilename); */
/* MEDprofileRd(const med_idt fid,const char * const profilename,med_int * const profilearray ); */


//Chaînes en OUT (taille standard) :
//char * const profilename

//Chaînes en OUT (taille particulière) :
// Nil
//Tableau de Chaînes en OUT (taille particulière MED_LNAME_SIZE) :
//Nil

//Chaînes en IN :
//const char * const profilename

//Tableau de Chaînes en IN (taille particulière MED_LNAME_SIZE) :
// Nil

//Bool IN (recopie)
//Nil
//Bool OUT
//Nil

//Scalaire IN :
// const int profileit
// const med_idt fid
// const med_int profilesize

//Scalaire en OUT:
// med_int *const profilesize

//Tableaux en OUT
// med_int * const profilearray

//Tableaux en IN
// const med_int * const profilearray

