# -*- coding:utf-8 -*-
%module (package="med") medlink

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medlink_module)
%include "med_common.i"

%{
#include "medlink.h"
%}

// TODO : tester en int/long
// [const] med_int paramètre scalaire en IN est tranmis par recopie
//  const  med_int * const est forcément un tableau (cf. %med_array_typemaps(medlink...) de med_common.i)
//         med_int * const peut être scalaire ou array
//--> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

%apply med_int * OUTPUT {med_int * const linksize}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_int * const linksize) {}


// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const
/* %med_array_typemaps(char,MEDCHAR,groupname) */
/* %med_array_typemaps(char,MEDCHAR,attributedes) */

//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medlink,\           ,MED_NAME_SIZE)


%include "medlink_exception.i"

%include "medlink.h"

// Liste des arguments apparaissant dans med_link.h :

 //MEDlinkWr(const med_idt fid,const char * const meshname,const char * const link);
 //MEDlinkRd(const med_idt fid,const char* const meshname,char* const link);
 //MEDlinkInfoByName(const med_idt fid,const char * const meshname );
 //MEDlinkInfo(const med_idt fid,const int linkit,char * const meshname,med_int * const linksize );



//Chaînes en OUT (taille standard) :
//char * const link
//char * const meshname

//Chaînes en OUT (taille particulière) :
// Nil
//Tableau de Chaînes en OUT (taille particulière MED_LNAME_SIZE) :
//Nil

//Chaînes en IN :
//const char * const link
//const char * const meshname

//Tableau de Chaînes en IN (taille particulière MED_LNAME_SIZE) :
// Nil

//Bool IN (recopie)
//Nil
//Bool OUT
//Nil

//Scalaire IN :
// const int linkit
// const med_idt fid

//Scalaire en OUT:
// med_int *const linksize

//Tableaux en OUT

//Tableaux en IN

