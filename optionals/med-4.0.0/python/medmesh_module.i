# -*- coding:utf-8 -*-

%module (package="med") medmesh

//A inclure avant toute déclaration à SWIG
%include "med_881.i"
%SwigPyIteratorDef(medmesh_module)
%include "med_common.i"

%{
#include "medmesh.h"
%}

%apply double * OUTPUT {med_float * const dt}
//Pour éviter le mapping sur le freearg de la macro med_array_typemaps (med_common.i)
%typemap(freearg) (med_float * const dt) {}

// [const] med_int paramètre scalaire en IN est tranmis par recopie
// --> const med_int * const est forcément un tableau (cf. %med_array_typemaps(medmesh...) de med_common.i)
// med_int * const peut être scalaire ou array --> Attention à l'ordre de déclaration SWIG (inclure med_common.i avant)

//SCALAIRE med_int OUT
%define MED_MESH_MED_INT_SCALAR_LIST \
med_int * const meshdim,
med_int * const geodim,
med_int * const nnodes,
med_int * const nstep,
med_int * const profilesize,
med_int * const spacedim,
med_int * const numdt,
med_int * const numit,
med_int * const cellmaxnodes,
med_int * const isolatednodes,
med_int * const verticesnodes,
med_geometry_type * const geotype
%enddef

%apply int *OUTPUT { med_int * const meshdim };
%typemap(freearg) med_int * const meshdim {}
%apply med_int * const meshdim { MED_MESH_MED_INT_SCALAR_LIST }

// Définir les tableaux de chaînes agrégées avant les définitions d'un typemap char *
// STRING ARRAY OUT mappés automatiquement sur un char * const


//TODO : Ajouter des contraintes sur la taille des chaines suivantes (sinon elles seront tronckées en C
%med_char_typemaps(medmesh,\           ,MED_NAME_SIZE)
%med_char_typemaps(medmesh,description ,MED_COMMENT_SIZE)
%med_char_typemaps(medmesh,dtunit      ,MED_SNAME_SIZE)
%med_char_typemaps(medmesh,univname    ,MED_LNAME_SIZE)
//TODO : Taille de axisname,axisunit en fonction de spacedim
//TODO:  On devrait les gérer avec les MEDCHAR ?
%med_char_typemaps(medmesh,axisname      ,MED_SNAME_SIZE*3)
%med_char_typemaps(medmesh,axisunit      ,MED_SNAME_SIZE*3)
//TODO
//%med_char_typemaps(medmesh,name          ,MED_SNAME_SIZE*Nombre d'entités)

//TODO : Créer l'import d'un fichier doc par module. Ce fichier sera généré en utilisant les alias doxygen des fonctions.
//TODO : Gérer l'utf-8 ds les chaînes documentaires.
%feature("docstring","Essai d Ajout a La Documentation Automatique") MEDmeshCr;

%include "medmesh_exception.i"

%include "medmesh.h"


// Liste des arguments apparaissant dans med_mesh.h :

// STRINGS :
//OUT (taille particulière) :
//char * const axisname
//char * const axisunit
//char * const description
//char * const dtunit

// STRING ARRAY OUT !!!! :
//char * const name
//char * const elementname
//char * const nodename

//OUT (taille standard) :
//char * const meshname
//char * const equivname
//char * const profilename
//char * const geotypename

//OUT (taille spéciale) :
//char * const univname


//char * const usedpath?

//IN :
//const char * const axisname
//const char * const axisunit
//const char* const datagroupname 
//const char* const datasetname
//const char * const description
//const char * const dtunit
//const char * const elementname
//const char * const meshname
//const char * const nodename
//const char * const profilename
//const char * const root
//const char* const rootname
//const char* const value
//const char* const varattname


//BOOL IN:
// Pas d'ambiguité, transmission par recopie
//const med_bool justopen
//const med_bool withelementname
//const med_bool withelementnumber
//const med_bool withfamnumber
//const med_bool withnodename
//const med_bool withnodenumber
//BOOL OUT:
//med_bool * const changement
//med_bool * const isasupportmesh
//med_bool * const transformation
//med_bool * const withelementname
//med_bool * const withelementnumber
//med_bool * const withfamnumber
//med_bool * const withnodename
//med_bool * const withnodenumber

// Pas d'ambiguité possible pour const med_float * const : ARRAY
//const med_float * const coordinate
//const med_float * const coordinates
//const med_float * const coordinatetrsf
//const med_float * const gridindex
//const med_float * const value

// Pas d'ambiguité, transmission par recopie
//const med_float dt
//const med_float dt2

// ARRAY : (OUT)
//med_float * const coordinate
//med_float * const coordinates
//med_float * const gridindex
//med_float * const value
//SCALAIRE : (OUT)
//med_float * dt

//Défini dans medfile
//const med_idt fid
//const med_idt id

// Pas d'ambiguité possible pour const med_int * const : ARRAY IN
//const med_int * const connectivity
//const med_int * const connectivity 
//const med_int * const connectivity
//const med_int * const elementnumber
//const med_int * const faceindex
//const med_int * const famnumber
//const med_int * const gridstruct
//const med_int * const nodeindex
//const med_int * const nodenumber
//const med_int * const number
//const med_int * const polyindex
//const med_int * const value

// Pas d'ambiguité, transmission par recopie SCALAIRE
//const med_int axis
//const med_int cellmaxnodes
//const med_int dimselect
//const med_int faceindexsize
//const med_int indexsize
//const med_int isolatednodes
//const med_int meshdim
//const med_int nentity
//const med_int nodeindexsize
//const med_int numdt
//const med_int numdt1
//const med_int numdt2
//const med_int numit
//const med_int numit1
//const med_int numit2
//const med_int spacedim
//const med_int umit
//const med_int verticesnodes
// ITERATEUR DE TYPE INT : RIEN A FAIRE (par valeur)
//const int csit
//const int geotypeit
//const int meshit

// ARRAY OUT :
//med_int * const connectivity
//med_int * const elementnumber
//med_int * const faceindex
//med_int * const famnumber
//med_int * const gridstruct
//med_int * const number
//med_int * const polyindex
//med_int * const nodeindex
//med_int * const nodenumber

// SCALAIRE EN OUT :
//med_int * const meshdim
//med_int * const geodim
//med_int * const nnodes
//med_int * const nstep
//med_int * const profilesize
//med_int * const spacedim
//med_int * const cellmaxnodes
//med_int * const numdt
//med_int * const numit
//med_int * const isolatednodes
//med_int * const verticesnodes

//LES ENUMS IN/OUT sont gérés par la même macros

//ENUMS IN :
//const med_internal_type datatype
//const med_mesh_type meshtype
//const med_sorting_type sortingtype
//const med_storage_mode storagemode
//const med_switch_mode switchmode
//const med_axis_type axistype
//const med_connectivity_mode cmode
//const med_data_type datatype
//const med_data_type meddatatype
//const med_entity_type entitype
//const med_entity_type entitytype
//const med_grid_type gridtype

//ENUMS OUT :
//med_mesh_type * const meshtype
//med_sorting_type * const sortingtype
//med_axis_type * const axistype
//med_grid_type * const gridtype

//DEFINE OUT :
//med_geometry_type * const geotype
//DEFINE IN :
//const med_geometry_type geotype
//const med_geometry_type mgeotype

//STRUCT:
//const med_filter * const filter

//TODO :
//IN:
//const void * const attval
//const void * const value
//OUT:
//unsigned char* const value
//void * const attval
//void * const value






