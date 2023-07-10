/*  This file is part of MED.
 *
 *  COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
 *  MED is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  MED is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with MED.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MED_ERR
#define MED_ERR


#define MED_ERR_UNDEFINED -1

#define MED_ERR_CREATE -100
#define MED_ERR_READ -200
#define MED_ERR_WRITE -300
#define MED_ERR_CLOSE -400
#define MED_ERR_ACCESS -500
#define MED_ERR_SELECT -600
#define MED_ERR_INIT -700
#define MED_ERR_RANGE -800
#define MED_ERR_NULL -900
#define MED_ERR_NOTNULL -1000
#define MED_ERR_OPEN -1100
#define MED_ERR_DOESNTEXIST -1200
#define MED_ERR_UNRECOGNIZED -1300
#define MED_ERR_INVALID -1400
#define MED_ERR_DELETE -1500
#define MED_ERR_NOTEQUAL -1600
#define MED_ERR_EXIST -1700
#define MED_ERR_MOUNT -1800
#define MED_ERR_UMOUNT -1900
#define MED_ERR_ULINK -2100
#define MED_ERR_COUNT -2200
#define MED_ERR_CALL -2300
#define MED_ERR_VISIT -2400
#define MED_ERR_COPY -2500
#define MED_ERR_USE -2600



#define MED_ERR_UNDEFINED_MSG "Erreur indéfinie "
#define MED_ERR_CREATE_MSG "Erreur à la création "
#define MED_ERR_READ_MSG "Erreur à la lecture "
#define MED_ERR_WRITE_MSG "Erreur à l'écriture "
#define MED_ERR_CLOSE_MSG "Erreur à la fermeture "
#define MED_ERR_ACCESS_MSG "Erreur d'accès "
#define MED_ERR_SELECT_MSG "Erreur de sélection "
#define MED_ERR_INIT_MSG "Erreur d'initialisation "
#define MED_ERR_RANGE_MSG "Erreur de domaine de définition "
#define MED_ERR_NULL_MSG "Erreur de valeur attendue nulle "
#define MED_ERR_NOTNULL_MSG "Erreur de valeur attendue non nulle "
#define MED_ERR_OPEN_MSG "Erreur à l'ouverture "
#define MED_ERR_DOESNTEXIST_MSG "Erreur de non existence "
#define MED_ERR_UNRECOGNIZED_MSG "Erreur de valeur non reconnue "
#define MED_ERR_INVALID_MSG "Erreur de valeur invalide "
#define MED_ERR_DELETE_MSG "Erreur de suppression "
#define MED_ERR_NOTEQUAL_MSG "Erreur d'égalité "
#define MED_ERR_EXIST_MSG "Erreur d'existence "
#define MED_ERR_MOUNT_MSG "Erreur au montage de l'objet "
#define MED_ERR_UMOUNT_MSG "Erreur au démontage de l'objet "
#define MED_ERR_ULINK_MSG "Erreur dans la destruction du lien "
#define MED_ERR_COUNT_MSG "Erreur de comptage "
#define MED_ERR_CALL_MSG "Erreur d'appel "
#define MED_ERR_VISIT_MSG "Erreur d'exploration "
#define MED_ERR_COPY_MSG "Erreur de copie "
#define MED_ERR_USE_MSG "Erreur d'utilisation  "



#define MED_ERR_FILE -1
#define MED_ERR_MESH -2
#define MED_ERR_SUPPORT_MESH -3
#define MED_ERR_ENTITY -4
#define MED_ERR_STRUCT_ELEMENT -5
#define MED_ERR_FAMILY -6
#define MED_ERR_GROUP -7
#define MED_ERR_LOCALIZATION -8
#define MED_ERR_INTERPOLATION -9
#define MED_ERR_FIELD -10
#define MED_ERR_JOINT -11
#define MED_ERR_FILTER -12
#define MED_ERR_PROFILE -13
#define MED_ERR_SCALAR -14
#define MED_ERR_MEMSPACE -15
#define MED_ERR_DISKSPACE -16
#define MED_ERR_DATASET -17
#define MED_ERR_DATAGROUP -18
#define MED_ERR_ATTRIBUTE -19
#define MED_ERR_PROPERTY -20
#define MED_ERR_INTERLACINGMODE -21
#define MED_ERR_PARAMETER -22
#define MED_ERR_ACCESSMODE -23
#define MED_ERR_GEOMETRIC -24
#define MED_ERR_DATASPACE -25
#define MED_ERR_FILESPACE -26
#define MED_ERR_LIBRARY -27
#define MED_ERR_API -28
#define MED_ERR_STORAGEMODE -29
#define MED_ERR_COMPUTINGSTEP -30
#define MED_ERR_HDFTYPE -31
#define MED_ERR_LINK -32
#define MED_ERR_DATATYPE -33
#define MED_ERR_MEDDATATYPE -34
#define MED_ERR_CONNECTIVITYMODE -35
#define MED_ERR_GRIDTYPE -36
#define MED_ERR_AXISTYPE -37
#define MED_ERR_EQUIVALENCE -38
#define MED_ERR_SUBDOMAINJOINT -39
#define MED_ERR_STRUCT -40
#define MED_ERR_INTERP -41
#define MED_ERR_MESHTYPE -42
#define MED_ERR_CORRESPONDENCE -43
#define MED_ERR_DIMENSION -44
#define MED_ERR_FILEVERSION -45
#define MED_ERR_MEMFILE -46
#define MED_ERR_CLASS -47

#define MED_ERR_FILE_MSG "du fichier "
#define MED_ERR_MESH_MSG "du maillage "
#define MED_ERR_SUPPORT_MESH_MSG "du maillage support "
#define MED_ERR_ENTITY_MSG "de l'entité "
#define MED_ERR_STRUCT_ELEMENT_MSG "de l'élément de structure "
#define MED_ERR_FAMILY_MSG "de la famille "
#define MED_ERR_GROUP_MSG "du groupe "
#define MED_ERR_LOCALIZATION_MSG "de la localisation des points d'intégration "
#define MED_ERR_INTERPOLATION_MSG "de la fonction d'interpolation "
#define MED_ERR_FIELD_MSG "du champ résultat "
#define MED_ERR_JOINT_MSG "du joint "
#define MED_ERR_FILTER_MSG "du filtre "
#define MED_ERR_PROFILE_MSG "du profil "
#define MED_ERR_SCALAR_MSG "des valeurs scalaires "
#define MED_ERR_LIBRARY_MSG "du numéro de version de la librairie "
#define MED_ERR_MEMSPACE_MSG "du memspace "
#define MED_ERR_DISKSPACE_MSG "du dataspace "
#define MED_ERR_DATASET_MSG "du dataset "
#define MED_ERR_DATAGROUP_MSG "du groupe "
#define MED_ERR_ATTRIBUTE_MSG "d'attribut "
#define MED_ERR_PROPERTY_MSG "de propriété "
#define MED_ERR_INTERLACINGMODE_MSG "d'entrelacement "
#define MED_ERR_PARAMETER_MSG "du paramètre "
#define MED_ERR_ACCESSMODE_MSG "du mode d'accès "
#define MED_ERR_GEOMETRIC_MSG "du type géométrique "
#define MED_ERR_DATASPACE_MSG "du dataspace "
#define MED_ERR_FILESPACE_MSG  "du filespace "
#define MED_ERR_API_MSG  "de l'API "
#define MED_ERR_STORAGEMODE_MSG "de mode de stockage "
#define MED_ERR_COMPUTINGSTEP_MSG "de l'étape de calcul "
#define MED_ERR_HDFTYPE_MSG "du type hdf "
#define MED_ERR_LINK_MSG "du lien "
#define MED_ERR_DATATYPE_MSG "du datatype "
#define MED_ERR_MEDDATATYPE_MSG "du datatatype med demandé "
#define MED_ERR_CONNECTIVITYMODE_MSG "du mode de connectivité "
#define MED_ERR_GRIDTYPE_MSG "du type de grille "
#define MED_ERR_AXISTYPE_MSG "du type de repère "
#define MED_ERR_EQUIVALENCE_MSG "de l'équivalence "
#define MED_ERR_SUBDOMAINJOINT_MSG "de joint  "
#define MED_ERR_STRUCT_MSG "de l'élément de structure "
#define MED_ERR_INTERP_MSG "de l'interpolation "
#define MED_ERR_PARAM_MSG "du parametre "
#define MED_ERR_MESHTYPE_MSG "du type de maillage"
#define MED_ERR_CORRESPONDENCE_MSG "de la correspondance "
#define MED_ERR_DIMENSION_MSG "de dimension"
#define MED_ERR_FILEVERSION_MSG "de version de fichier"
#define MED_ERR_MEMFILE_MSG "de fichier mémoire"
#define MED_ERR_CLASS_MSG "du type d'objet MED"

/*COMPLEMENTS*/
#define MED_ERR_VALUE_MSG "de valeur "
#define MED_ERR_SIZE_MSG "de taille "
#define MED_ERR_GSIZE_MSG "de taille trop grande "
#define MED_ERR_LSIZE_MSG "de taille trop petite "
#define MED_ERR_ID_MSG "d'identifiant "
#define MED_ERR_ARRAY_MSG "(tableau) "
#define MED_ERR_MODE_MSG "en mode "
#define MED_ERR_PARALLEL_MSG "en parallelisme "
#define MED_ERR_ACCESS2_MSG "du mode d'accès "
#define MED_ERR_FORBIDDEN_MSG "interdit(e) "
#define MED_ERR_TYPEOF_MSG "de type "
#define MED_ERR_NAME_MSG "de nom "
#define MED_ERR_INTERNAL_MSG "interne "
#define MED_ERR_NBR_MSG "nombre de "


/* Pour générer la table :
  for i in `grep 'MED_ERR_' med_err.h | grep -v 'MSG' | awk '{print $2}' `; do echo "{ $i , ${i}_MSG },"; done
*/
/* typedef struct _med_err_tab_entry { */
/*   int errno; */
/*   const char* const mesg; */
/* } med_err_tab_entry; */

/* med_err_tab_entry MED_TAB_ERR[]={ */
/*   { MED_ERR_UNDEFINED , MED_ERR_UNDEFINED_MSG }, */
/*   { MED_ERR_CREATE , MED_ERR_CREATE_MSG }, */
/*   { MED_ERR_READ , MED_ERR_READ_MSG }, */
/*   { MED_ERR_WRITE , MED_ERR_WRITE_MSG }, */
/*   { MED_ERR_CLOSE , MED_ERR_CLOSE_MSG }, */
/*   { MED_ERR_ACCESS , MED_ERR_ACCESS_MSG }, */
/*   { MED_ERR_SELECT , MED_ERR_SELECT_MSG }, */
/*   { MED_ERR_INIT , MED_ERR_INIT_MSG }, */
/*   { MED_ERR_FILE , MED_ERR_FILE_MSG }, */
/*   { MED_ERR_MESH , MED_ERR_MESH_MSG }, */
/*   { MED_ERR_SUPPORT_MESH , MED_ERR_SUPPORT_MESH_MSG }, */
/*   { MED_ERR_ENTITY , MED_ERR_ENTITY_MSG }, */
/*   { MED_ERR_STRUCT_ELEMENT , MED_ERR_STRUCT_ELEMENT_MSG }, */
/*   { MED_ERR_FAMILY , MED_ERR_FAMILY_MSG }, */
/*   { MED_ERR_GROUP , MED_ERR_GROUP_MSG }, */
/*   { MED_ERR_LOCALIZATION , MED_ERR_LOCALIZATION_MSG }, */
/*   { MED_ERR_INTERPOLATION , MED_ERR_INTERPOLATION_MSG }, */
/*   { MED_ERR_FIELD , MED_ERR_FIELD_MSG }, */
/*   { MED_ERR_JOINT , MED_ERR_JOINT_MSG }, */
/*   { MED_ERR_FILTER , MED_ERR_FILTER_MSG }, */
/*   { MED_ERR_PROFILE , MED_ERR_PROFILE_MSG }, */
/*   { MED_ERR_SCALAR , MED_ERR_SCALAR_MSG }, */
/*   { MED_ERR_MEMSPACE , MED_ERR_MEMSPACE_MSG }, */
/*   { MED_ERR_DISKSPACE , MED_ERR_DISKSPACE_MSG }, */
/*   { MED_ERR_DATASET , MED_ERR_DATASET_MSG }, */
/*   { MED_ERR_DATAGROUP , MED_ERR_DATAGROUP_MSG }, */
/*   { MED_ERR_ATTRIBUTE , MED_ERR_ATTRIBUTE_MSG } */
/* } */

#endif
