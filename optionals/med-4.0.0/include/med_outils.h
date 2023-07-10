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

#ifndef MED_OUTILS_H
#define MED_OUTILS_H
#include <med.h>
#include "med_vargs.h"

#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define MED_MAJOR_NAME "MAJ"
#define MED_MINOR_NAME "MIN"
#define MED_RELEASE_NAME "REL"

#define MED_NOM_MAJEUR "MAJ"
#define MED_NOM_MINEUR "MIN"
#define MED_NOM_RELEASE "REL"

#define MED_NOM_DESCRIPTEUR "descripteur de fichier"
#define MED_COMMENT_NAME MED_NOM_DESCRIPTEUR

/* Noms des data sets ou attributs correspondant a des entites MED */
#define MED_TAILLE_NOM_ENTITE MED_NAME_SIZE
#define MED_NOM_NUM "NUM"
#define MED_NOM_NBR "NBR"
#define MED_NOM_NOM "NOM"
#define MED_NOM_UNV "UNV"
#define MED_NOM_NNS "NNS"
#define MED_NOM_NNM "NNM"
#define MED_NOM_NNI "NNI"
#define MED_NOM_GRO "GRO"

#define MED_NOM_ATR "ATR"
#define MED_NOM_ATT "ATT"

#define MED_NOM_NCO "NCO"
#define MED_NOM_DIM "DIM"
#define MED_NOM_ESP "ESP"
#define MED_NOM_FAM "FAM"
#define MED_NOM_IDE "IDE"
#define MED_NOM_VAL "VAL"
#define MED_NOM_DES "DES"
#define MED_NOM_COR "COR"
#define MED_NOM_DIM "DIM"
#define MED_NOM_COO "COO"
#define MED_NOM_TRF "TRF"
#define MED_NOM_IN1 "IN1"
#define MED_NOM_IN2 "IN2"
#define MED_NOM_IN3 "IN3"
#define MED_NOM_REP "REP"
#define MED_NOM_UNI "UNI"
#define MED_NOM_UNT "UNT"
#define MED_NOM_NOD "NOD"
#define MED_NOM_TYP "TYP"
#define MED_NOM_GTY "GTY"
#define MED_NOM_CO  "CO"
#define MED_NOM_NCW "NCW"
#define MED_NOM_TYW "TYW"
#define MED_NOM_NXT "NXT"
#define MED_NOM_NXI "NXI"
#define MED_NOM_PVT "PVT"
#define MED_NOM_PVI "PVI"
#define MED_NOM_CGT "CGT"
#define MED_NOM_CGS "CGS"
#define MED_NOM_SRT "SRT"

#define MED_NOM_MAI "MAI"
#define MED_NOM_FAC "FAC"
#define MED_NOM_ARE "ARE"
#define MED_NOM_NOE "NOE"
#define MED_NOM_STR "STR"

#define MED_NOM_PO1 "PO1"
#define MED_NOM_SE2 "SE2"
#define MED_NOM_SE3 "SE3"
#define MED_NOM_SE4 "SE4"
#define MED_NOM_TR3 "TR3"
#define MED_NOM_TR6 "TR6"
#define MED_NOM_TR7 "TR7"
#define MED_NOM_QU4 "QU4"
#define MED_NOM_QU8 "QU8"
#define MED_NOM_QU9 "QU9"
#define MED_NOM_TE4 "TE4"
#define MED_NOM_T10 "T10"
#define MED_NOM_O12 "O12"
#define MED_NOM_HE8 "HE8"
#define MED_NOM_H20 "H20"
#define MED_NOM_H27 "H27"
#define MED_NOM_PE6 "PE6"
#define MED_NOM_P15 "P15"
#define MED_NOM_P18 "P18"
#define MED_NOM_PY5 "PY5"
#define MED_NOM_P13 "P13"

#define MED_NOM_TAI "TAI"
#define MED_NOM_TTI "TTI"
#define MED_NOM_IFN "IFN"
#define MED_NOM_IFD "IFD"
#define MED_NOM_INN "INN"
#define MED_NOM_IND "IND"
#define MED_NOM_POG "POG"
#define MED_NOM_PO2 "PO2"
#define MED_NOM_POE "POE"

#define MED_NOM_GEO "GEO"
#define MED_NOM_NEO "NEO"
#define MED_NOM_ENT "ENT"
#define MED_NOM_GED "GED"
#define MED_NOM_END "END"
#define MED_NOM_GAU "GAU"
#define MED_NOM_NGA "NGA"
#define MED_NOM_PFL "PFL"
#define MED_NOM_PFU "PFU"
#define MED_NOM_NDT "NDT"
#define MED_NOM_RDT "RDT"
#define MED_NOM_PDT "PDT"
#define MED_NOM_NOR "NOR"
#define MED_NOM_ROR "ROR"
#define MED_NOM_LIE "LIE"

#define MED_NOM_DOM "DOM"
#define MED_NOM_GLB "GLB"

/* #define MED_NOM_NBN "NBN" */
/* #define MED_NOM_NBM "NBM" */

#define MED_NOM_PRM "PRM"

/* #define MED_NOM_NBB "NBB" */
#define MED_NOM_INM "INM"
#define MED_NOM_NBV "NBV"
#define MED_NOM_MDG "MDG"
#define MED_NOM_MCF "NCF"
#define MED_NOM_FCN "FCN"
#define MED_NOM_COE "COE"
#define MED_NOM_POW "POW"

/* #define MED_NOM_NAV "NAV" */
/* #define MED_NOM_NAC "NAC" */

/* Nom du DATA GROUP CONTENANT TOUS LES MAILLAGES DU FICHIER HDF */
#define MED_MESH_NAME "ENS_MAA"
#define MED_MESH_GRP "/" MED_MESH_NAME "/"
#define MED_MESH_GRP_SIZE  9

/* Nom du DATA GROUP CONTENANT TOUS LES MAILLAGES SUPPORTS DU FICHIER HDF */
#define MED_MESH_SUPPORT_GRP "/ENS_SUP_MAA/"
#define MED_MESH_SUPPORT_GRP_SIZE 13

/* Nom du DATA GROUP CONTENANT TOUS LES CHAMPS RESULTATS */
#define MED_FIELD_NAME "CHA"
#define MED_FIELD_GRP "/" MED_FIELD_NAME "/"
#define MED_FIELD_GRP_SIZE 5

/* Nom du DATA GROUP CONTENANT TOUS LES MODELES D'ELEMENTS DE STRUCTURE */
#define MED_ELSTRUCT_NAME "STRUCT"
#define MED_ELSTRUCT_GRP "/" MED_ELSTRUCT_NAME "/"
#define MED_ELSTRUCT_GRP_SIZE 8

/* Nom du DATA GROUP CONTENANT TOUTES LES FONCTIONS D'INTERPOLATIONS */
#define MED_INTERPOLATION_NAME "INTERP"
#define MED_INTERPOLATION_GRP "/" MED_INTERPOLATION_NAME "/"
#define MED_INTERPOLATION_GRP_SIZE 8

/* Nom du DATA GROUP CONTENANT LES FONCTIONS D'INTERPOLATIONS UTILISEES PAR LES CHAMPS */
#define MED_CHA_INTERP "/CHA_INTERP/"
#define MED_TAILLE_CHA_INTERP 12

/* Nom du DATA GROUP CONTENANT TOUS LES ATTRIBUTS CONSTANTS D'ELEMENTS DE STRUCTURE */
#define MED_CSTATR "/CSTATR/"
#define MED_CSTATR_NOM "CSTATR"
#define MED_TAILLE_CSTATR 8

/* Nom du DATA GROUP CONTENANT TOUS LES ATTRIBUTS VARAIBLES D'ELEMENTS DE STRUCTURE */
#define MED_VARATR "/VARATR/"
#define MED_VARATR_NOM "VARATR"
#define MED_TAILLE_VARATR 8

/* Nom du DATA GROUP CONTENANT LES TRANSFORMATIONS GEOMETRIQUES APPLIQUEES AUX COORDONNEES */
#define MED_COOTRF "/COOTRF/"
#define MED_COOTRF_NOM "COOTRF"
#define MED_TAILLE_COOTRF 8

/* Nom du data group point de montage */
#define MED_MNT "/MNT/"
#define MED_TAILLE_MNT 5

/* Nom du data group ou ranger les familles */
#define MED_FAMILY_NAME "FAS"
#define MED_FAMILY_GRP "/" MED_FAMILY_NAME "/"
#define MED_FAMILY_GRP_SIZE 5

#define FAMILLE_ZERO "FAMILLE_ZERO"
#define MED_FAS_ZERO "/FAMILLE_ZERO/"
#define MED_FAS_ZERO_NOM "FAMILLE_ZERO"
#define MED_TAILLE_FAS_ZERO 14
#define MED_FAS_NOEUD "/NOEUD/"
#define MED_FAS_NOEUD_NOM "NOEUD"
#define MED_FAS_ELEME "/ELEME/"
#define MED_FAS_ELEME_NOM "ELEME"
#define MED_TAILLE_FAS_ENTITE 7

/* Nom du data group ou ranger les equivalences */
#define MED_EQUIVALENCE_NAME "EQS"
#define MED_EQUIVALENCE_GRP "/" MED_EQUIVALENCE_NAME "/"
#define MED_EQUIVALENCE_GRP_SIZE 5

/* Nom du data group contenant les profils */
#define MED_PROFILE_NAME "PROFILS"
#define MED_PROFILE_GRP "/" MED_PROFILE_NAME "/"
#define MED_PROFILE_GRP_SIZE 9

/* Nom du data group contenant la localisation des points de GAUSS */
#define MED_LOCALIZATION_NAME "GAUSS"
#define MED_LOCALIZATION_GRP "/" MED_LOCALIZATION_NAME "/"
#define MED_LOCALIZATION_GRP_SIZE 7

/*  Nom du data group contenant  les valeurs numeriques scalaires */
#define MED_NUMERICAL_DATA_NAME  "NUM_DATA"
#define MED_NUMERICAL_DATA_GRP "/" MED_NUMERICAL_DATA_NAME "/"
#define MED_NUMERICAL_DATA_GRP_SIZE 10

/* Nom du data group contenant les liens */
#define MED_LIENS "/LIENS/"
#define MED_TAILLE_LIENS 7

/* Nom du data group contenant les joints */
#define MED_JOINT_NAME "JNT"
#define MED_JOINT_GRP "/" MED_JOINT_NAME "/"
#define MED_JOINT_GRP_SIZE 5

/* Nom du data group contenant la version du fichier */
#define MED_INFOS "/INFOS_GENERALES/"

/*Pour eviter le bug solaris*/
#ifndef __APPLE__
#include <malloc.h>
#endif

/* Interface des routines du composant tools */
#include <med_misc.h>
#include <med_hdfi.h>
#include <med_utils.h>

#endif /* MED_OUTILS_H */
