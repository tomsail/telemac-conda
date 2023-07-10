/*************************************************************************
* COPYRIGHT (C) 1999 - 2019  EDF R&D, CEA/DEN
* THIS LIBRARY IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
* IT UNDER THE TERMS OF THE GNU LESSER GENERAL PUBLIC LICENSE
* AS PUBLISHED BY THE FREE SOFTWARE FOUNDATION;
* EITHER VERSION 2.1 OF THE LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
*
* THIS LIBRARY IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
* WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
* LESSER GENERAL PUBLIC LICENSE FOR MORE DETAILS.
*
* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU LESSER GENERAL PUBLIC LICENSE
* ALONG WITH THIS LIBRARY; IF NOT, WRITE TO THE FREE SOFTWARE FOUNDATION,
* INC., 59 TEMPLE PLACE, SUITE 330, BOSTON, MA 02111-1307 USA
*
*************************************************************************/

#ifndef MED_OUTILS_H
#define MED_OUTILS_H
#include <med.h>

#define MED_NOM_MAJEUR "MAJ"
#define MED_NOM_MINEUR "MIN"
#define MED_NOM_RELEASE "REL"

#define MED_NOM_DESCRIPTEUR "descripteur de fichier"
#define HDF_VERSION_ACTUELLE "5-1.6.4"

/* Noms des data sets ou attributs correspondant a des entites MED */
#define MED_TAILLE_NOM_ENTITE 3
#define MED_NOM_NUM "NUM"
#define MED_NOM_NBR "NBR"
#define MED_NOM_NOM "NOM"
#define MED_NOM_UNV "UNV"
#define MED_NOM_NNS "NNS"
#define MED_NOM_NNM "NNM"
#define MED_NOM_NNI "NNI"
#define MED_NOM_GRO "GRO"
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
#define MED_NOM_NOE "NOE"
#define MED_NOM_COO "COO"
#define MED_NOM_IN1 "IN1"
#define MED_NOM_IN2 "IN2"
#define MED_NOM_IN3 "IN3"
#define MED_NOM_REP "REP"
#define MED_NOM_UNI "UNI"
#define MED_NOM_NOD "NOD"
#define MED_NOM_TYP "TYP"
#define MED_NOM_GTY "GTY"
#define MED_NOM_CO  "CO"
#define MED_NOM_NCW "NCW"
#define MED_NOM_TYW "TYW"

#define MED_NOM_MAI "MAI"
#define MED_NOM_FAC "FAC"
#define MED_NOM_ARE "ARE"

#define MED_NOM_PO1 "PO1"
#define MED_NOM_SE2 "SE2"
#define MED_NOM_SE3 "SE3"
#define MED_NOM_TR3 "TR3"
#define MED_NOM_TR6 "TR6"
#define MED_NOM_QU4 "QU4"
#define MED_NOM_QU8 "QU8"
#define MED_NOM_TE4 "TE4"
#define MED_NOM_T10 "T10"
#define MED_NOM_HE8 "HE8"
#define MED_NOM_H20 "H20"
#define MED_NOM_PE6 "PE6"
#define MED_NOM_P15 "P15"
#define MED_NOM_PY5 "PY5"
#define MED_NOM_P13 "P13"

#define MED_NOM_TAI "TAI"
#define MED_NOM_TTI "TTI"
#define MED_NOM_IFN "IFN"
#define MED_NOM_IFD "IFD"
#define MED_NOM_INN "INN"
#define MED_NOM_IND "IND"
#define MED_NOM_POG "POG"
#define MED_NOM_POE "POE"

#define MED_NOM_GEO "GEO"
#define MED_NOM_GAU "GAU"
#define MED_NOM_NGA "NGA"
#define MED_NOM_PFL "PFL"
#define MED_NOM_NDT "NDT"
#define MED_NOM_PDT "PDT"
#define MED_NOM_NOR "NOR"
#define MED_NOM_LIE "LIE"

#define MED_NOM_DOM "DOM"
#define MED_NOM_GLB "GLB"

/* Nom du DATA GROUP CONTENANT TOUS LES MAILLAGES DU FICHIER HDF */
#define MED_MAA "/ENS_MAA/"
#define MED_TAILLE_MAA 9

/* Nom du data group ou ranger les champs solution */
#define MED_CHA "/CHA/"
#define MED_TAILLE_CHA 5

/* Nom du data group point de montage */
#define MED_MNT "/MNT/"
#define MED_TAILLE_MNT 5

/* Nom du data group ou ranger les familles */
#define FAMILLE_ZERO "FAMILLE_ZERO"
#define MED_FAS "/FAS/"
#define MED_TAILLE_FAS 5
#define MED_FAS_NOEUD "/NOEUD/"
#define MED_FAS_NOEUD_NOM "NOEUD"
#define MED_FAS_ELEME "/ELEME/"
#define MED_FAS_ELEME_NOM "ELEME"
#define MED_TAILLE_FAS_ENTITE 7


/* Nom du data group ou ranger les equivalences */
#define MED_EQS "/EQS/"
#define MED_TAILLE_EQS 5

/* Nom du data group contenant les profils */
#define MED_PROFILS "/PROFILS/"
#define MED_TAILLE_PROFILS 9 

/* Nom du data group contenant la localisation des points de GAUSS */
#define MED_GAUSS "/GAUSS/"
#define MED_TAILLE_GAUSS 7 

/* Nom du groupe HDF ou sont rangees les valeurs numeriques scalaires 
   non liees au modele de donnees
*/
#define MED_NUM_DATA  "/NUM_DATA/"
#define MED_TAILLE_NUM_DATA 10

/* Nom du data group contenant les liens */
#define MED_LIENS "/LIENS/"
#define MED_TAILLE_LIENS 7 

/* Nom du data group contenant les joints */
#define MED_JNT "/JNT/"
#define MED_TAILLE_JNT 5

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
