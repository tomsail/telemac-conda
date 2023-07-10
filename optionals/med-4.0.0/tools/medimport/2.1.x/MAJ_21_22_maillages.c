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



#include "med_config.h"
#include "med_outils.h"
#include <string.h>

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "med21.h"
#include "med_hdfi21.h"
#include "MAJ_21_22.h"

void MAJ_21_22_maillages(med_idt fid)
{
  med_idt gid;
  med_err ret;
  int n,i;
  char nom[MED_TAILLE_NOM+1];
  char chemin[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  char description[MED_TAILLE_DESC+1] = "Maillage converti au format MED V2.2";
  med_int type = (med_int) MED_NON_STRUCTURE;
  med_int dimension;
  
  /* Lecture du nombre de maillages */
  n = 0;
  _MEDnObjets(fid,(char *) MED_MAA,&n);
  EXIT_IF(n < 0,"Erreur a la lecture du nombre de maillage",NULL);

  /* 
   * Mise a jour des maillages :
   *  - type : MED_NON_STRUCTURE
   *  - description : "Maillage converti au format V2.2"
   */
  for (i=0;i<n;i++) {
    /* on recupere le nom du maillage */
    ret = _MEDobjetIdentifier(fid,(char *) MED_MAA,i,nom);
    EXIT_IF(ret < 0,"Identification d'un maillage",NULL);
    fprintf(stdout,"  >>> Normalisation du maillage [%s] \n",nom);

    /* on accede au maillage */
    strcpy(chemin,MED_MAA);
    strcat(chemin,nom);
    gid = _MEDdatagroupOuvrir(fid,chemin); 
    EXIT_IF(gid < 0,"Accès au maillage",nom);

    /* lecture de la dimension du maillage */
    ret = _MEDattrEntierLire(gid,(char *)(MED_NOM_DIM),&dimension);
    EXIT_IF(ret < 0,"Lecture de la dimension du maillage",nom);

    /* Ecriture du type et de la description */
    ret = _MEDattrStringEcrire(gid,(char *)(MED_NOM_DES),MED_TAILLE_DESC,description);
    EXIT_IF(ret < 0,"Ecriture de la description du maillage ",nom);
    ret = _MEDattrEntierEcrire(gid,(char *)(MED_NOM_TYP),&type);
    EXIT_IF(ret < 0,"Ecriture de la dimension du maillage ",nom);
    
    /* Mise a jour des noeuds du maillage */ 
    MAJ_21_22_noeuds_maillage(gid,dimension);  
    fprintf(stdout,"  ... Normalisation des noeuds effectuée ... \n");
    
    /* Mise a jour des éléments du maillage */ 
    MAJ_21_22_elements_maillage(gid,dimension);  
    fprintf(stdout,"  ... Normalisation des éléments effectuée ... \n");
    
    /* Mise a jour des familles du maillage */
    MAJ_21_22_familles_maillage(gid);
    fprintf(stdout,"  ... Normalisation des familles effectuée ... \n");
    
    /* On ferme tout */
    ret = _MEDdatagroupFermer(gid);
    EXIT_IF(ret < 0,"Fermeture de l'accès au maillage",NULL);

    fprintf(stdout,"  >>> Normalisation du maillage [%s] ... OK ... \n",nom);
  }
}
