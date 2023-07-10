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

void MAJ_21_22_familles_maillage(med_idt mid)
{
  med_idt fid, gid;
  char *nouvelle_chaine;
  int n;
  med_size dimd[1];
  med_err ret;
  int i;
  char chemin[MED_TAILLE_FAS+2*MED_TAILLE_NOM+1];
  char nom[MED_TAILLE_NOM+1];
  char *noeuds, *elements;
  int nnoeuds = 0;
  int nelements = 0;
  med_int numero;
  char *groupe_noeuds = "NOEUD";
  char *groupe_elements = "ELEME";
  char *nom_famille_zero = "FAMILLE_ZERO";
  char famille0[MED_TAILLE_NOM+1]="";
  med_int zero=0;
  int *index_noeuds, *index_elements;
  char stockage[MED_TAILLE_DESC]="";
  med_float *valr;

  /* Combien de famille ? */
  fid = _MEDdatagroupOuvrir(mid,"FAS");
  if (fid < 0) {
    MESSAGE("Le datagroup contenant les familles n'existe pas, medimport le crée");
    fid = _MEDdatagroupCreer(mid,"FAS");
    EXIT_IF(fid < 0,"Creation du groupe HDF des familles",NULL);
  }
  n = 0;
  _MEDnObjets(fid,".",&n);
  EXIT_IF(n < 0,"Lecture du nombre de famille",NULL);

  noeuds = (char *) malloc(n*MED_TAILLE_NOM+1);
  EXIT_IF(noeuds == NULL,NULL,NULL);
  index_noeuds = (int *) malloc(sizeof(int)*(n+1));
  EXIT_IF(index_noeuds == NULL,NULL,NULL);
  elements = (char *) malloc(n*MED_TAILLE_NOM+1);
  EXIT_IF(elements == NULL,NULL,NULL);
  index_elements = (int *) malloc(sizeof(int)*(n+1));
  EXIT_IF(index_elements == NULL,NULL,NULL);

  /* On recupere les familles une par une et on les
     normalise */
  for (i=0;i<n;i++) {
    ret = _MEDobjetIdentifier(fid,".",i,nom);
    EXIT_IF(ret < 0,"Identification d'une famille",NULL);

    /* On accede a la famille */
    gid = _MEDdatagroupOuvrir(fid,nom);
    EXIT_IF(gid < 0,"Ouverture de l'accès à la famille",nom);

    /* On lit le numero de la famille */
    ret = _MEDattrEntierLire(gid,(char *)(MED_NOM_NUM),&numero);
    EXIT_IF(ret < 0,"Lecture du numéro de la famille",nom);

    /* On ferme l'acces a la famille */
    ret = _MEDdatagroupFermer(gid);
    EXIT_IF(ret < 0,"Fermeture de l'accès à la famille",nom);

    if (numero == 0)
      strcpy(famille0,nom);

    if (numero < 0) {
      if (nelements == 0) {
	*(index_elements) = 0;
	strcpy(elements,nom);
      }
      else
	strcat(elements,nom);
      nelements ++;
      *(index_elements+nelements) = strlen(nom) + *(index_elements+nelements-1);
    }

    if (numero > 0) {
      if (nnoeuds == 0) {
	strcpy(noeuds,nom);
	*(index_noeuds) = 0;
      }
      else
	strcat(noeuds,nom);
      nnoeuds++;
      *(index_noeuds+nnoeuds) = strlen(nom) + *(index_noeuds+nnoeuds-1);
    }
  }

  /* Normalisation de la famille 0 */
  if ( strlen(famille0) ) {
    ret = H5Gmove(fid,famille0,nom_famille_zero);
    EXIT_IF(ret < 0,"Normalisation de la famille ZERO",NULL);
  } else {
    MESSAGE("La famille zéro n'existe pas, medimport la crée.");
    gid = _MEDdatagroupCreer(fid,nom_famille_zero);
    EXIT_IF(gid < 0,"Creation de la famille zéro",NULL);
    ret = _MEDattrEntierEcrire(gid,(char *)(MED_NOM_NUM),&zero);
    EXIT_IF(ret < 0,"Ecriture du numéro de la famille zéro",nom_famille_zero);
    ret = _MEDdatagroupFermer(gid);
    EXIT_IF(ret < 0,"Fermeture de l'accès à la famille zéro",nom);
  }

  /* On cree les groupes HDF pour les familles de noeuds et d'elements */
  gid = _MEDdatagroupCreer(fid,groupe_noeuds); 
  EXIT_IF(gid < 0,"Creation du grupe HDF pour les familles de noeuds",NULL); 
  ret = _MEDdatagroupFermer(gid); 
  EXIT_IF(ret < 0,"Fermeture du groupe HDF pour les familles de noeuds",NULL); 
  gid = _MEDdatagroupCreer(fid,groupe_elements); 
  EXIT_IF(gid < 0,"Creation du groupe HDF pour les familles d'éléments",NULL); 
  ret = _MEDdatagroupFermer(gid); 
  EXIT_IF(ret < 0,"Fermeture du groupe HDF pour les familles d'éléments",NULL); 

  /* On deplace les groupes HDF des familles en fonction de la nature de 
     chaque famille */
  for (i=0;i<nnoeuds;i++) {
    strncpy(nom,noeuds+*(index_noeuds+i),*(index_noeuds+i+1)-*(index_noeuds+i));
    nom[*(index_noeuds+i+1)-*(index_noeuds+i)] = '\0';
    strcpy(stockage,groupe_noeuds);
    strcat(stockage,"/");
    strcat(stockage,nom);
    ret = H5Gmove(fid,nom,stockage); 
    EXIT_IF(ret < 0,"Normalisation de la famille",nom); 
  }

  for (i=0;i<nelements;i++) {
    strncpy(nom,elements+*(index_elements+i),*(index_elements+i+1)-*(index_elements+i));
    nom[*(index_elements+i+1)-*(index_elements+i)] = '\0';
    strcpy(stockage,groupe_elements);
    strcat(stockage,"/"); 
    strcat(stockage,nom);
    ret = H5Gmove(fid,nom,stockage);  
    EXIT_IF(ret < 0,"Normalisation de la famille",nom);  
  }

  /* On libere la memoire */
  free(noeuds);
  free(index_noeuds);
  free(elements);
  free(index_elements);

  /* On ferme tout */
  ret = _MEDdatagroupFermer(fid);
  EXIT_IF(ret < 0,"Fermeture du groupe HDF 'FAS'",NULL);
}
