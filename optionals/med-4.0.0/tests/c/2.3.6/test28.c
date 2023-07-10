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

/******************************************************************************
 * - Nom du fichier : test28.c
 *
 * - Description : lecture d'un maillage structure (grille cartesienne |
 *                 grille polaire) dans le fichier test27.med
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include "med_utils.h"
#include <string.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

int main (int argc, char **argv)


{
  int i;
  med_err ret = 0;
  med_idt fid;
  /* la dimension du maillage */
  med_int mdim,axe,j;
  med_float *indices;
  med_int nind,nmaa;
  /* nom du maillage de longueur maxi MED_TAILLE_NOM */
  char maa[MED_TAILLE_NOM+1];
  /* composantes et unites */
  char comp[MED_TAILLE_PNOM+1];
  char unit[MED_TAILLE_PNOM+1];
  char desc[MED_TAILLE_DESC+1];
  med_maillage type_maillage;
  med_type_grille type;
  med_table quoi;
  med_int nnoeuds;
  char *comp2, *unit2;
  med_float *coo;
  med_int *structure_grille;
  med_repere repere;

  /* Ouverture du fichier test17.med en lecture seule */
  fid = MEDouvrir("test27.med",MED_LECTURE);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test27.med");
    return -1;
  }   

  /* Lecture du nombre de maillage */
  nmaa = MEDnMaa(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a la lecture du nombre de maillage");
    return -1;
  }   

  /* On boucle sur les maillages et on ne lit que la grille cartesienne 
     et la grille desturcutree */
  for (i=0;i<nmaa;i++) {
    
    /* On repere le maillage qui nous interesse */
    if (MEDmaaInfo(fid,i+1,maa,&mdim,&type_maillage,desc) < 0) {
      MESSAGE("Erreur a la lecture des infos sur le maillage");
      return -1;
    }   
    printf("Maillage de nom [%s] de dimension "IFORMAT" \n",maa,mdim);
    if (type_maillage == MED_STRUCTURE)
      printf("- Type : Maillage structure \n");
    else
      printf("- Type : Maillage non structure \n");
    
    /* On regarde le type de la grille */
    if (type_maillage == MED_STRUCTURE) {
      if (MEDnatureGrilleLire(fid,maa,&type) < 0) {
	MESSAGE("Erreur a la lecture de la nature d'une grille");
	return -1;
      }   
      if (type == MED_GRILLE_CARTESIENNE)
	printf("- Grille cartesienne \n");
      if (type == MED_GRILLE_STANDARD)
	printf("- Grille de-structureee \n");
    }
    
    /* On regarde les coordonnees de la grille standard */
    if (type_maillage == MED_STRUCTURE && type == MED_GRILLE_STANDARD) {
      
      nnoeuds = MEDnEntMaa(fid,maa,MED_COOR,MED_NOEUD,0,0);
      if (nnoeuds < 0) {
	MESSAGE("Erreur a la lecture du nombre de noeuds");
	return -1;
      }   
      printf("Nombre de noeuds : "IFORMAT" \n",nnoeuds);
      
      structure_grille = (med_int *) malloc(sizeof(med_int)*mdim);
      if (MEDstructureCoordLire(fid,maa,mdim,structure_grille) < 0) {
	MESSAGE("Erreur a la lecture de la structure de la grille");
	ret = -1;
      }   
      if (ret == 0) {
	printf("Structure des noeuds de la grille : [ ");
	for (j=0;j<mdim;j++)
	  printf(" "IFORMAT" ",*(structure_grille+j));
	printf(" ] \n");
	free(structure_grille);
      }
      
      if (ret == 0) {
	coo = (med_float *) malloc(sizeof(med_float)*nnoeuds*mdim);
	comp2 = (char *) malloc(sizeof(char)*(mdim*MED_TAILLE_PNOM+1));
	unit2 = (char *) malloc(sizeof(char)*(mdim*MED_TAILLE_PNOM+1));

	if ((ret = MEDcoordLire(fid,maa,mdim,coo,MED_FULL_INTERLACE,MED_ALL,NULL,0,
				&repere,comp2,unit2)) < 0) {
	  MESSAGE("Erreur a la lecture des coordonnees des noeuds");
	  ret = -1;
	}   
      }
	
      if (ret == 0) {
	printf("Coordonnees : [ ");
	for (j=0;j<nnoeuds*mdim;j++)
	  printf(" %f ",*(coo+j));
	printf(" ] \n");
	free(coo);
	free(comp2);
	free(unit2);
      }
    } 

    /* On regarde les coordonnees des indices de la grille cartesienne */
    if (type_maillage == MED_STRUCTURE && type == MED_GRILLE_CARTESIENNE) 
      for(axe=1;axe<=mdim;axe++) {
	switch(axe) {
	case 1 :
	  quoi = MED_COOR_IND1;
	  break;
	  
	case 2 :
	  quoi = MED_COOR_IND2;
	  break;
	  
	case 3 :
	  quoi = MED_COOR_IND3;
	  break;
	  
	default :
	  return -1;
	}
	
	nind = MEDnEntMaa(fid,maa,quoi,MED_NOEUD,0,0);
	if (nind < 0) {
	  MESSAGE("Erreur a la lecture de la taille de l'indice");
	  return -1;
	}   
	printf("Lecture de la taille de l'indice : "IFORMAT" \n",nind);
	
	  /* on lit le tableau des indices */
	indices = (med_float *) malloc(sizeof(med_float)*nind);
	if (MEDindicesCoordLire(fid,maa,mdim,indices,nind,axe,comp,unit) < 0) {
	  MESSAGE("Erreur a lecture de indices de coordonnees");
	  ret = -1;
	}   
	if (ret == 0) {
	  printf("Axe %s [%s] : [ ",comp,unit);
	  for (j=0;j<nind;j++)
	    printf(" %f ",indices[j]);
	  printf(" ] \n");
	  free(indices);
	}
      }
  }

  /* On ferme le fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");
  
  return ret;
}
