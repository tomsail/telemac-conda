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
 * - Nom du fichier : test17.c
 *
 * - Description : lecture d'elements de maillages MED ecrits par test16
 *                 via les routines de niveau 2
 *                 - equivalent a test7.c
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
  med_err ret = 0;
  med_idt fid;
  med_int nse2;
  med_int *se2;
  char *nomse2;
  med_int *numse2;
  med_int *nufase2; 
  med_int ntr3;
  med_int *tr3;
  char *nomtr3;
  med_int *numtr3;
  med_int *nufatr3;
  char maa[MED_TAILLE_NOM+1] ="maa1";
  med_int mdim;
  med_booleen inoele1,inoele2,inuele1,inuele2;
  med_int tse2,ttr3;
  med_int i;
  char str[MED_TAILLE_PNOM+1];
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;

  /* Ouverture du fichier test16.med en lecture seule */
  if ((fid = MEDouvrir("test16.med",MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test16.med");
    return -1;
  }

  /* Lecture des infos sur le 1er maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0) {
    MESSAGE("Erreur a la lecture des infos sur le 1er maillage");
    return -1;
  }
  printf("Maillage de nom : %s et de dimension "IFORMAT" \n",maa,mdim);

  /* Lecture du nombre de triangle et de segment */
  if ((nse2 = MEDnEntMaa(fid,maa,MED_CONN,MED_ARETE,MED_SEG2,MED_DESC)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de segments ");
    return -1;
  }
  if ((ntr3 = MEDnEntMaa(fid,maa,MED_CONN,MED_MAILLE,MED_TRIA3,MED_DESC))<0) {
    MESSAGE("Erreur a la lecture du nombre de triangles ");
    return -1;
  }
  printf("Nombre de MED_SEG2 : "IFORMAT" - nombre de MED_TRIA3 :"IFORMAT"\n",nse2,ntr3);

  /* Allocations memoire */ 
  tse2 = 2;
  se2  = (med_int*) malloc(sizeof(med_int)*tse2*nse2);
  nomse2 = (char*) malloc(MED_TAILLE_PNOM*nse2+1);
  numse2 = (med_int*) malloc(sizeof(med_int)*nse2);
  nufase2 = (med_int*) malloc(sizeof(med_int)*nse2);

  ttr3 = 3;
  tr3 = (med_int*) malloc(sizeof(med_int)*ntr3*ttr3);
  nomtr3 = (char*) malloc(MED_TAILLE_PNOM*ntr3+1);
  numtr3 = (med_int*) malloc(sizeof(med_int)*ntr3);
  nufatr3 = (med_int*) malloc(sizeof(med_int)*ntr3);

  /* Lecture des aretes segments MED_SEG2 : 
     - Connectivite,
     - Noms (optionnel)
     - Numeros (optionnel)
     - Numeros de familles */
  if (MEDelementsLire(fid,maa,mdim,se2,MED_NO_INTERLACE,nomse2,&inoele1,numse2,&inuele1,
		      nufase2,nse2,MED_ARETE,MED_SEG2,MED_DESC) < 0) {
    MESSAGE("Erreur a la lecture des segments");
    ret = -1;
  }

  /* Lecture des mailles triangles MED_TRIA3 : 
     - Connectivite,
     - Noms (optionnel)
     - Numeros (optionnel)
     - Numeros de familles */
  if (MEDelementsLire(fid,maa,mdim,tr3,MED_NO_INTERLACE,nomtr3,&inoele2,numtr3,&inuele2,
		      nufatr3,ntr3,MED_MAILLE,MED_TRIA3,MED_DESC) < 0) {
    MESSAGE("Erreur a la lecture des triangles");
    ret = -1;
  }

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    ret = -1;
  }

  /* Affichage */
  if (ret == 0) {
    if (nse2 > 0) {
      printf("Connectivite des segments : \n");
      for (i=0;i<nse2*tse2;i++)
	printf(IFORMAT" ",*(se2+i));
      if (inoele1) {
	printf("\nNoms des segments :\n");
	for (i=0;i<nse2;i++) {
	  strncpy(str,nomse2+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	  str[MED_TAILLE_PNOM] = '\0';
	  printf("%s ",str);
	}
      }
      if (inuele1) {
	printf("\nNumeros des segments :\n");
	for (i=0;i<nse2;i++)
	  printf(IFORMAT" ",*(numse2+i));
      }      
      printf("\nNumeros des familles des segments :\n");
      for (i=0;i<nse2;i++)
	printf(IFORMAT" ",*(nufase2+i));
    }

    if (ntr3 > 0) {
      printf("\nConnectivite des triangles : \n");
      for (i=0;i<ntr3*ttr3;i++)
	printf(IFORMAT" ",*(tr3+i));
      if (inoele2) {
	printf("\nNoms des triangles :\n");
	for (i=0;i<ntr3;i++) {
	  strncpy(str,nomtr3+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	  str[MED_TAILLE_PNOM] = '\0';
	  printf("%s ",str);
	}
      }
      if (inuele2) {
	printf("\nNumeros des triangles :\n");
	for (i=0;i<ntr3;i++)
	  printf(IFORMAT" ",*(numtr3+i));
      }      
      printf("\nNumeros des familles des triangles :\n");
      for (i=0;i<ntr3;i++)
	printf(IFORMAT" ",*(nufatr3+i));

      printf("\n");
    }
  }

  /* Nettoyage memoire */
  free(se2);
  free(nomse2);
  free(numse2);
  free(nufase2);

  free(tr3);
  free(nomtr3);
  free(numtr3);
  free(nufatr3);

  return ret;
}




