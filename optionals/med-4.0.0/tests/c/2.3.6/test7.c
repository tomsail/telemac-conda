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
 * - Nom du fichier : test7.c
 *
 * - Description : lecture des elements du maillage MED crees par test6
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
  med_int *se2_1;
  med_int *se2_2;
  char *nomse2;
  med_int *numse2;
  med_int *nufase2; 
  med_int ntr3;
  med_int *tr3;
  char *nomtr3;
  med_int *numtr3;
  med_int *nufatr3;
  char maa[MED_TAILLE_NOM+1] ="maa1";
  med_int mdim = 2;
  med_booleen inoele,inuele;
  med_int tse2,ttr3;
  med_int i;
  char str[MED_TAILLE_PNOM+1];
  med_int profil[2] = { 2, 3 };
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;
  
  /* Ouverture du fichier en mode lecture seule */
  if ((fid = MEDouvrir("test6.med",MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test6.med");
    return -1;
  }

  /* Lecture des informations sur le premier maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0)  {
    MESSAGE("Erreur a la lecture des information sur le 1er maillage");
    return -1;
  } else
    printf("Maillage de nom : %s et de dimension %d \n",maa,mdim);

  /* Combien de triangles et de segments */
  if ((nse2 = MEDnEntMaa(fid,maa,MED_CONN,MED_ARETE,MED_SEG2,MED_DESC)) < 0)  {
    MESSAGE("Erreur a la lecture du nombre de faces MED_SEG2");
    return -1;
  }
  if ((ntr3 = MEDnEntMaa(fid,maa,MED_CONN,MED_MAILLE,MED_TRIA3,MED_DESC))<0) {
    MESSAGE("Erreur a la lecture du nombre de mailles MED_TRIA3");
    return -1;
  }
  printf("Nombre de MED_SEG2 : "IFORMAT" - nombre de MED_TRIA3 : "IFORMAT"\n",nse2,ntr3);

  /* Allocations memoire */ 
  tse2 = 2;
  se2_1  = (med_int*) calloc(tse2*nse2,sizeof(med_int));
  se2_2  = (med_int*) malloc(sizeof(med_int)*tse2*nse2);
  nomse2 = (char*) malloc(MED_TAILLE_PNOM*nse2+1);
  numse2 = (med_int*) malloc(sizeof(med_int)*nse2);
  nufase2 = (med_int*) malloc(sizeof(med_int)*nse2);

  ttr3 = 3;
  tr3 = (med_int*) malloc(sizeof(med_int)*ntr3*ttr3);
  nomtr3 = (char*) malloc(MED_TAILLE_PNOM*ntr3+1);
  numtr3 = (med_int*) malloc(sizeof(med_int)*ntr3);
  nufatr3 = (med_int*) malloc(sizeof(med_int)*ntr3);

  /* Lecture des connectivites des segments avec profil */
  if (MEDconnLire(fid,maa,mdim,se2_1,MED_FULL_INTERLACE,profil,2,
		  MED_ARETE,MED_SEG2,MED_DESC) < 0) {
    MESSAGE("Erreur a la lecture de la connectivite des segments");
    return -1;
  }

  /* Lecture de la connectivite des segments */
  if (MEDconnLire(fid,maa,mdim,se2_2,MED_FULL_INTERLACE,NULL,0,
		  MED_ARETE  ,MED_SEG2,MED_DESC) < 0) {
    MESSAGE("Erreur a la lecture de la connectivite des segments");
    return -1;
  }

  /* Lecture (optionnelle) des noms des segments */
  if (MEDnomLire(fid,maa,nomse2,nse2,MED_ARETE,MED_SEG2) < 0)
    inoele = MED_FAUX;
  else
    inoele = MED_VRAI;

  /* Lecture (optionnelle) des numeros des segments */
  if (MEDnumLire(fid,maa,numse2,nse2,MED_ARETE,MED_SEG2) < 0)
    inuele = MED_FAUX;
  else
    inuele = MED_VRAI;

  /* Lecture des numeros des familles des segments */
  if (MEDfamLire(fid,maa,nufase2,nse2,MED_ARETE,MED_SEG2) < 0) {
    MESSAGE("Erreur a la lecture des numéros de famille des segments");
    return -1;
  }

  /* Lecture de la connectivite des triangles */
  if (MEDconnLire(fid,maa,mdim,tr3,MED_NO_INTERLACE,NULL,0,MED_MAILLE,MED_TRIA3,
		     MED_DESC) < 0) {
    MESSAGE("Erreur a la lecture de la connectivite des triangles");
    return -1;
  }

  /* Lecture (optionnelle) des noms des triangles */
  if (MEDnomLire(fid,maa,nomtr3,ntr3,MED_MAILLE,MED_TRIA3) < 0)
    inoele = MED_FAUX;
  else
    inoele = MED_VRAI;

  /* Lecture (optionnelle) des numeros des triangles */
  if (MEDnumLire(fid,maa,numtr3,ntr3,MED_MAILLE,MED_TRIA3) < 0)
    inuele = MED_FAUX;
  else
    inuele = MED_VRAI;

  /* Lecture des numeros des familles des triangles */
  if (ret = MEDfamLire(fid,maa,nufatr3,ntr3,MED_MAILLE,MED_TRIA3) < 0) {
    MESSAGE("Erreur a la lecture des numeros de famille des segments");
    return -1;
  }

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  /* Affichage */
  if (ret == 0) {
    printf("Connectivite des segments (1): \n");
    for (i=0;i<nse2*tse2;i++)
      printf(IFORMAT" ",*(se2_1+i));
    printf("\n");
    printf("Connectivite des segments (2): \n");
    for (i=0;i<nse2*tse2;i++)
      printf(IFORMAT" ",*(se2_2+i));
    if (inoele) {
      printf("\nNoms des segments :\n");
      for (i=0;i<nse2;i++) {
	strncpy(str,nomse2+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	str[MED_TAILLE_PNOM] = '\0';
	printf("%s ",str);
      }
    }
    if (inuele) {
      printf("\nNumeros des segments :\n");
      for (i=0;i<nse2;i++)
	printf(IFORMAT" ",*(numse2+i));
    }      
    printf("\nNumeros des familles des segments :\n");
    for (i=0;i<nse2;i++)
      printf(IFORMAT" ",*(nufase2+i));

    printf("\nConnectivite des triangles : \n");
    for (i=0;i<ntr3*ttr3;i++)
      printf(IFORMAT" ",*(tr3+i));
    if (inoele) {
      printf("\nNoms des triangles :\n");
      for (i=0;i<ntr3;i++) {
	strncpy(str,nomtr3+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	str[MED_TAILLE_PNOM] = '\0';
	printf("%s ",str);
      }
    }
    if (inuele) {
      printf("\nNumeros des triangles :\n");
      for (i=0;i<ntr3;i++)
	printf(IFORMAT" ",*(numtr3+i));
    }      
    printf("\nNumeros des familles des triangles :\n");
    for (i=0;i<ntr3;i++)
      printf(IFORMAT" ",*(nufatr3+i));
    
    printf("\n");
  }

  /* Nettoyage memoire */
  free(se2_1);
  free(se2_2);
  free(nomse2);
  free(numse2);
  free(nufase2);

  free(tr3);
  free(nomtr3);
  free(numtr3);
  free(nufatr3);

  return ret;
}

