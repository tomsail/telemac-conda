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
 * - Nom du fichier : test5.c
 *
 * - Description : lecture des noeuds d'un maillage MED.
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
  /* la dimension du maillage */
  med_int mdim;
  /* nom du maillage de longueur maxi MED_TAILLE_NOM */
  char maa[MED_TAILLE_NOM+1];
  /* le nombre de noeuds */
  med_int nnoe = 0;
  /* table des coordonnees */
  med_float *coo1,*coo2;
  /* tables des noms et des unites des coordonnees 
     profil : (dimension*MED_TAILLE_PNOM+1) */
  char nomcoo[2*MED_TAILLE_PNOM+1];
  char unicoo[2*MED_TAILLE_PNOM+1];
  /* tables des noms, numeros, numeros de familles des noeuds
     autant d'elements que de noeuds - les noms ont pout longueur
     MED_TAILLE_PNOM */
  char *nomnoe;
  med_int *numnoe;
  med_int *nufano; 
  med_repere rep;
  med_booleen inonoe,inunoe;
  char str[MED_TAILLE_PNOM+1];
  med_int i;
  med_int profil[2] = { 2, 3 };
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;

  /* Ouverture du fichier "test4.med" en lecture seule */
  fid = MEDouvrir("test4.med",MED_LECTURE);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test4.med");
    return -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0) {
    MESSAGE("Erreur a la lecture des informations sur le maillage : ");SSCRUTE(maa);
    return -1;
  } else
    printf("Maillage de nom : %s et de dimension : %d \n",maa,mdim);

  /* Combien de noeuds a lire ? */
  nnoe = MEDnEntMaa(fid,maa,MED_COOR,MED_NOEUD,0,0);
  if (nnoe < 0) {
    MESSAGE("Erreur a la lecture du nombre de noeuds dans : ");
    ret = -1;
  } else
    printf("Nombre de noeuds : "IFORMAT" \n",nnoe);

  /* Allocations memoires */
  if (nnoe > 0) {
    /* table des coordonnees 
       profil : (dimension * nombre de noeuds ) */
    coo1 = (med_float*) calloc(nnoe*mdim,sizeof(med_float));
    coo2 = (med_float*) calloc(nnoe*mdim,sizeof(med_float));
    /* table des des numeros, des numeros de familles des noeuds
       profil : (nombre de noeuds) */
    numnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
    nufano = (med_int*) malloc(sizeof(med_int)*nnoe);
    /* table des noms des noeuds 
       profil : (nnoe*MED_TAILLE_PNOM+1) */
    nomnoe = (char*) malloc(MED_TAILLE_PNOM*nnoe+1);
  }

  /* Lecture des composantes n°2 des coordonnees des noeuds */
  if (nnoe > 0) {
    if (MEDcoordLire(fid,maa,mdim,coo1,MED_FULL_INTERLACE,2,NULL,0,
		     &rep,nomcoo,unicoo) < 0) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo1 : ");
      for (i=0;i<nnoe*mdim;i++)
	printf("%4.2f ",coo1[i]);
      printf("\n");
    }
  }

  /* Lecture des composantes n°1 des coordonnees des noeuds */
  if (nnoe > 0) {
    if (MEDcoordLire(fid,maa,mdim,coo1,MED_FULL_INTERLACE,1,NULL,0,
		       &rep,nomcoo,unicoo) < 0) {
    MESSAGE("Erreur a la lecture des coordonnees des noeuds");
    ret = -1;
    } else {
      printf("Valeur de coo1 : ");
      for (i=0;i<nnoe*mdim;i++)
	printf("%4.2f ",coo1[i]);
      printf("\n");
    }
  }

  /* Lecture des composantes n°1 des coordonnees des noeuds du profil */
  if (nnoe > 0) { 
    if (MEDcoordLire(fid,maa,mdim,coo2,MED_FULL_INTERLACE,2,profil,2,
		       &rep,nomcoo,unicoo) < 0) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*mdim;i++)
	printf("%4.2f ",coo2[i]);
      printf("\n");
    }
  }

  /* Lecture des composantes n°2 des coordonnees des noeuds du profil */
  if (nnoe > 0) {
    if (MEDcoordLire(fid,maa,mdim,coo2,MED_FULL_INTERLACE,1,profil,2,
		       &rep,nomcoo,unicoo) < 0) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*mdim;i++) {
	printf("%4.2f ",coo2[i]);
	coo2[i] = 0.0;
      }
      printf("\n");
    }
  }

  /* Lecture de toutes les composantes des coordonnees des noeuds du profil */
  if (nnoe > 0) {
    if (MEDcoordLire(fid,maa,mdim,coo2,MED_FULL_INTERLACE,MED_ALL,profil,2,
		       &rep,nomcoo,unicoo) < 0) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*mdim;i++) {
	printf("%4.2f ",coo2[i]);
	coo2[i] = 0.0;
      }
      printf("\n");
    }
  }

  /* Lecture des composantes des coordonnees des noeuds */
  if (nnoe > 0) {
    if (MEDcoordLire(fid,maa,mdim,coo2,MED_FULL_INTERLACE,MED_ALL,NULL,0,
		       &rep,nomcoo,unicoo) < 0) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*mdim;i++)
	printf("%4.2f ",coo2[i]);
      printf("\n");
    }
  }

  /* Lecture des noms des noeuds (optionnel dans un maillage MED) */
  if ((nnoe > 0)) {
    if (MEDnomLire(fid,maa,nomnoe,nnoe,MED_NOEUD,0) < 0)
      inonoe = MED_FAUX;
    else
      inonoe = MED_VRAI;
  }

  /* Lecture des numeros des noeuds (optionnel dans un maillage MED) */
  if ((nnoe > 0)) {
    if (MEDnumLire(fid,maa,numnoe,nnoe,MED_NOEUD,0) < 0)
      inunoe = MED_FAUX;
    else
      inunoe = MED_VRAI;
  }

  /* Lecture des numeros de familles des noeuds */	      
  if ((nnoe > 0))
    if (MEDfamLire(fid,maa,nufano,nnoe,MED_NOEUD,0) < 0) {
      MESSAGE("Erreur a la lecture des numeros de famille des noeuds");
      ret = -1;
    } 

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0){
      MESSAGE("Erreur a la fermeture du fichier");
      ret = -1;
    } 

  /* Affichage des resulats */
  if (ret == 0 && nnoe > 0)
    {
      printf("Type de repere : %d \n",rep);
      printf("Nom des coordonnees : \n");
      for (i=0;i<mdim;i++)
	{
	  strncpy(str,nomcoo+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
          str[MED_TAILLE_PNOM] = '\0';
          printf("%s ",str);
	}
      printf("\nUnites des coordonnees : \n");
      for (i=0;i<mdim;i++)
	{
	  strncpy(str,unicoo+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
          str[MED_TAILLE_PNOM] = '\0';
          printf("%s ",str);
	}     
      printf("\nCoordonnees des noeuds : \n");
      for (i=0;i<nnoe*mdim;i++)
	printf("%f ",*(coo2+i));
      if (inonoe)
	{
	  printf("\nNoms des noeuds : \n");
	  for (i=0;i<nnoe;i++)
	    {
	      strncpy(str,nomnoe+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
              str[MED_TAILLE_PNOM] = '\0';
	      printf(" %s ",str);
	    }
	}
      if (inunoe)
	{
	  printf("\nNumeros des noeuds : \n");
	  for (i=0;i<nnoe;i++)
	      printf(IFORMAT" ",*(numnoe+i));
	}
      printf("\nNumeros des familles des noeuds : \n");
      for (i=0;i<nnoe;i++)
	printf(IFORMAT" ",*(nufano+i));
      printf("\n");
    }

  /* liberation memoire */
  if (nnoe > 0) {
    free(coo1);
    free(coo2);
    free(nomnoe);
    free(numnoe);
    free(nufano);
  }

  return ret;
}




