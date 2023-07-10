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
 * - Nom du fichier : test15.c
 *
 * - Description : lecture des noeuds d'un maillage MED
 *                 a l'aide des routines de niveau 2
 *                 - equivalent a test5.c
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
  med_float *coo;
  /* tables des noms et des unites des coordonnees 
     profil : (dimension*MED_TAILLE_PNOM+1) */
  char nomcoo[3*MED_TAILLE_PNOM+1];
  char unicoo[3*MED_TAILLE_PNOM+1];
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
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;

  /* Ouverture du fichier passe en argument en lecture seule */
  if ((fid= MEDouvrir(argv[1],MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier :");
    SSCRUTE(argv[1]);
    return -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0) {
    MESSAGE("Erreur a la lecture des informations du 1er maillage");
    return -1;
  }
  printf("Maillage de nom : %s et de dimension : "IFORMAT" \n",maa,mdim);

  /* Lecture du nombre de noeuds */
  if ((nnoe = MEDnEntMaa(fid,maa,MED_COOR,MED_NOEUD,0,0)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de noeuds ");
    return -1;
  }
  printf("Nombre de noeuds : %d \n",nnoe);

  /* Allocations memoires */
  /* table des coordonnees 
     profil : (dimension * nombre de noeuds ) */
  if (nnoe > 0) {
    coo = (med_float*) malloc(sizeof(med_float)*nnoe*mdim);
    /* table des des numeros, des numeros de familles des noeuds
       profil : (nombre de noeuds) */
    numnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
    nufano = (med_int*) malloc(sizeof(med_int)*nnoe);
    /* table des noms des noeuds 
       profil : (nnoe*MED_TAILLE_PNOM+1) */
    nomnoe = (char*) malloc(MED_TAILLE_PNOM*nnoe+1);

    /* Lecture des noeuds : 
       - Coordonnees
       - Noms (optionnel dans un fichier MED) 
       - Numeros (optionnel dans un fichier MED) 
       - Numeros de familles	*/
    if (MEDnoeudsLire(fid,maa,mdim,coo,MED_FULL_INTERLACE,&rep,nomcoo,unicoo,
		      nomnoe,&inonoe,numnoe,&inunoe,nufano,nnoe) < 0) {
      MESSAGE("Erreur a la lecture des noeuds du maillage");
      ret = -1;
    }

    /* Affichage */
    if (ret == 0) {
      printf("Type de repere : %d \n",rep);
      printf("Nom des coordonnees : \n");
      for (i=0;i<mdim;i++) {
	strncpy(str,nomcoo+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	str[MED_TAILLE_PNOM] = '\0';
	printf("%s ",str);
      }
      printf("\nUnites des coordonnees : \n");
      for (i=0;i<mdim;i++) {
	strncpy(str,unicoo+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	str[MED_TAILLE_PNOM] = '\0';
	printf("%s ",str);
      }     
      printf("\nCoordonnees des noeuds : \n");
      for (i=0;i<nnoe*mdim;i++)
	printf("%f ",*(coo+i));
      if (inonoe) {
	printf("\nNoms des noeuds : \n");
	for (i=0;i<nnoe;i++) {
	  strncpy(str,nomnoe+i*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	  str[MED_TAILLE_PNOM] = '\0';
	  printf(" %s ",str);
	}
      }
      if (inunoe) {
	printf("\nNumeros des noeuds : \n");
	for (i=0;i<nnoe;i++)
	  printf("%d ",*(numnoe+i));
      }
      printf("\nNumeros des familles des noeuds : \n");
      for (i=0;i<nnoe;i++)
	printf(IFORMAT" ",*(nufano+i));
      printf("\n");
    }

    /* Liberation memoire */
    free(coo);
    free(nomnoe);
    free(numnoe);
    free(nufano);
  }

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
 
  return ret;
}




