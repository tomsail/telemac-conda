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
 * - Nom du fichier : test24.c
 *
 * - Description : lecture de mailles/faces de type MED_POLYGONE
 *                 dans le maillage MED du fichier test23.med 
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
  char maa[MED_TAILLE_NOM+1];  
  med_int nmaa,i,mdim,npoly,j;
  char desc[MED_TAILLE_DESC+1];  
  med_int taille;
  med_int *con, *index, *num, *fam;
  char *nom;
  char tmp[MED_TAILLE_PNOM+1];
  int ind1, ind2,k;
  med_maillage type;

  /* Ouverture du fichier test23.med en lecture seule */
  fid = MEDouvrir("test23.med",MED_LECTURE);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test23.med");
    return -1;
  }
  printf("Ouverture du fichier test23.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMaa(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a la lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages = "IFORMAT"\n",nmaa);

  for (i=0;i<nmaa;i++) {

    /* Infos sur le maillage */
    if (MEDmaaInfo(fid,i+1,maa,&mdim,&type,desc) < 0) {
      MESSAGE("Erreur a la lecture des infos sur le maillage");
      return -1;
    }
    printf("maillage %d de nom [%s] et de dimension : "IFORMAT" \n",i+1,maa,mdim);
    
    /* Combien de mailles polygones en mode nodal */
    if ((npoly = MEDnEntMaa(fid,maa,MED_CONN,MED_MAILLE,MED_POLYGONE,MED_NOD)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de mailles MED_POLYGONE");
      return -1;
    }
    printf("Nombre de mailles polygones en mode nodal : "IFORMAT" \n",npoly); 

    /* Quelle taille pour le tableau des connectivites */
    if (MEDpolygoneInfo(fid,maa,MED_MAILLE,MED_NOD,&taille) < 0) {
      MESSAGE("Erreur a la lecture des infos sur les maillaes MED_POLYGONE");
      return -1;
    }
    printf("Taille a allouer pour la connectivite des polygones : "IFORMAT" \n",taille); 

    /* Allocation memoire : 
     *  - tableau d'index : npoly + 1
     *  - tableau des connectivites : taille
     *  - tableaux numeros et numeros de familles : npoly
     *  - tableau des noms : MED_TAILLE_PNOM*npoly + 1 
	 */
    index = (med_int *) malloc(sizeof(med_int)*(npoly+1));
    con = (med_int *) malloc(sizeof(med_int)*taille);
    num = (med_int *) malloc(sizeof(med_int)*npoly);
    fam = (med_int *) malloc(sizeof(med_int)*npoly);
    nom = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*npoly+1);

    /* Lecture de la connectivite des mailles polygones */
    if (MEDpolygoneConnLire(fid,maa,index,npoly+1,con,MED_MAILLE,MED_NOD) < 0) {
      MESSAGE("Erreur a la lecture de la connectivite des mailles MED_POLYGONE");
      ret = -1;
    }
    printf("Lecture de la connectivite des mailles MED_POLYGONE en mode nodal \n");

    /* Lecture noms */
    if (ret == 0) {
      if (MEDnomLire(fid,maa,nom,npoly,MED_MAILLE,MED_POLYGONE) < 0) {
	MESSAGE("Erreur a la lecture des noms des mailles MED_POLYGONE");
	ret = -1;
      }
      printf("Lecture des noms des mailles MED_POLYGONE \n");
    }
    
    /* Lecture des numeros */
    if (ret == 0) {
      if (MEDnumLire(fid,maa,num,npoly,MED_MAILLE,MED_POLYGONE) < 0) {
	MESSAGE("Erreur a la lecture des numeros des mailles MED_POLYGONE");
	ret = -1;
      }
      printf("Lecture des numeros des mailles MED_POLYGONE \n");
    }

    /* lecture des numeros de familles */
    if (ret == 0) {
      if ((ret = MEDfamLire(fid,maa,fam,npoly,MED_MAILLE,MED_POLYGONE)) < 0) {
	MESSAGE("Erreur a la lecture des numeros de famille des mailles MED_POLYGONE");
	ret = -1;
      }
      printf("Lecture des numeros de familles des mailles MED_POLYGONE \n");
    }

    if (ret == 0) {
      printf("Affichage des resultats \n");
      for (j=0;j<npoly;j++) {
	printf(">> Maille MED_POLYGONE "IFORMAT" : \n",j+1);
	printf("---- Connectivite       ----- : [ ");
	ind1 = *(index+j)-1;
	ind2 = *(index+j+1)-1;
	for (k=ind1;k<ind2;k++)
	  printf(IFORMAT" ",*(con+k));
	printf(" ] \n");
	strncpy(tmp,nom+j*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	tmp[MED_TAILLE_PNOM] = '\0';
	printf("---- Nom                ----- : %s \n",tmp);
	printf("---- Numero             ----- : "IFORMAT" \n",*(num+j));
	printf("---- Numero de famille  ----- : "IFORMAT" \n",*(fam+j));
      }
    }

    /* Liberation de la memoire */
    free(index);
    free(con);
    free(num);
    free(fam);
    free(nom);
  }

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");
 
  return ret; 
}
