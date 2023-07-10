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
 * - Nom du fichier : test9.c
 *
 * - Description : lecture des familles d'un maillage MED 
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
  med_int mdim;
  med_int nfam;
  med_int i,j;
  med_int natt,ngro;
  char *attdes,*gro;
  med_int *attval,*attide;
  char nomfam[MED_TAILLE_NOM+1];
  med_int numfam;
  char str1[MED_TAILLE_DESC+1];
  char str2[MED_TAILLE_LNOM+1];
  char desc[MED_TAILLE_DESC+1];
  med_maillage type;

  /* Ouverture du fichier "test8.med" en lecture seule */
  if ((fid = MEDouvrir("test8.med",MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test8.med");
    return -1;
  }

  /* Lecture des information sur le 1er maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,desc) < 0) {
    MESSAGE("Erreur a la lecture des informations du premier maillage");
    return -1;
  }

  /* Lecture du nombre de familles */
  if ((nfam = MEDnFam(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de famille");
    return -1;
  }
  printf("Nombre de familles : "IFORMAT" \n",nfam);

  /* Lecture de chaque famille */
  for (i=0;i<nfam;i++) {
	
    /* Lecture du nombre de groupe */
    if ((ngro = MEDnGroupe(fid,maa,i+1)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de groupe de la famille d'indice : ");
      ISCRUTE(i+1);
      ret = -1;
    }
	
    /* Lecture du nombre d'attribut */
    if ((natt = MEDnAttribut(fid,maa,i+1)) < 0) {
      MESSAGE("Erreur a la lecture du nombre d'attribut de la famille d'indice : ");
      ISCRUTE(i+1);
      ret = -1;
    }

    if (ret == 0)
      printf("Famille "IFORMAT" a "IFORMAT" attributs et "IFORMAT" groupes \n",i+1,natt,ngro); 

    /* Lecture des informations sur la famille */
    if (ret == 0) {
      /* Allocations memoire */
      attide = (med_int*) malloc(sizeof(med_int)*natt);
      attval = (med_int*) malloc(sizeof(med_int)*natt);	    
      attdes = (char *) malloc(MED_TAILLE_DESC*natt+1);
      gro = (char*) malloc(MED_TAILLE_LNOM*ngro+1);

      if (MEDfamInfo(fid,maa,i+1,nomfam,&numfam,attide,attval,attdes,
	       	     &natt,gro,&ngro) < 0) {
	MESSAGE("Erreur a la lecture des informations de la famille d'indice : ");
	ISCRUTE(i+1);
	ret = -1;
      }

      if (ret == 0) {
	printf("Famille de nom %s et de numero "IFORMAT" : \n",nomfam,numfam);
	printf("Attributs : \n");
	for (j=0;j<natt;j++) {
	  strncpy(str1,attdes+j*MED_TAILLE_DESC,MED_TAILLE_DESC);
	  str1[MED_TAILLE_DESC] = '\0';
	  printf("ide = "IFORMAT" - val = "IFORMAT" - des = %s\n",*(attide+j),
		 *(attval+j),str1);
	}
	free(attide);
	free(attval);
	free(attdes);	
	
	for (j=0;j<ngro;j++) {
	  strncpy(str2,gro+j*MED_TAILLE_LNOM,MED_TAILLE_LNOM);
	  str2[MED_TAILLE_LNOM] = '\0';
	  printf("gro = %s\n",str2);
	      }
	free(gro);
      }
    }
  }

  /* Fermeture du fichier */
  if (MEDfermer(fid)  < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  
  return ret;
}
