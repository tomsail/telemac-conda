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
 * - Nom du fichier : test26.c
 *
 * - Description : lecture de mailles de type MED_POLYEDRE
 *                 dans le maillage MED du fichier test25.med 
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
  med_int taille,nf,np;
  med_int taille2,nf2,np2;
  med_int *conn, *conn2, *indexf, *indexf2, *num, *fam;
  med_int *indexp, *indexp2;
  char *nom;
  char tmp[MED_TAILLE_PNOM+1];
  int ind1, ind2,k,nfaces,nnoeuds,l;
  med_maillage type;

  /* Ouverture du fichier test25.med en lecture seule */
  fid = MEDouvrir("test25.med",MED_LECTURE);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test25.med");
    return -1;
  }
  printf("Ouverture du fichier test25.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMaa(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages = "IFORMAT"\n",nmaa);

  for (i=0;i<nmaa;i++)
    if (ret == 0) {
      
      /* Lecture des infos sur le maillage */
      if (MEDmaaInfo(fid,i+1,maa,&mdim,&type,desc) < 0) {
	MESSAGE("Erreur a lecture des infos sur le maillage");
	return -1;
      }
      printf("maillage "IFORMAT" de nom [%s] et de dimension : "IFORMAT" \n",i+1,maa,mdim);
      
      /* Combien de mailles polyedres en mode nodal */
      if ((npoly = MEDnEntMaa(fid,maa,MED_CONN,MED_MAILLE,MED_POLYEDRE,MED_NOD)) < 0) {
	MESSAGE("Erreur a lecture du nombre de maille MED_POLYEDRE en mode nodal");
	return -1;
      }
      printf("Nombre de mailles polyedres : "IFORMAT" \n",npoly); 
      
      /* Quelle taille pour le tableau des connectivites et d'indexation 
	 en mode MED_NOD */
      if (MEDpolyedreInfo(fid,maa,MED_NOD,&nf,&taille) < 0) {
	MESSAGE("Erreur a lecture des infos sur les polyedres");
	return -1;
      }
      printf("Taille a allouer pour la connectivite nodale des polyedres : "IFORMAT" \n",taille);
      printf("Taille a allouer pour le tableau d'indexation des faces : "IFORMAT" \n",nf); 
      
      /* Quelle taille pour le tableau des connectivites et d'indexation 
	 en mode MED_DESC */
      if (MEDpolyedreInfo(fid,maa,MED_DESC,&nf2,&taille2) < 0) {
	MESSAGE("Erreur a la lecture des infos sur les polyedres");
	return -1;
      }
      printf("Taille a allouer pour la connectivite descendante des polyedres : "IFORMAT" \n",taille2);
      printf("Taille a allouer pour le tableau d'indexation des types de faces : "IFORMAT" \n",nf2); 
      
      /* Allocation memoire : 
       *  - tableau indexp et indexp2 : npoly + 1
       *  - tableau indexf et indexf2 : nf et nf2
       *  - tableau des connectivites : consize
       *  - tableaux numeros et numeros de familles : npoly
       *  - tableau des noms : MED_TAILLE_PNOM*npoly + 1 
       */
      indexp   = (med_int *) malloc(sizeof(med_int)*(npoly+1));
      indexp2  = (med_int *) malloc(sizeof(med_int)*(npoly+1));
      indexf   = (med_int *) malloc(sizeof(med_int)*nf);
      indexf2  = (med_int *) malloc(sizeof(med_int)*nf2);
      conn     = (med_int *) malloc(sizeof(med_int)*taille);
      conn2    = (med_int *) malloc(sizeof(med_int)*taille2);
      num      = (med_int *) malloc(sizeof(med_int)*npoly);
      fam      = (med_int *) malloc(sizeof(med_int)*npoly);
      nom      = (char *) malloc(sizeof(char)*MED_TAILLE_PNOM*npoly+1);
      
      /* Lecture de la connectivite des mailles polyedres en mode nodal */
      if (MEDpolyedreConnLire(fid,maa,indexp,npoly+1,indexf,nf,conn,MED_NOD) < 0) {
	MESSAGE("Erreur a lecture de la connectivite nodale des polyedres");
	ret = -1;
      }
      printf("Lecture de la connectivite des mailles MED_POLYEDRE en mode nodal \n");
      
      /* Lecture de la connectivite des mailles polyedres en mode descendant */ 
      if (ret == 0) {
	if (MEDpolyedreConnLire(fid,maa,indexp2,npoly+1,indexf2,nf2,conn2,MED_DESC) < 0) {
	  MESSAGE("Erreur a lecture de la connectivite descendante des polyedres");
	  ret = -1;
	}
	printf("Lecture de la connectivite des mailles MED_POLYEDRE en mode descendant \n");
      }
      
      /* Lecture noms */
      if (ret == 0) {
	if (MEDnomLire(fid,maa,nom,npoly,MED_MAILLE,MED_POLYEDRE) < 0) {
	  MESSAGE("Erreur a lecture des noms des polyedres");
	  ret = -1;
	}
	printf("Lecture des noms des mailles MED_POLYEDRE \n");
      }
      
      /* Lecture des numeros */
      if (ret == 0) {
	if (MEDnumLire(fid,maa,num,npoly,MED_MAILLE,MED_POLYEDRE) < 0) {
	  MESSAGE("Erreur a lecture des numeros des polyedres");
	  ret = -1;
	}
	printf("Lecture des numeros des mailles MED_POLYEDRE \n");
      }
      
      /* Lecture des numeros de familles */
      if (ret == 0) {
	if (MEDfamLire(fid,maa,fam,npoly,MED_MAILLE,MED_POLYEDRE) < 0) {
	  MESSAGE("Erreur a lecture des numeros de famille des polyedres");
	  ret = -1;
	}
	printf("Lecture des numeros de familles des mailles MED_POLYEDRE \n");
      }
      
      if (ret == 0) {
	printf("Affichage des resultats \n");
	for (j=0;j<npoly;j++) {
	  printf(">> Maille MED_POLYEDRE "IFORMAT" : \n",j+1);
	  printf("---- Connectivite nodale      ----- : \n"); 
	  nfaces  = *(indexp+j+1) - *(indexp+j);
	  /* ind1 = indice dans "indexf" pour acceder aux numeros des faces */
	  ind1 = *(indexp+j) - 1;
	  for (k=0;k<nfaces;k++) {
	    /* ind2 = indice dans "conn" pour acceder au premier noeud de la face */
	    ind2 = *(indexf+ind1+k) - 1;
	    nnoeuds = *(indexf+ind1+k+1) - *(indexf+ind1+k);
	    printf("   - Face %d : [ ", k+1);
	    for (l=0;l<nnoeuds;l++)
	      printf(" "IFORMAT" ",*(conn+ind2+l));
	    printf(" ] \n"); 
	  }
	  printf("---- Connectivite descendante ----- : \n");
	  nfaces  = *(indexp2+j+1) - *(indexp2+j);
	  /* ind1 = indice dans "conn2" pour acceder aux numeros des faces */
	  ind1 = *(indexp2+j) - 1;
	  for (k=0;k<nfaces;k++) 
	    printf("   - Face %d de numero : "IFORMAT" et de type "IFORMAT" \n", k+1,*(conn2+ind1+k),*(indexf2+ind1+k));
	  strncpy(tmp,nom+j*MED_TAILLE_PNOM,MED_TAILLE_PNOM);
	  tmp[MED_TAILLE_PNOM] = '\0';
	  printf("---- Nom                      ----- : %s \n",tmp);
	  printf("---- Numero                   ----- : "IFORMAT" \n",*(num+j));
	  printf("---- Numero de famille        ----- : "IFORMAT" \n",*(fam+j));
	}
      }
      
      /* liberation de la memoire */
      free(indexp);
      free(indexp2);
      free(indexf);
      free(indexf2);
      free(conn);
      free(conn2);
      free(num);
      free(fam);
      free(nom);
    }
  
  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");
  
  return ret; 
}
