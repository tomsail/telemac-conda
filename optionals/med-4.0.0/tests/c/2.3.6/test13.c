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
 * - Nom du fichier : test13.c
 *
 * - Description : lecture des equivalences d'un maillage MED.
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include <med_utils.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

/* On prend en compte tous les types de mailles concernees
 *    par les equivalences */
#define MED_NBR_MAILLE_EQU 8
 
int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  char maa[MED_TAILLE_NOM+1];
  med_int mdim;
  med_int nequ,ncor;
  med_int *cor;
  char equ[MED_TAILLE_NOM+1];
  char des[MED_TAILLE_DESC+1];
  med_geometrie_element typmai[MED_NBR_GEOMETRIE_MAILLE+1] = {MED_POINT1,MED_SEG2, 
							      MED_SEG3,MED_TRIA3,
							      MED_TRIA6,MED_QUAD4,
							      MED_QUAD8,MED_POLYGONE};
  med_geometrie_element typfac[MED_NBR_GEOMETRIE_FACE+1] = {MED_TRIA3,MED_TRIA6,
							    MED_QUAD4,MED_QUAD8,
							    MED_POLYGONE};
  med_geometrie_element typare[MED_NBR_GEOMETRIE_ARETE] = {MED_SEG2,MED_SEG3};  
  int i,j,k;
  med_maillage type;

  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en paramètre");
    return -1;
  }

  /* Ouverture du fichier passe en argument en lecture seule */
  if ((fid = MEDouvrir(argv[1],MED_LECTURE)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }
  
  /* Lecture des infos sur le premier maillage */
  if (MEDmaaInfo(fid,1,maa,&mdim,&type,des) < 0) {
    MESSAGE("Erreur a lecture des infos sur le 1er maillage"); 
    return -1;
  }
  printf("Maillage de nom %s et de dimension "IFORMAT" \n",maa,mdim);

  /* Lecture du nombre d'equivalence */
  if ((nequ = MEDnEquiv(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre d'equivalence"); 
    return -1;
  }
  printf("Nombre d'equivalences : "IFORMAT" \n",nequ);

  /* Lecture de toutes les equivalences du maillage */
  if (nequ > 0)
    for (i = 0;i<nequ;i++) {
      printf("Equivalence numero : %d \n",i+1);

      /* Lecture des infos sur l'equivalence */
      if (MEDequivInfo(fid,maa,i+1,equ,des) < 0) {
	MESSAGE("Erreur a la lecture de l'equivalence d'indice");
	ISCRUTE_int(i+1);
	return -1;
      }
      printf("Nom de l'equivalence: %s \n",equ);
      printf("Description de l'equivalence : %s \n",des);

      /* Lecture des correspondances sur les differents types d'entites */

      /* Les noeuds */
      if ((ncor = MEDnCorres(fid,maa,equ,MED_NOEUD,0)) < 0) {
	MESSAGE("Erreur a la lecture du nombre de correspondance sur les noeuds");
	return -1;
      }
      printf("Il y a "IFORMAT" correspondances sur les noeuds \n",ncor);
      if (ncor > 0) {
	cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	if (MEDequivLire(fid,maa,equ,cor,ncor,MED_NOEUD,0) < 0) {
	  MESSAGE("Erreur a la lecture des correspondances sur les noeuds");
	  ret = -1;
	}
	if (ret == 0)
	  for (j=0;j<ncor;j++)
	    printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",j+1,*(cor+2*j),
		   *(cor+2*j+1));
	free(cor);
      }
	    
      /* Les mailles : on ne prend pas en compte les mailles 3D */
      if (ret == 0)
	for (j=0;j<MED_NBR_MAILLE_EQU;j++) {

	  if ((ncor = MEDnCorres(fid,maa,equ,MED_MAILLE,typmai[j])) < 0) {
	    MESSAGE("Erreur a la lecture du nombre de correspondance sur les mailles : ");
	    ISCRUTE_int(typmai[j]);
	    return -1;
	  }
	  printf("Il y a "IFORMAT" correspondances sur les mailles %d \n",ncor,
		 typmai[j]);
	  if (ncor > 0) {
	    cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	    if (MEDequivLire(fid,maa,equ,cor,ncor,MED_MAILLE,
			     typmai[j]) < 0) {
	      MESSAGE("Erreur a la lecture des correspondances sur les mailles : ");
	      ISCRUTE_int(typmai[j]);
	      ret = -1;
	    }
	    if (ret == 0)
	      for (k=0;k<ncor;k++)
		printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k),
		       *(cor+2*k+1));
	    free(cor);
	  }
	}

      /* Les faces */
      if (ret == 0)
	for (j=0;j<MED_NBR_GEOMETRIE_FACE+1;j++) {
	  if ((ncor = MEDnCorres(fid,maa,equ,MED_FACE,typfac[j])) < 0) {
	    MESSAGE("Erreur a la lecture du nombre de correspondance sur les faces : ");
	    ISCRUTE_int(typfac[j]);
	    return -1;
	  }
	  printf("Il y a %d correspondances sur les faces "IFORMAT" \n",ncor,
		 typfac[j]);
	  if (ncor > 0) {
	    cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	    if (MEDequivLire(fid,maa,equ,cor,ncor,MED_FACE,
			     typfac[j]) < 0) {
	      MESSAGE("Erreur a la lecture des correspondances sur les faces : ");
	      ISCRUTE_int(typfac[j]);
	      ret = -1;
	    }
	    if (ret == 0)  
	      for (k=0;k<ncor;k++)
		printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k),
		       *(cor+2*k+1));
	    free(cor);
	  }
	}

      /*  Les aretes */
      if (ret == 0)
	for (j=0;j<MED_NBR_GEOMETRIE_ARETE;j++) {
	  if ((ncor = MEDnCorres(fid,maa,equ,MED_ARETE,typare[j])) < 0) {
	    MESSAGE("Erreur a la lecture du nombre de correspondance sur les aretes : ");
	    ISCRUTE_int(typare[j]);
	    return -1;
	  }
	  printf("Il y a "IFORMAT" correspondances sur les aretes %d \n",ncor,
		 typare[j]);
	  if (ncor > 0) {
	    cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	   if (MEDequivLire(fid,maa,equ,cor,ncor,MED_ARETE,
			       typare[j]) < 0) {
	     MESSAGE("Erreur a la lecture des correspondances sur les faces : ");
	     ISCRUTE_int(typare[j]);
	     ret = -1;
	   }
	   if (ret == 0)
	     for (k=0;k<ncor;k++)
	       printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k),
		      *(cor+2*k+1));
	   free(cor);
	  }
	}

    }			    

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return ret;
}




