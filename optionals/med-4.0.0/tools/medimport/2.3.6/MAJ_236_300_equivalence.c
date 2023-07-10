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
#include "med_versioned.h"
#include <string.h>


#include "MAJ_236_300.h"
#include "MAJ_version.h"


med_err MAJ_236_300_equivalence(med_idt fid,const char * const maa)
{
  med_err ret = 0;
  med_int nequ=0,ncor=0,nstep=0,nocstpncor=0;
  med_int *cor;
  char equ[MED_NAME_SIZE+1]   ="";
  char des[MED_COMMENT_SIZE+1]="";

  int i,j,k;

  med_geometry_type *typmai = MED_GET_CELL_GEOMETRY_TYPE;
  med_geometry_type *typfac = MED_GET_FACE_GEOMETRY_TYPE;
  med_geometry_type *typare = MED_GET_EDGE_GEOMETRY_TYPE;

  MAJ_version_num(fid,2,3,6);

  /* Lecture du nombre d'equivalence */
  if ((nequ = MEDnEquivalence(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre d'equivalence");
    return -1;
  }
/*   printf("Nombre d'equivalences : "IFORMAT" \n",nequ); */

  /* Lecture de toutes les equivalences du maillage */
  for (i = 0;i<nequ;i++) {
/*     printf("Equivalence numero : %d \n",i+1); */


    /* Lecture des infos sur l'equivalence */
    if (MEDequivalenceInfo(fid,maa,i+1,equ,des,&nstep,&nocstpncor) < 0) {
      MESSAGE("Erreur a la lecture de l'equivalence d'indice");
      ISCRUTE_int(i+1);
      return -1;
    }
    fprintf(stdout,"  >>> Normalisation de l'équivalence [%s] du maillage [%s] \n",
	    equ,maa);

/*     printf("Nom de l'equivalence: |%s| \n",equ); */
/*     printf("Description de l'equivalence : |%s| \n",des); */
/*     printf("Nombre d'étapes de calcul : %d \n",nstep); */
/*     printf("Nombre de correspondances pour l'étape de calcul MED_NO_DT,MED_NO_IT : %d \n",nocstpncor); */

    MAJ_version_num(fid,3,0,8);
    /* Creation de l'equivalence */
    if (MEDequivalenceCr(fid,maa,equ,des) < 0) {
      MESSAGE("Erreur a la creation de l'equivalence");
      return -1;
    }
    MAJ_version_num(fid,2,3,6);

    /* Lecture des correspondances sur les differents types d'entites */

    /* Les noeuds */
    if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,&ncor) < 0) {
      MESSAGE("Erreur a la lecture du nombre de correspondance sur les noeuds");
      return -1;
    }
/*     printf("Il y a "IFORMAT" correspondances sur les noeuds \n",ncor); */
    if (ncor > 0) {
      cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
      if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					 MED_NODE,MED_NONE,cor) < 0) {
	MESSAGE("Erreur a la lecture des correspondances sur les noeuds");
	ret = -1;
      }
      if (ret == 0) {
/* 	for (j=0;j<ncor;j++) */
/* 	  printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",j+1,*(cor+2*j), */
/* 		 *(cor+2*j+1)); */

	/* Ecriture des equivalences sur les noeuds */
	MAJ_version_num(fid,3,0,8);
	if (MEDequivalenceCorrespondenceWr(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					   MED_NODE,MED_NONE,ncor,cor) < 0) {
	  MESSAGE("Erreur a l'ecriture du tableau des correspondances");
	  ret = -1;
	}
	MAJ_version_num(fid,2,3,6);
      }
      free(cor);
    }

    /* Les mailles : on ne prend pas en compte les mailles 3D */
    if (ret == 0)
      for (j=1;j<=MED_N_CELL_FIXED_GEO;j++) {

	if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_CELL,typmai[j],&ncor) < 0) {
	  MESSAGE("Erreur a la lecture du nombre de correspondance sur les mailles : ");
	  SSCRUTE(MED_GET_CELL_GEOMETRY_TYPENAME[j]);
	  return -1;
	}
/* 	printf("Il y a "IFORMAT" correspondances sur les mailles %s \n",ncor, */
/* 	       MED_GET_CELL_GEOMETRY_TYPENAME[j]); */
	if (ncor > 0) {
	  cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	  if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					     MED_CELL,typmai[j],cor) < 0) {
	    MESSAGE("Erreur a la lecture des correspondances sur les mailles : ");
	    SSCRUTE(MED_GET_CELL_GEOMETRY_TYPENAME[j]);
	    ret = -1;
	  }
	  if (ret == 0) {
/* 	    for (k=0;k<ncor;k++) */
/* 	      printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k), */
/* 		     *(cor+2*k+1)); */
	    /* Ecriture des equivalences sur les mailles */
	    MAJ_version_num(fid,3,0,8);
	    if (MEDequivalenceCorrespondenceWr(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					       MED_CELL,typmai[j],ncor,cor) < 0) {
	      MESSAGE("Erreur a l'ecriture du tableau des correspondances");
	      ret = -1;
	    }
	    MAJ_version_num(fid,2,3,6);
	  }
	  free(cor);
	}
      }

    /* Les faces */
    if (ret == 0)
      for (j=1;j<=MED_N_FACE_FIXED_GEO;j++) {
	if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					      MED_DESCENDING_FACE,typfac[j],&ncor) < 0) {
	  MESSAGE("Erreur a la lecture du nombre de correspondance sur les faces : ");
	  SSCRUTE(MED_GET_FACE_GEOMETRY_TYPENAME[j]);
	  return -1;
	}
/* 	printf("Il y a %d correspondances sur les faces %s \n",ncor, */
/* 	       MED_GET_FACE_GEOMETRY_TYPENAME[j]); */
	if (ncor > 0) {
	  cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	  if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					     MED_DESCENDING_FACE,typfac[j],cor) < 0) {
	    MESSAGE("Erreur a la lecture des correspondances sur les faces : ");
	    SSCRUTE(MED_GET_FACE_GEOMETRY_TYPENAME[j]);
	    ret = -1;
	  }
	  if (ret == 0) {
	    for (k=0;k<ncor;k++)
/* 	      printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k), */
/* 		     *(cor+2*k+1)); */

	    /* Ecriture des equivalences sur les mailles */
	    MAJ_version_num(fid,3,0,8);
	    if (MEDequivalenceCorrespondenceWr(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					       MED_DESCENDING_FACE,typfac[j],ncor,cor) < 0) {
	      MESSAGE("Erreur a l'ecriture du tableau des correspondances");
	      ret = -1;
	    }
	    MAJ_version_num(fid,2,3,6);
	  }
	  free(cor);
	}
      }

    /*  Les aretes */
    if (ret == 0)
      for (j=1;j<=MED_N_EDGE_FIXED_GEO;j++) {
	if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					      MED_DESCENDING_EDGE,typare[j],&ncor) < 0) {
	  MESSAGE("Erreur a la lecture du nombre de correspondance sur les aretes : ");
	  SSCRUTE(MED_GET_EDGE_GEOMETRY_TYPENAME[j]);
	  return -1;
	}
/* 	printf("Il y a "IFORMAT" correspondances sur les aretes %s \n",ncor, */
/* 	       MED_GET_EDGE_GEOMETRY_TYPENAME[j]); */
	if (ncor > 0) {
	  cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	  if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					     MED_DESCENDING_EDGE,typare[j],cor) < 0) {
	    MESSAGE("Erreur a la lecture des correspondances sur les faces : ");
	    SSCRUTE(MED_GET_EDGE_GEOMETRY_TYPENAME[j]);
	    ret = -1;
	  }
	  if (ret == 0) {
	    for (k=0;k<ncor;k++)
/* 	      printf("Correspondance %d : "IFORMAT" et "IFORMAT" \n",k+1,*(cor+2*k), */
/* 		     *(cor+2*k+1)); */

	    /* Ecriture des equivalences sur les mailles */
	    MAJ_version_num(fid,3,0,8);
	    if (MEDequivalenceCorrespondenceWr(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					       MED_DESCENDING_EDGE,typare[j],ncor,cor) < 0) {
	      MESSAGE("Erreur a l'ecriture du tableau des correspondances");
	      ret = -1;
	    }
	    MAJ_version_num(fid,2,3,6);
	  }
	  free(cor);
	}
      }

  }

  MAJ_version(fid);
  return ret;
}




