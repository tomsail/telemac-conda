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
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif

int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid = 0;
  char    maa[MED_NAME_SIZE+1]="";
  med_int mdim=0,sdim=0;
  med_int nequ=0,ncor=0,nstep=0,nocstpncor=0;
  med_int *cor;
  char equ[MED_NAME_SIZE+1]   ="";
  char des[MED_COMMENT_SIZE+1]="";

  int i,j,k;
  med_mesh_type type;
  med_sorting_type sort;
  char desc[MED_COMMENT_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[3*MED_SNAME_SIZE+1];
  char unicoo[3*MED_SNAME_SIZE+1];
  med_axis_type rep;

  med_geometry_type *typmai = MED_GET_CELL_GEOMETRY_TYPE;
  med_geometry_type *typfac = MED_GET_FACE_GEOMETRY_TYPE;
  med_geometry_type *typare = MED_GET_EDGE_GEOMETRY_TYPE;

  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en paramètre");
    return -1;
  }

  /* Ouverture du fichier passe en argument en lecture seule */
  if ((fid = MEDfileOpen(argv[1],MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }

  if ((sdim=MEDmeshnAxis(fid, 1)) <0) {
    MESSAGE("Erreur a la lecture de la dimension de l'espace du maillage :");
    SSCRUTE(maa);
    return -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if ( MEDmeshInfo( fid, 1,  maa, &sdim, &mdim, &type, desc, dtunit, &sort,
		    &nstep,  &rep, nomcoo,unicoo) < 0 ) {
    MESSAGE("Erreur a la lecture des informations sur le maillage : ");SSCRUTE(maa);
    return -1;
  } else {
    printf("Maillage de nom : |%s| , de dimension : "IFORMAT" , et de type %d\n",maa,mdim,type);
    printf("\t -Dimension de l'espace : "IFORMAT"\n",sdim);
    printf("\t -Description du maillage : %s\n",desc);
    printf("\t -Noms des axes : |%s|\n",nomcoo);
    printf("\t -Unités des axes : |%s|\n",unicoo);
    printf("\t -Type de repère : %d\n",rep);
    printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n",dtunit);
  }

  /* Lecture du nombre d'equivalence */
  if ((nequ = MEDnEquivalence(fid,maa)) < 0)
    {
    MESSAGE("Erreur a la lecture du nombre d'equivalence");
    return -1;
  }
  printf("Nombre d'equivalences : "IFORMAT" \n",nequ);

  /* Lecture de toutes les equivalences du maillage */
  if (nequ > 0)
    for (i = 0;i<nequ;i++)
      {
      printf("Equivalence numero : %d \n",i+1);

      /* Lecture des infos sur l'equivalence */
      if (MEDequivalenceInfo(fid,maa,i+1,equ,des,&nstep,&nocstpncor) < 0)
	{
	  MESSAGE("Erreur a la lecture de l'equivalence d'indice");
	  ISCRUTE_int(i+1);
	  return -1;
	}
      printf("Nom de l'equivalence: |%s| \n",equ);
      printf("Description de l'equivalence : |%s| \n",des);
      printf("Nombre d'étapes de calcul : "IFORMAT" \n",nstep);
      printf("Nombre de correspondances pour l'étape de calcul MED_NO_DT,MED_NO_IT : "IFORMAT" \n",nocstpncor);

      /* Lecture des correspondances sur les differents types d'entites */

      /* Les noeuds */
      if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,&ncor) < 0) {
	MESSAGE("Erreur a la lecture du nombre de correspondance sur les noeuds");
	return -1;
      }
      printf("Il y a "IFORMAT" correspondances sur les noeuds \n",ncor);
      if (ncor > 0) {
	cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					   MED_NODE,MED_NONE,cor) < 0) {
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
	for (j=1;j<=MED_N_CELL_FIXED_GEO;j++) {

	  if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,MED_CELL,typmai[j],&ncor) < 0) {
	    MESSAGE("Erreur a la lecture du nombre de correspondance sur les mailles : ");
	    SSCRUTE(MED_GET_CELL_GEOMETRY_TYPENAME[j]);
	    return -1;
	  }
	  printf("Il y a "IFORMAT" correspondances sur les mailles %s \n",ncor,
		 MED_GET_CELL_GEOMETRY_TYPENAME[j]);
	  if (ncor > 0) {
	    cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	    if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					       MED_CELL,typmai[j],cor) < 0) {
	      MESSAGE("Erreur a la lecture des correspondances sur les mailles : ");
	      SSCRUTE(MED_GET_CELL_GEOMETRY_TYPENAME[j]);
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
	for (j=1;j<=MED_N_FACE_FIXED_GEO;j++) {
	  if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,
						MED_DESCENDING_FACE,typfac[j],&ncor) < 0) {
	    MESSAGE("Erreur a la lecture du nombre de correspondance sur les faces : ");
	    SSCRUTE(MED_GET_FACE_GEOMETRY_TYPENAME[j]);
	    return -1;
	  }
	  printf("Il y a "IFORMAT" correspondances sur les faces %s \n",ncor,
		 MED_GET_FACE_GEOMETRY_TYPENAME[j]);
	  if (ncor > 0) {
	    cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	    if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					       MED_DESCENDING_FACE,typfac[j],cor) < 0) {
	      MESSAGE("Erreur a la lecture des correspondances sur les faces : ");
	      SSCRUTE(MED_GET_FACE_GEOMETRY_TYPENAME[j]);
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
	for (j=1;j<=MED_N_EDGE_FIXED_GEO;j++) {
	  if ( MEDequivalenceCorrespondenceSize(fid,maa,equ,MED_NO_DT,MED_NO_IT,
						MED_DESCENDING_EDGE,typare[j],&ncor) < 0) {
	    MESSAGE("Erreur a la lecture du nombre de correspondance sur les aretes : ");
	    SSCRUTE(MED_GET_EDGE_GEOMETRY_TYPENAME[j]);
	    return -1;
	  }
	  printf("Il y a "IFORMAT" correspondances sur les aretes %s \n",ncor,
		 MED_GET_EDGE_GEOMETRY_TYPENAME[j]);
	  if (ncor > 0) {
	    cor = (med_int*) malloc(sizeof(med_int)*ncor*2);
	    if (MEDequivalenceCorrespondenceRd(fid,maa,equ,MED_NO_DT,MED_NO_IT,
					       MED_DESCENDING_EDGE,typare[j],cor) < 0) {
	     MESSAGE("Erreur a la lecture des correspondances sur les faces : ");
	     SSCRUTE(MED_GET_EDGE_GEOMETRY_TYPENAME[j]);
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
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return ret;
}




