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
 * - Nom du fichier : test28.c
 *
 * - Description : lecture d'un maillage structure (grille cartesienne |
 *                 grille curvilinéaire) dans le fichier test27.med
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include "med_utils.h"
#include <string.h>

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_ACC_RDWR
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_ACC_RDEXT
#else
#define MODE_ACCES MED_ACC_CREAT
#endif


#define MAXDIM 3

int main (int argc, char **argv)


{
  med_err    ret = 0;
  med_idt    fid = 0;
  med_int    mdim= 0, sdim=0, axe=0, j=0;
  med_float *indices=NULL;
  med_int    nind=0,nmaa=0, nnoeuds=0;
  med_bool   chgt=MED_FALSE,trsf=MED_FALSE;
  char maa     [MED_NAME_SIZE+1]        ="";
  char axisname[MAXDIM*MED_SNAME_SIZE+1]="";
  char axisunit[MAXDIM*MED_SNAME_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]="";
  char desc[MED_COMMENT_SIZE+1]="";
  med_mesh_type    meshtype;
  med_grid_type    gridtype;
  med_data_type    quoi;
  med_sorting_type sort;
  med_axis_type    repere;
  med_float *coo;
  med_int   *structure_grille;
  med_int nstep=0,i=0;

  /* Ouverture du fichier test17.med en lecture seule */
  fid = MEDfileOpen("test27.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test27.med");
    return -1;
  }

  /* Lecture du nombre de maillage */
  nmaa = MEDnMesh(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a la lecture du nombre de maillage");
    return -1;
  }

  /* On boucle sur les maillages et on ne lit que la grille cartesienne
     et la grille destructuree */
  for (i=0;i<nmaa;i++) {

    if ((sdim=MEDmeshnAxis(fid, i+1)) <0) {
      MESSAGE("Erreur a la lecture de la dimension de l'espace du maillage :");
      SSCRUTE(maa);
      return -1;
    }

    /* On repere le maillage qui nous interesse */
    if ( MEDmeshInfo( fid, i+1,  maa, &sdim, &mdim, &meshtype, desc, dtunit, &sort,
		      &nstep,  &repere, axisname,axisunit) < 0 ) {
      MESSAGE("Erreur a la lecture des infos sur le maillage");
      return -1;
    }

    printf("\nMaillage de nom : |%s| , de dimension : "IFORMAT" , et de type %d\n",maa,mdim,meshtype);
    printf("\t -Dimension de l'espace : "IFORMAT"\n",sdim);
    printf("\t -Description du maillage : |%s|\n",desc);
    printf("\t -Noms des axes : |%s|\n",axisname);
    printf("\t -Unités des axes : |%s|\n",axisunit);
    printf("\t -Type de repère : %d\n",repere);
    printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n",dtunit);
    if (meshtype == MED_STRUCTURED_MESH)
      printf("\t - Type : Maillage structure \n");
    else
      printf("\t - Type : Maillage non structure \n");

    /* On regarde le type de la grille */
    if (meshtype == MED_STRUCTURED_MESH) {
      if (MEDmeshGridTypeRd(fid,maa, &gridtype) < 0) {
	MESSAGE("Erreur a la lecture de la nature d'une grille");
	return -1;
      }
      if (gridtype == MED_CARTESIAN_GRID)
	printf("\t - Grille cartesienne \n");
      if (gridtype == MED_CURVILINEAR_GRID)
	printf("\t - Grille de-structureee \n");
    }

    /* On regarde les coordonnees de la grille standard */
    if (meshtype == MED_STRUCTURED_MESH && gridtype == MED_CURVILINEAR_GRID) {

      nnoeuds = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			       MED_NODE, MED_NONE, MED_COORDINATE, MED_NO_CMODE, &chgt, &trsf);
      if (nnoeuds < 0) {
	MESSAGE("Erreur a la lecture du nombre de noeuds");
	return -1;
      }
      printf("Nombre de noeuds : "IFORMAT" \n",nnoeuds);

      structure_grille = (med_int *) malloc(sizeof(med_int)*mdim);
      if ( MEDmeshGridStructRd(fid,maa,MED_NO_DT,MED_NO_IT, structure_grille ) < 0) {
	MESSAGE("Erreur a la lecture de la structure de la grille");
	ret = -1;
      }
      if (ret == 0) {
	printf("Structure des noeuds de la grille : [ ");
	for (j=0;j<mdim;j++)
	  printf(" "IFORMAT" ",*(structure_grille+j));
	printf(" ] \n");
	free(structure_grille);
      }

      if (ret == 0) {
	coo = (med_float *) malloc(sizeof(med_float)*nnoeuds*mdim);

	if ( MEDmeshNodeCoordinateRd(fid, maa, MED_NO_DT, MED_NO_IT,
				     MED_FULL_INTERLACE, coo) < 0 ) {
	  MESSAGE("Erreur a la lecture des coordonnees des noeuds");
	  ret = -1;
	}
      }

      if (ret == 0) {
	printf("Coordonnees : [ ");
	for (j=0;j<nnoeuds*mdim;j++)  printf(" %f ",*(coo+j));
	printf(" ] \n");
	free(coo);
      }
    }

    /* On regarde les coordonnees des indices de la grille cartesienne */
    if (meshtype == MED_STRUCTURED_MESH && gridtype == MED_CARTESIAN_GRID)
      for (axe=1;axe<=mdim;axe++) {
	switch(axe) {
	case 1 :
	  quoi = MED_COORDINATE_AXIS1;
	  break;

	case 2 :
	  quoi = MED_COORDINATE_AXIS2;
	  break;

	case 3 :
	  quoi = MED_COORDINATE_AXIS3;
	  break;

	default :
	  return -1;
	}

	nind = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_NODE, MED_NONE, quoi, MED_NO_CMODE, &chgt, &trsf);
	if (nind < 0) {
	  MESSAGE("Erreur a la lecture de la taille de l'indice");
	  return -1;
	}
	printf("Lecture de la taille de l'indice : "IFORMAT" \n",nind);

	/* on lit le tableau des indices */
	indices = (med_float *) malloc(sizeof(med_float)*nind);
	if (MEDmeshGridIndexCoordinateRd(fid,maa,MED_NO_DT,MED_NO_IT, axe,indices) < 0) {
	  MESSAGE("Erreur a lecture de indices de coordonnees");
	  ret = -1;
	}
	if (ret == 0) {
	  printf("Axe %.*s [%.*s] : [ ",MED_SNAME_SIZE, &axisname[(axe-1)*MED_SNAME_SIZE],
		 MED_SNAME_SIZE,&axisunit[(axe-1)*MED_SNAME_SIZE]);
	  for (j=0;j<nind;j++) printf(" %f ",indices[j]);
	  printf(" ] \n");
	}
	free(indices);
      }
  }

  /* On ferme le fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");

  return ret;
}
