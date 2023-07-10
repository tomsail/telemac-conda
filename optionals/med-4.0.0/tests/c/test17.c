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
 * - Nom du fichier : test17.c
 *
 * - Description : lecture d'elements de maillages MED ecrits par test16
 *                 via les routines de niveau 2
 *                 - equivalent a test7.c
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


int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid;
  med_int nse2;
  med_int *se2;
  char *nomse2;
  med_int *numse2;
  med_int *nufase2; 
  med_int ntr3;
  med_int *tr3;
  char *nomtr3;
  med_int *numtr3;
  med_int *nufatr3;
  char maa[MED_NAME_SIZE+1] ="maa1";
  med_int mdim=0,sdim=0;
  med_bool inoele1,inoele2,inuele1,inuele2,infele1,infele2;
  med_int tse2,ttr3;
  med_bool   chgt=MED_FALSE, trsf=MED_FALSE;
  char str   [MED_SNAME_SIZE+1]  ="";
  char desc  [MED_COMMENT_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]  ="";
  char nomcoo[3*MED_SNAME_SIZE+1]="";
  char unicoo[3*MED_SNAME_SIZE+1]="";
  med_mesh_type    type;
  med_sorting_type sort;
  med_axis_type    rep;
  med_int          nstep=0;
  med_int i;


  /* Ouverture du fichier en mode lecture seule */
  if ((fid = MEDfileOpen("test16.med",MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test16.med .");
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
    printf("\t -Description du maillage : |%s|\n",desc);
    printf("\t -Noms des axes : |%s|\n",nomcoo);
    printf("\t -Unités des axes : |%s|\n",unicoo);
    printf("\t -Type de repère : %d\n",rep);
    printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n",dtunit);
  }

  /* Combien de triangles et de segments */
  if ((nse2 = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_DESCENDING_EDGE, MED_SEG2,MED_CONNECTIVITY, MED_DESCENDING,
			     &chgt, &trsf)) < 0)  {
    MESSAGE("Erreur a la lecture du nombre de segments");
    return -1;
  }

  if ((ntr3 = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_CELL, MED_TRIA3,MED_CONNECTIVITY, MED_DESCENDING,
			     &chgt, &trsf)) < 0)  {
    MESSAGE("Erreur a la lecture du nombre de segments");
    return -1;
  }
  printf("Nombre de MED_SEG2 : "IFORMAT" - nombre de MED_TRIA3 :"IFORMAT"\n",nse2,ntr3);

  /* Allocations memoire */
  tse2 = 2;
  se2  = (med_int*) malloc(sizeof(med_int)*tse2*nse2);
  nomse2 = (char*) malloc(MED_SNAME_SIZE*nse2+1);
  numse2 = (med_int*) malloc(sizeof(med_int)*nse2);
  nufase2 = (med_int*) calloc(nse2,sizeof(med_int));

  ttr3 = 3;
  tr3 = (med_int*) malloc(sizeof(med_int)*ntr3*ttr3);
  nomtr3 = (char*) malloc(MED_SNAME_SIZE*ntr3+1);
  numtr3 = (med_int*) malloc(sizeof(med_int)*ntr3);
  nufatr3 = (med_int*) calloc(ntr3,sizeof(med_int));

  /* Lecture des aretes segments MED_SEG2 :
     - Connectivite,
     - Noms (optionnel)
     - Numeros (optionnel)
     - Numeros de familles */
  if (MEDmeshElementRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_DESCENDING_EDGE,MED_SEG2,MED_DESCENDING,MED_NO_INTERLACE,
		       se2,&inoele1,nomse2,&inuele1,numse2,&infele1,nufase2) < 0) {
    MESSAGE("Erreur a la lecture des segments");
    ret = -1;
  }

  /* Lecture des mailles triangles MED_TRIA3 :
     - Connectivite,
     - Noms (optionnel)
     - Numeros (optionnel)
     - Numeros de familles */
  if (MEDmeshElementRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_TRIA3,MED_DESCENDING,MED_NO_INTERLACE,
		       tr3,&inoele2,nomtr3,&inuele2,numtr3,&infele2,nufatr3) < 0) {
    MESSAGE("Erreur a la lecture des triangles");
    ret = -1;
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    ret = -1;
  }

  /* Affichage */
  if (ret == 0) {
    if (nse2 > 0) {
      printf("Connectivite des segments : \n");
      for (i=0;i<nse2*tse2;i++)
	printf(IFORMAT" ",*(se2+i));
      if (inoele1) {
	printf("\nNoms des segments :\n");
	for (i=0;i<nse2;i++) {
	  strncpy(str,nomse2+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  printf("|%s| ",str);
	}
      }
      if (inuele1) {
	printf("\nNumeros des segments :\n");
	for (i=0;i<nse2;i++)
	  printf(IFORMAT" ",*(numse2+i));
      }
      printf("\nPrésence de numeros des familles des segments : %d\n",infele1);
      printf("\nNumeros des familles des segments :\n");
      for (i=0;i<nse2;i++)
	printf(IFORMAT" ",*(nufase2+i));
    }

    if (ntr3 > 0) {
      printf("\nConnectivite des triangles : \n");
      for (i=0;i<ntr3*ttr3;i++)
	printf(IFORMAT" ",*(tr3+i));
      if (inoele2) {
	printf("\nNoms des triangles :\n");
	for (i=0;i<ntr3;i++) {
	  strncpy(str,nomtr3+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  printf("|%s| ",str);
	}
      }
      if (inuele2) {
	printf("\nNumeros des triangles :\n");
	for (i=0;i<ntr3;i++)
	  printf(IFORMAT" ",*(numtr3+i));
      }
      printf("\nPrésence de numeros des familles des triangles : %d\n",infele2);
      printf("\nNumeros des familles des triangles :\n");
      for (i=0;i<ntr3;i++)
	printf(IFORMAT" ",*(nufatr3+i));

      printf("\n");
    }
  }

  /* Nettoyage memoire */
  free(se2);
  free(nomse2);
  free(numse2);
  free(nufase2);

  free(tr3);
  free(nomtr3);
  free(numtr3);
  free(nufatr3);

  return ret;
}




