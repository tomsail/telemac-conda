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
 * - Nom du fichier : test7.c
 *
 * - Description : lecture des elements du maillage MED crees par test6
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
  med_int *se2_1;
  med_int *se2_2;
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
  med_bool inoele=MED_FALSE,inuele=MED_FALSE,chgt=MED_FALSE,trsf=MED_FALSE;
  med_bool inoele3=MED_FALSE,inuele3=MED_FALSE;
  med_int tse2,ttr3;
  char str[MED_SNAME_SIZE+1];
  med_int flt[2] = { 2, 3 }, fltsize=2;
  char desc[MED_COMMENT_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[2*MED_SNAME_SIZE+1];
  char unicoo[2*MED_SNAME_SIZE+1];
  med_mesh_type type;
  med_sorting_type sort;
  med_int nstep=0,i=0;
  med_filter filter=MED_FILTER_INIT;
  med_axis_type rep;
  med_int nname=0;

  /* Ouverture du fichier en mode lecture seule */
  if ((fid = MEDfileOpen("test6.med",MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test6.med");
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

  /* Combien de triangles et de segments */
  if ((nse2 = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_DESCENDING_EDGE, MED_SEG2,MED_CONNECTIVITY, MED_DESCENDING,
			     &chgt, &trsf)) < 0)  {
    MESSAGE("Erreur a la lecture du nombre de faces MED_SEG2");
    return -1;
  }

  if ((ntr3 = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			     MED_CELL,MED_TRIA3,MED_CONNECTIVITY, MED_DESCENDING,
			     &chgt, &trsf))<0) {
    MESSAGE("Erreur a la lecture du nombre de mailles MED_TRIA3");
    return -1;
  }
  printf("Nombre de MED_SEG2 : "IFORMAT" - nombre de MED_TRIA3 : "IFORMAT"\n",nse2,ntr3);


  /* Allocations memoire */ 
  tse2    = 2;
  se2_1   = (med_int*) calloc(tse2*nse2,sizeof(med_int));
  se2_2   = (med_int*) malloc(sizeof(med_int)*tse2*nse2);
  nomse2  = (char*)    malloc(MED_SNAME_SIZE*nse2+1);
  numse2  = (med_int*) malloc(sizeof(med_int)*nse2);
  nufase2 = (med_int*) malloc(sizeof(med_int)*nse2);

  ttr3    = 3;
  tr3     = (med_int*) malloc(sizeof(med_int)*ntr3*ttr3);
  nomtr3  = (char*)    malloc(MED_SNAME_SIZE*ntr3+1);
  numtr3  = (med_int*) malloc(sizeof(med_int)*ntr3);
  nufatr3 = (med_int*) malloc(sizeof(med_int)*ntr3);

  if ( MEDfilterEntityCr( fid, nse2, 1, sdim, 2,
			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
			  MED_NO_PROFILE, fltsize,
			  flt, &filter) < 0 ) {
    MESSAGE("Erreur à la crétion du filtre 1.");
  }


  /* Lecture des connectivites des segments avec flt */
  if (MEDmeshElementConnectivityAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
					   MED_DESCENDING_EDGE, MED_SEG2, MED_DESCENDING, &filter,
					   se2_1) < 0) {
    MESSAGE("Erreur a la lecture de la connectivite des segments");
    return -1;
  }

  MEDfilterClose(&filter);

  /* Lecture de la connectivite des segments */
  if (MEDmeshElementConnectivityRd(fid, maa, MED_NO_DT, MED_NO_IT,
				   MED_DESCENDING_EDGE, MED_SEG2, MED_DESCENDING,
				   MED_FULL_INTERLACE, se2_2) < 0) {
    MESSAGE("Erreur a la lecture de la connectivite des segments");
    return -1;
  }

  /* Lecture (optionnelle) des noms des segments */
  if (MEDmeshEntityNameRd(fid, maa, MED_NO_DT, MED_NO_IT,
			  MED_DESCENDING_EDGE, MED_SEG2,nomse2) < 0)
    inoele = MED_FALSE;
  else
    inoele = MED_TRUE;

  /* Test complémentaire */
  if ((nname = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_DESCENDING_EDGE,MED_SEG2,MED_NAME, MED_NO_CMODE,
			      &chgt, &trsf))<0) {
    MESSAGE("Erreur a la lecture du nombre de nom de mailles MED_SEG2");
    return -1;
  }
  printf("Nombre de nom de MED_SEG2 : "IFORMAT" \n",nname);

  /* Lecture (optionnelle) des numeros des segments */
  if ( MEDmeshEntityNumberRd(fid, maa, MED_NO_DT, MED_NO_IT,
			    MED_DESCENDING_EDGE, MED_SEG2, numse2) < 0)
    inuele = MED_FALSE;
  else
    inuele = MED_TRUE;

  /* Lecture des numeros des familles des segments */
  if (MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,
				  MED_DESCENDING_EDGE, MED_SEG2,nufase2) < 0) {
    MESSAGE("Erreur a la lecture des numéros de famille des segments");
    return -1;
  }

  /* Lecture de la connectivite des triangles */
  if (MEDmeshElementConnectivityRd(fid, maa, MED_NO_DT, MED_NO_IT,
				   MED_CELL, MED_TRIA3, MED_DESCENDING,
				   MED_FULL_INTERLACE, tr3) < 0) {
    MESSAGE("Erreur a la lecture de la connectivite des triangles");
    return -1;
  }

  /* Lecture (optionnelle) des noms des triangles */
  if (MEDmeshEntityNameRd(fid, maa, MED_NO_DT, MED_NO_IT,
			  MED_CELL, MED_TRIA3, nomtr3) < 0)
    inoele3 = MED_FALSE;
  else
    inoele3 = MED_TRUE;

  /* Lecture (optionnelle) des numeros des triangles */
  if (MEDmeshEntityNumberRd(fid, maa, MED_NO_DT, MED_NO_IT,
			    MED_CELL, MED_TRIA3, numtr3) < 0)
    inuele3 = MED_FALSE;
  else
    inuele3 = MED_TRUE;

  /* Lecture des numeros des familles des triangles */
  if ( (ret = MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,
					  MED_CELL, MED_TRIA3,nufatr3)) < 0) {
    MESSAGE("Erreur a la lecture des numeros de famille des segments");
    return -1;
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  /* Affichage */
  if (ret == 0) {
    printf("Connectivite des segments (1): \n");
    for (i=0;i<nse2*tse2;i++)
      printf(IFORMAT" ",*(se2_1+i));
    printf("\n");
    printf("Connectivite des segments (2): \n");
    for (i=0;i<nse2*tse2;i++)
      printf(IFORMAT" ",*(se2_2+i));
    if (inoele) {
      printf("\nNoms des segments :\n");
      for (i=0;i<nse2;i++) {
	strncpy(str,nomse2+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	str[MED_SNAME_SIZE] = '\0';
	printf("|%s| ",str);
      }
    }
    if (inuele) {
      printf("\nNumeros des segments :\n");
      for (i=0;i<nse2;i++)
	printf(IFORMAT" ",*(numse2+i));
    }
    printf("\nNumeros des familles des segments :\n");
    for (i=0;i<nse2;i++)
      printf(IFORMAT" ",*(nufase2+i));

    printf("\nConnectivite des triangles : \n");
    for (i=0;i<ntr3*ttr3;i++)
      printf(IFORMAT" ",*(tr3+i));
    if (inoele3) {
      printf("\nNoms des triangles :\n");
      for (i=0;i<ntr3;i++) {
	strncpy(str,nomtr3+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	str[MED_SNAME_SIZE] = '\0';
	printf("|%s| ",str);
      }
    }
    if (inuele3) {
      printf("\nNumeros des triangles :\n");
      for (i=0;i<ntr3;i++)
	printf(IFORMAT" ",*(numtr3+i));
    }
    printf("\nNumeros des familles des triangles :\n");
    for (i=0;i<ntr3;i++)
      printf(IFORMAT" ",*(nufatr3+i));

    printf("\n");
  }

  /* Nettoyage memoire */
  free(se2_1);
  free(se2_2);
  free(nomse2);
  free(numse2);
  free(nufase2);

  free(tr3);
  free(nomtr3);
  free(numtr3);
  free(nufatr3);

  return ret;
}

