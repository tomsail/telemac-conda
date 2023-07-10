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
  med_int mdim=0,sdim=0;
  /* nom du maillage de longueur maxi MED_NAME_SIZE */
  char maa[MED_NAME_SIZE+1];
  /* le nombre de noeuds */
  med_int nnoe = 0;
  /* table des coordonnees */
  med_float *coo;
  /* tables des noms et des unites des coordonnees (dimension*MED_SNAME_SIZE+1) */
  char nomcoo[3*MED_SNAME_SIZE+1]="";
  char unicoo[3*MED_SNAME_SIZE+1]="";
  /* tables des noms, numeros, numeros de familles des noeuds
     autant d'elements que de noeuds - les noms ont pour longueur  MED_SNAME_SIZE */
  char       *nomnoe;
  med_int    *numnoe;
  med_int    *nufano;
  med_bool   inonoe,inunoe,inufam;
  med_bool   chgt=MED_FALSE, trsf=MED_FALSE;
  char str   [MED_SNAME_SIZE+1]  ="";
  char desc  [MED_COMMENT_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]  ="";
  med_mesh_type    type;
  med_sorting_type sort;
  med_axis_type    rep;
  med_int          nstep=0;
  med_int i;

  /* Ouverture du fichier en mode lecture seule */
  if ((fid = MEDfileOpen(argv[1],MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier.");
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
  if ((nnoe = MEDmeshnEntity(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_NODE, MED_NODE,MED_COORDINATE, MED_NO_CMODE,
			     &chgt, &trsf)) < 0)  {
    MESSAGE("Erreur a la lecture du nombre de noeuds");
    return -1;
  }
  printf("Nombre de noeuds : "IFORMAT" \n",nnoe);

  /* Allocations memoires */
  /* table des coordonnees
     profil : (dimension * nombre de noeuds ) */
  if (nnoe > 0) {
    coo = (med_float*) malloc(sizeof(med_float)*nnoe*mdim);
    /* table des des numeros, des numeros de familles des noeuds
       profil : (nombre de noeuds) */
    numnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
    nufano = (med_int*) calloc(nnoe,sizeof(med_int));
    /* table des noms des noeuds
       profil : (nnoe*MED_SNAME_SIZE+1) */
    nomnoe = (char*) malloc(MED_SNAME_SIZE*nnoe+1);

    /* Lecture des noeuds :
       - Coordonnees
       - Noms (optionnel dans un fichier MED)
       - Numeros (optionnel dans un fichier MED)
       - Numeros de familles	*/
    if (MEDmeshNodeRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_FULL_INTERLACE,
		      coo,&inonoe,nomnoe,&inunoe,numnoe,&inufam,nufano) < 0) {
      MESSAGE("Erreur a la lecture des noeuds du maillage");
      ret = -1;
    }

    /* Affichage */
    if (ret == 0) {
      printf("Type de repere : %d \n",rep);
      printf("Nom des coordonnees : \n");
      for (i=0;i<mdim;i++) {
	strncpy(str,nomcoo+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	str[MED_SNAME_SIZE] = '\0';
	printf("|%s| ",str);
      }
      printf("\nUnites des coordonnees : \n");
      for (i=0;i<mdim;i++) {
	strncpy(str,unicoo+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	str[MED_SNAME_SIZE] = '\0';
	printf("|%s| ",str);
      }
      printf("\nCoordonnees des noeuds : \n");
      for (i=0;i<nnoe*mdim;i++)
	printf("%f ",*(coo+i));
      if (inonoe) {
	printf("\nNoms des noeuds : \n");
	for (i=0;i<nnoe;i++) {
	  strncpy(str,nomnoe+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  printf(" |%s| ",str);
	}
      }
      if (inunoe) {
	printf("\nNumeros des noeuds : \n");
	for (i=0;i<nnoe;i++)
	  printf(""IFORMAT" ",*(numnoe+i));
      }

      printf("\nPrésence de numeros des familles des noeuds : %d\n",inufam);
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
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return ret;
}




