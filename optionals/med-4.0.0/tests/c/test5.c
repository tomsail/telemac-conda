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
 * - Nom du fichier : test5.c
 *
 * - Description : lecture des noeuds d'un maillage MED.
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
  med_idt fid=0;
  /* la dimension du maillage */
  med_int mdim=0,sdim=0;
  /* nom du maillage de longueur maxi MED_NAME_SIZE */
  char maa[MED_NAME_SIZE+1]="";
  /* le nombre de noeuds */
  med_int nnoe = 0;
  /* table des coordonnees */
  med_float *coo1=NULL,*coo2=NULL;
  /* tables des noms et des unites des coordonnees
     flt : (dimension*MED_SNAME_SIZE+1) */
  char nomcoo[2*MED_SNAME_SIZE+1];
  char unicoo[2*MED_SNAME_SIZE+1];
  /* tables des noms, numeros, numeros de familles des noeuds
     autant d'elements que de noeuds - les noms ont pour longueur
     MED_SNAME_SIZE */
  char *nomnoe=NULL;
  med_int *numnoe=NULL, *nufano=NULL;
  med_axis_type rep;
  med_bool inonoe=MED_FALSE,inunoe=MED_FALSE,chgt=MED_FALSE,trsf=MED_FALSE;
  char str[MED_SNAME_SIZE+1];
  med_int flt[2] = { 2, 3 };
  char desc[MED_COMMENT_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]="";
  med_mesh_type type;
  med_sorting_type sort;
  med_int nstep=0,i=0;
  med_filter filter=MED_FILTER_INIT;
  med_int isolatednodes=0;
  med_int verticesnodes=0;
  med_int cellmaxnodes=0;

  /* Ouverture du fichier "test4.med" en lecture seule */
  fid = MEDfileOpen("test4.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test4.med");
    return -1;
  }

  if ((sdim=MEDmeshnAxis(fid, 1)) <0) {
    MESSAGE("Erreur a la lecture de la dimension de l'espace du maillage :");
    SSCRUTE(maa);
    ret = -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if ( MEDmeshInfo( fid, 1,  maa,  &sdim, &mdim, &type, desc, dtunit, &sort,
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
    printf("\t -Nombre d'étape de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n",dtunit);
  }
  /* Lecture des attributs des noeuds du maillage  */
  if (MEDmeshAttributeRd( fid, maa, &isolatednodes, &verticesnodes, &cellmaxnodes) < 0 ) {
    MESSAGE("Aucune définition des attributs des noeuds du maillage");
  } else {
    printf("\t -Nombre de noeuds isolés             : "IFORMAT"\n",isolatednodes);
    printf("\t -Nombre de noeuds sommets            : "IFORMAT"\n",verticesnodes);
    printf("\t -Nombre maximum de noeuds par maille : "IFORMAT"\n",cellmaxnodes);
  }


  /* Combien de noeuds a lire ? */
  nnoe = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
			MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
			&chgt,&trsf);
  if (nnoe < 0) {
    MESSAGE("Erreur a la lecture du nombre de noeuds dans : ");
    ret = -1;
  } else
    printf("Nombre de noeuds : "IFORMAT" \n",nnoe);

  /* Allocations memoires */
  if (nnoe > 0) {
    /* table des coordonnees
       flt : (dimension * nombre de noeuds ) */
    coo1 = (med_float*) calloc(nnoe*sdim,sizeof(med_float));
    coo2 = (med_float*) calloc(nnoe*sdim,sizeof(med_float));
    /* table des des numeros, des numeros de familles des noeuds
       flt : (nombre de noeuds) */
    numnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
    nufano = (med_int*) malloc(sizeof(med_int)*nnoe);
    /* table des noms des noeuds
       flt : (nnoe*MED_SNAME_SIZE+1) */
    nomnoe = (char*) malloc(MED_SNAME_SIZE*nnoe+1);
  }

  /* med_int filterarray[2]={2,4}; */

  /* if ( MEDfilterEntityCr( fid, nnoe, 1, sdim, MED_ALL_CONSTITUENT, */
  /* 			  MED_NO_INTERLACE, MED_GLOBAL_STMODE, */
  /* 			  MED_NO_PROFILE, 2, */
  /* 			  filterarray, &filter) < 0 ) { */
  /*   MESSAGE("Erreur à la création du filtre 1."); */
  /* } */

  if ( MEDfilterEntityCr( fid, nnoe, 1, sdim, 2,
  			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
  			  MED_NO_PROFILE, MED_UNDEF_SIZE,
  			  NULL, &filter) < 0 ) {
    MESSAGE("Erreur à la création du filtre 1.");
  }

  /* Lecture des composantes n°2 des coordonnees des noeuds */
  if (nnoe > 0) {
    if ( MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
					 &filter, coo1) < 0 ) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo1 : ");
      for (i=0;i<nnoe*sdim;i++)
	printf("%4.2f ",coo1[i]);
      printf("\n");
    }
  }

  MEDfilterClose(&filter);
  if ( MEDfilterEntityCr( fid, nnoe, 1, sdim, 1,
			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
			  MED_NO_PROFILE, MED_UNDEF_SIZE,
			  NULL, &filter) < 0 ) {
    MESSAGE("Erreur à la création du filtre 2.");
  }


  /* Lecture des composantes n°1 des coordonnees des noeuds */
  if (nnoe > 0) {
    if ( MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
					 &filter, coo1) < 0 ) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo1 : ");
      for (i=0;i<nnoe*sdim;i++)
	printf("%4.2f ",coo1[i]);
      printf("\n");
    }
  }

  MEDfilterClose(&filter);
  if ( MEDfilterEntityCr( fid, nnoe, 1, sdim, 1,
			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
			  MED_NO_PROFILE, 2,
			  flt, &filter) < 0 ) {
    MESSAGE("Erreur à la création du filtre 3.");
  }

  /* Lecture des composantes n°1 des coordonnees des noeuds du filtre */
  if (nnoe > 0) {
    if ( MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
					 &filter, coo2) < 0 ) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*sdim;i++)
	printf("%4.2f ",coo2[i]);
      printf("\n");
    }
  }

  MEDfilterClose(&filter);
  if ( MEDfilterEntityCr( fid, nnoe, 1, sdim, 2,
			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
			  MED_NO_PROFILE, 2,
			  flt, &filter) < 0 ) {
    MESSAGE("Erreur à la création du filtre 4.");
  }

  /* Lecture des composantes n°2 des coordonnees des noeuds du filtre */
  if (nnoe > 0) {
    if ( MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
					 &filter, coo2) < 0 ) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*sdim;i++) {
	printf("%4.2f ",coo2[i]);
	coo2[i] = 0.0;
      }
      printf("\n");
    }
  }

  MEDfilterClose(&filter);
  if ( MEDfilterEntityCr( fid, nnoe, 1, sdim, MED_ALL_CONSTITUENT,
			  MED_FULL_INTERLACE, MED_GLOBAL_STMODE,
			  MED_NO_PROFILE, 2,
			  flt, &filter) < 0 ) {
    MESSAGE("Erreur à la création du filtre 5.");
  }

  /* Lecture de toutes les composantes des coordonnees des noeuds du filtre */
  if (nnoe > 0) {
    if ( MEDmeshNodeCoordinateAdvancedRd(fid, maa, MED_NO_DT, MED_NO_IT,
					 &filter, coo2) < 0 ) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*sdim;i++) {
	printf("%4.2f ",coo2[i]);
	coo2[i] = 0.0;
      }
      printf("\n");
    }
  }
  MEDfilterClose(&filter);

  /* Lecture des composantes des coordonnees des noeuds */
  if (nnoe > 0) {
    if ( MEDmeshNodeCoordinateRd(fid, maa, MED_NO_DT, MED_NO_IT,
				 MED_FULL_INTERLACE, coo2) < 0 ) {
      MESSAGE("Erreur a la lecture des coordonnees des noeuds");
      ret = -1;
    } else {
      printf("Valeur de coo2 : ");
      for (i=0;i<nnoe*sdim;i++)
	printf("%4.2f ",coo2[i]);
      printf("\n");
    }
  }

  /* Lecture des noms des noeuds (optionnel dans un maillage MED) */
  if ((nnoe > 0)) {
    if (MEDmeshEntityNameRd(fid,maa, MED_NO_DT, MED_NO_IT,
			    MED_NODE,MED_NONE,nomnoe) < 0)
      inonoe = MED_FALSE;
    else
      inonoe = MED_TRUE;
  }

  /* Lecture des numeros des noeuds (optionnel dans un maillage MED) */
  if ((nnoe > 0)) {
    if (MEDmeshEntityNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,
			      MED_NODE,MED_NONE,numnoe) < 0)
      inunoe = MED_FALSE;
    else
      inunoe = MED_TRUE;
  }

  /* Lecture des numeros de familles des noeuds */
  if ((nnoe > 0))
    if (MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,
				    MED_NODE,MED_NONE,nufano ) < 0) {
      MESSAGE("Erreur a la lecture des numeros de famille des noeuds");
      ret = -1;
    }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0){
    MESSAGE("Erreur a la fermeture du fichier");
    ret = -1;
  }

  /* Affichage des resulats */
  if (ret == 0 && nnoe > 0)
    {
      printf("Type de repere : %d \n",rep);
      printf("Nom des coordonnees : \n");
      for (i=0;i<sdim;i++)
	{
	  strncpy(str,nomcoo+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
          str[MED_SNAME_SIZE] = '\0';
          printf("|%s| ",str);
	}
      printf("\nUnites des coordonnees : \n");
      for (i=0;i<sdim;i++)
	{
	  strncpy(str,unicoo+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
          str[MED_SNAME_SIZE] = '\0';
          printf("|%s| ",str);
	}
      printf("\nCoordonnees des noeuds : \n");
      for (i=0;i<nnoe*sdim;i++)
	printf("%f ",*(coo2+i));
      if (inonoe)
	{
	  printf("\nNoms des noeuds : \n");
	  for (i=0;i<nnoe;i++)
	    {
	      strncpy(str,nomnoe+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
              str[MED_SNAME_SIZE] = '\0';
	      printf("|%s|",str);
	    }
	}
      if (inunoe)
	{
	  printf("\nNumeros des noeuds : \n");
	  for (i=0;i<nnoe;i++)
	      printf(IFORMAT" ",*(numnoe+i));
	}
      printf("\nNumeros des familles des noeuds : \n");
      for (i=0;i<nnoe;i++)
	printf(IFORMAT" ",*(nufano+i));
      printf("\n");
    }

  /* liberation memoire */
  if (nnoe > 0) {
    free(coo1);
    free(coo2);
    free(nomnoe);
    free(numnoe);
    free(nufano);
  }

  return ret;
}




