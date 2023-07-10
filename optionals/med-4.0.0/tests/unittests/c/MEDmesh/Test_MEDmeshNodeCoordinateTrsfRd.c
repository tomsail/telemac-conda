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
 * - Nom du fichier : Test_MEDmeshNodeCoordinateTrsfRd.c
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
  med_int mdim=0,sdim=0;
  char    maa[MED_NAME_SIZE+1]="";
  med_int nnoe = 0;
  med_int ntrsf = 0;
  med_float *coo1;
  med_float mtrsf[7]={ 0, 0, 0, 0, 0, 0, 0};
  /* tables des noms et des unites des coordonnees
     flt : (dimension*MED_SNAME_SIZE+1) */
  char nomcoo[2*MED_SNAME_SIZE+1];
  char unicoo[2*MED_SNAME_SIZE+1];
  /* tables des noms, numeros, numeros de familles des noeuds
     autant d'elements que de noeuds - les noms ont pout longueur
     MED_SNAME_SIZE */
  char *nomnoe;
  med_int *numnoe, *nufano;
  med_axis_type rep;
  med_int  nnomnoe=0,nnumnoe=0,nnufano=0;
  med_bool inonoe=MED_FALSE,inunoe=MED_FALSE,inufano=MED_FALSE;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;
  med_bool chgtco=MED_FALSE,trsfdataset=MED_FALSE;
  char desc[MED_COMMENT_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]="";
  char str[MED_SNAME_SIZE+1]="";
  med_mesh_type type;
  med_sorting_type sort;
  med_int nstep=0,i=0;
  med_int numit=MED_NO_IT,numdt=MED_NO_DT;
  med_float dt=0.0;
  int csit=0;

  /* Ouverture du fichier en lecture seule */
  fid = MEDfileOpen("Test_MEDmeshNodeCoordinateTrsfWr.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier Test_MEDmeshNodeCoordinateTrsfWr.med");
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
    printf("Maillage de nom : |%s| , de dimension : %d , et de type %d\n",maa,mdim,type);
    printf("\t -Dimension de l'espace : %d\n",sdim);
    printf("\t -Description du maillage : %s\n",desc);
    printf("\t -Noms des axes : %s\n",nomcoo);
    printf("\t -Unités des axes : %s\n",unicoo);
    printf("\t -Type de repère : %d\n",rep);
    printf("\t -Nombre d'étape de calcul : %d\n",nstep);
    printf("\t -Unité des dates : %s\n",dtunit);
  }

  for (csit=0; csit <nstep; ++csit) {

    if ( MEDmeshComputationStepInfo(fid, maa, csit+1, &numdt, &numit, &dt ) < 0) {
      MESSAGE("Erreur a la lecture des informations sur le maillage : ");SSCRUTE(maa);
      ret = -1;
      continue;
    }
    if (sort == MED_SORT_DTIT )
      printf("\n -Etape de calcul n° %d : (%d,%d) avec dt=%f %s\n\n",csit+1,numdt,numit,dt,dtunit);
    else
      printf("\n -Etape de calcul n° %d : (%d,%d) avec dt=%f %s\n\n",csit+1,numit,numdt,dt,dtunit);

    /* Combien de noeuds a lire ? */
    nnoe = MEDmeshnEntity(fid,maa,numdt,numit,
			  MED_NODE,MED_NONE,MED_COORDINATE,MED_NODAL, &chgt,&trsf);
    if (nnoe < 0) {
      MESSAGE("Erreur a la lecture du nombre de noeuds.");
      ret = -1;
    } else
      printf("Nombre de noeuds : "IFORMAT", (chgt,trsf) : (%1d,%1d) \n",nnoe,chgt,trsf);

    if ( !chgt )
      printf("Aucun changement par rapport à la séquence de calcul précédente.\n");

    if ( chgt && trsf ) {
      printf("Seules les coordonnées sont modifiées par rapport à la séquence de calcul précédente.\n");
      chgtco=MED_TRUE;
    } else {
      chgtco=MED_FALSE;
    }
    trsfdataset=MED_FALSE;
    if ( chgt && !trsf ) {

      printf("Des modifications ont eu lieu depuis la séquence de calcul précédente.\n");
      ntrsf = MEDmeshnEntity(fid,maa,numdt,numit,
			     MED_NODE,MED_NONE,MED_COORDINATE_TRSF,MED_NODAL, &chgtco, &trsfdataset);
      if (ntrsf < 0)  {
	MESSAGE("Erreur a la lecture de la présence d'une transformation géométrique : ");
	ret=-1;
      }
      if (chgtco)
	printf("Les coordonnées sont modifiées par rapport à la séquence de calcul précédente.\n");
      else
	printf("Les coordonnées ne sont pas modifiées par rapport à la séquence de calcul précédente.\n");

      if (trsfdataset)
	printf("Une modification de la matrice de transformation a eu lieu par rapport à la séquence de calcul précédente.\n");
    }

    /* Lecture des coordonnees des noeuds */
    if (nnoe > 0) {
      if (chgtco) {
	coo1 = (med_float*) calloc(nnoe*sdim,sizeof(med_float));
	if ( MEDmeshNodeCoordinateRd(fid, maa, numdt, numit,MED_FULL_INTERLACE, coo1) < 0 ) {
	  MESSAGE("Erreur a la lecture des coordonnees des noeuds");
	  ret = -1;
	}
      }

    /* Lecture des noms des noeuds (optionnel dans un maillage MED) */
      nnomnoe = MEDmeshnEntity(fid,maa,numdt,numit,
			       MED_NODE,MED_NONE,MED_NAME,MED_NODAL, &chgtco, &inonoe);
      if ( (nnomnoe>0) && inonoe) {
	nomnoe = (char*) malloc(MED_SNAME_SIZE*nnomnoe+1);
	if (MEDmeshEntityNameRd(fid,maa, numdt, numit, MED_NODE,MED_NONE,nomnoe) < 0) {
	  MESSAGE("Erreur a la lecture des noms des noeuds");
	  ret = -1;
	}
      }

    /* Lecture des numeros des noeuds (optionnel dans un maillage MED) */
      nnumnoe = MEDmeshnEntity(fid,maa,numdt,numit,
			       MED_NODE,MED_NONE,MED_NUMBER,MED_NODAL, &chgtco, &inunoe);
      if ( (nnumnoe>0) && inunoe) {
	numnoe = (med_int*) malloc(sizeof(med_int)*nnumnoe);
	if (MEDmeshEntityNumberRd(fid,maa, numdt, numit, MED_NODE,MED_NONE,numnoe) < 0) {
	  MESSAGE("Erreur a la lecture des numéros des noeuds");
	  ret = -1;
	}
      }

    /* Lecture des numeros de familles des noeuds */
      nnufano = MEDmeshnEntity(fid,maa,numdt,numit,
			       MED_NODE,MED_NONE,MED_FAMILY_NUMBER,MED_NODAL, &chgtco, &inufano);
      if ( (nnufano>0) && inufano) {
	nufano = (med_int*) malloc(sizeof(med_int)*nnufano);
	if (MEDmeshEntityFamilyNumberRd(fid,maa, numdt, numit,MED_NODE,MED_NONE,nufano ) < 0) {
	  MESSAGE("Erreur a la lecture des numeros de famille des noeuds");
	  ret = -1;
	}
      }
    }

    /* Affichage des resulats */
    if (ret == 0 && nnoe > 0) {

      if (chgtco) {
	printf("\n\tCoordonnees des noeuds : \n\t");
	for (i=0;i<nnoe*sdim;i++)
	  printf("%f ",*(coo1+i));
      }

      if (trsfdataset)
	if ( MEDmeshNodeCoordinateTrsfRd(fid, maa, numdt, numit, mtrsf) < 0 ) {
	  MESSAGE("Erreur a la lecture de la matrice de transformation");
	  ret = -1;
	} else {
	  printf("Valeur de la matrice de transformation : ");
	  for (i=0;i<7;i++)
	    printf("%4.2f ",mtrsf[i]);
	  printf("\n\n");
	}

      if (inonoe) {
	printf("\n\tNoms des noeuds : \n\t");
	for (i=0;i<nnoe;i++) {
	  strncpy(str,nomnoe+i*MED_SNAME_SIZE,MED_SNAME_SIZE);
	  str[MED_SNAME_SIZE] = '\0';
	  printf("|%s|",str);
	}
      }
      if (inunoe) {
	printf("\n\tNumeros des noeuds : \n\t");
	for (i=0;i<nnoe;i++)
	  printf(IFORMAT" ",*(numnoe+i));
      }
      if (inufano) {
	printf("\n\tNumeros des familles des noeuds : \n\t");
	for (i=0;i<nnoe;i++)
	  printf(IFORMAT" ",*(nufano+i));
	printf("\n");
      }
    }

    /* liberation memoire */
    if (nnoe > 0) {
      if (chgtco)  free(coo1);
      if (inonoe)  free(nomnoe);
      if (inunoe)  free(numnoe);
      if (inufano) free(nufano);
    }
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0){
    MESSAGE("Erreur a la fermeture du fichier");
    ret = -1;
  }

  return ret;
}




