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
 * - Nom du fichier : test24.c
 *
 * - Description : lecture de mailles/faces de type MED_POLYGONE
 *                 dans le maillage MED du fichier test23.med 
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

#define MAXDIM 3

{
  med_err ret = 0;
  med_idt fid;
  char maa[MED_NAME_SIZE+1];
  med_int nmaa,i,mdim,nindex,npoly,j,nind,nnoe;
  char desc[MED_COMMENT_SIZE+1];
  med_int *con, *index, *num, *fam;
  char *nom;
  char tmp[MED_SNAME_SIZE+1];
  int ind1, ind2,k;
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[MAXDIM*MED_SNAME_SIZE+1];
  char unicoo[MAXDIM*MED_SNAME_SIZE+1];
  med_mesh_type type;
  med_sorting_type sort;
  med_axis_type rep;
  med_int nstep=0,spacedim=0;
  med_bool inoele=MED_FALSE,inuele=MED_FALSE,chgt=MED_FALSE,trsf=MED_FALSE;

  /* Ouverture du fichier test23.med en lecture seule */
  fid = MEDfileOpen("test23.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test23.med");
    return -1;
  }
  printf("Ouverture du fichier test23.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMesh(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a la lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages = "IFORMAT"\n",nmaa);

  for (i=0;i<nmaa;i++) {

    /* Infos sur le maillage */
    if ( MEDmeshInfo( fid, i+1,  maa, &spacedim, &mdim, &type, desc, dtunit, &sort,
		      &nstep,  &rep, nomcoo,unicoo) < 0 ) {
      MESSAGE("Erreur a la lecture des infos sur le maillage");
      return -1;
    } else {
      printf("maillage "IFORMAT" de nom [%s] et de dimension : "IFORMAT" , et de type %d\n",i+1,maa,mdim,type);
      printf("\t -Dimension de l'espace : "IFORMAT"\n",spacedim);
      printf("\t -Description du maillage : |%s|\n",desc);
      printf("\t -Noms des axes : |%s|\n",nomcoo);
      printf("\t -Unités des axes : |%s|\n",unicoo);
      printf("\t -Type de repère : %d\n",rep);
      printf("\t -Nombre d'étapes de calcul : "IFORMAT"\n",nstep);
      printf("\t -Unité des dates : |%s|\n",dtunit);
    }

    /* Combien de mailles polygones en mode nodal */
    if ((nind = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
			       MED_CELL,MED_POLYGON,MED_INDEX_NODE,MED_NODAL,
			       &chgt,&trsf)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de mailles MED_POLYGONE");
      return -1;
    }
    npoly = nind-1;
    printf("Nombre de mailles polygones en mode nodal : "IFORMAT" \n",npoly);

    /* Quelle taille pour le tableau des connectivites, nombre de noeuds
       tous polygones confondus*/
    if ((nnoe = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
			       MED_CELL,MED_POLYGON,MED_CONNECTIVITY,MED_NODAL,
			       &chgt,&trsf)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de mailles MED_POLYGONE");
      return -1;
    }

    printf("Taille a allouer pour la connectivite des polygones : "IFORMAT" \n",nnoe);

    /* Allocation memoire :
     *  - tableau d'index : npoly + 1
     *  - tableau des connectivites : nnoe
     *  - tableaux numeros et numeros de familles : npoly
     *  - tableau des noms : MED_SNAME_SIZE*npoly + 1
	 */
    index = (med_int *) malloc(sizeof(med_int)*nind);
    con = (med_int *)   malloc(sizeof(med_int)*nnoe);
    num = (med_int *)   malloc(sizeof(med_int)*npoly);
    fam = (med_int *)   malloc(sizeof(med_int)*npoly);
    nom = (char *)      malloc(sizeof(char)*MED_SNAME_SIZE*npoly+1);

    /* Lecture de la connectivite des mailles polygones */
    if (MEDmeshPolygonRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_CELL,MED_NODAL,
			 index,con) < 0) {
      MESSAGE("Erreur a la lecture de la connectivite des mailles MED_POLYGONE");
      return -1;
    }
    printf("Lecture de la connectivite des mailles MED_POLYGONE en mode nodal \n");

    /* Lecture (optionnelle) des noms des polygones */
    if (MEDmeshEntityNameRd(fid, maa, MED_NO_DT, MED_NO_IT,
			      MED_CELL, MED_POLYGON,nom) < 0)
      inoele = MED_FALSE;
    else
      inoele = MED_TRUE;

    /* Lecture (optionnelle) des numeros des polygones  */
    if ( MEDmeshEntityNumberRd(fid, maa, MED_NO_DT, MED_NO_IT,
			       MED_CELL, MED_POLYGON, num) < 0)
      inuele = MED_FALSE;
    else
      inuele = MED_TRUE;

    /* Lecture des numeros des familles des segments */
    if (MEDmeshEntityFamilyNumberRd(fid,maa, MED_NO_DT, MED_NO_IT,
				    MED_CELL, MED_POLYGON,fam) < 0) {
      MESSAGE("Erreur a la lecture des numéros de famille des segments");
      /*TODO : Considérer famille 0 */
      return -1;
    }


    if (ret == 0) {
      printf("Affichage des resultats \n");
      for (j=0;j<npoly;j++) {
	printf(">> Maille MED_POLYGONE "IFORMAT" : \n",j+1);
	printf("---- Connectivite       ----- : [ ");
	ind1 = *(index+j)-1;
	ind2 = *(index+j+1)-1;
	for (k=ind1;k<ind2;k++)
	  printf(IFORMAT" ",*(con+k));
	printf(" ] \n");
	strncpy(tmp,nom+j*MED_SNAME_SIZE,MED_SNAME_SIZE);
	tmp[MED_SNAME_SIZE] = '\0';
	if (inoele) printf("---- Nom                ----- : |%s| \n",tmp);
	if (inuele) printf("---- Numero             ----- : "IFORMAT" \n",*(num+j));
	printf("---- Numero de famille  ----- : "IFORMAT" \n",*(fam+j));
      }
    }

    /* Liberation de la memoire */
    free(index);
    free(con);
    free(num);
    free(fam);
    free(nom);
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  printf("Fermeture du fichier \n");

  return ret;
}
