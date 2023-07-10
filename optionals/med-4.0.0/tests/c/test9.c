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
 * - Nom du fichier : test9.c
 *
 * - Description : lecture des familles d'un maillage MED 
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
  char maa[MED_NAME_SIZE+1];
  med_int mdim;
  med_int nfam;
  int     i,j;
  med_int natt,ngro;
  char *attdes,*gro;
  med_int *attval,*attide;
  char nomfam[MED_NAME_SIZE+1];
  med_int numfam=0, nstep=0, sdim=0;
  char str1[MED_COMMENT_SIZE+1];
  char str2[MED_LNAME_SIZE+1];
  char desc[MED_COMMENT_SIZE+1];
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[2*MED_SNAME_SIZE+1]="";
  char unicoo[2*MED_SNAME_SIZE+1]="";
  med_mesh_type type;
  med_sorting_type sort;
  med_axis_type rep;

  /* Ouverture du fichier "test8.med" en lecture seule */
  if ((fid = MEDfileOpen("test8.med",MED_ACC_RDONLY)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test8.med");
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
    printf("\t -Nombre d'étape de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : |%s|\n",dtunit);
  }


  /* Lecture du nombre de familles */
  if ((nfam = MEDnFamily(fid,maa)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de famille");
    return -1;
  }
  printf("Nombre de familles : "IFORMAT" \n",nfam);

  /* Lecture de chaque famille */
  for (i=0;i<nfam;i++) {

    /* Lecture du nombre de groupe */
    if ((ngro = MEDnFamilyGroup(fid,maa,i+1)) < 0) {
      MESSAGE("Erreur a la lecture du nombre de groupe de la famille d'indice : ");
      ISCRUTE_int(i+1);
      ret = -1;
    }

    /* Lecture du nombre d'attribut */
    if ((natt = MEDnFamily23Attribute(fid,maa,i+1)) < 0) {
      MESSAGE("Erreur a la lecture du nombre d'attribut de la famille d'indice : ");
      ISCRUTE_int(i+1);
      ret = -1;
    }

    if (ret == 0)
      printf("Famille %d a "IFORMAT" attributs et "IFORMAT" groupes \n",i+1,natt,ngro);

    /* Lecture des informations sur la famille */
    if (ret == 0) {
      /* Allocations memoire */
      attide = (med_int*) malloc(sizeof(med_int)*natt);
      attval = (med_int*) malloc(sizeof(med_int)*natt);
      attdes = (char *) malloc(MED_COMMENT_SIZE*natt+1);
      gro = (char*) malloc(MED_LNAME_SIZE*ngro+1);

      if (MEDfamily23Info(fid,maa,i+1,nomfam,attide,attval,attdes,&numfam,gro) < 0) {
	MESSAGE("Erreur a la lecture des informations de la famille d'indice : ");
	ISCRUTE_int(i+1);
	ret = -1;
      }

      if (ret == 0) {
	printf("Famille de nom %s et de numero "IFORMAT" : \n",nomfam,numfam);
	printf("Attributs : \n");
	for (j=0;j<natt;j++) {
	  strncpy(str1,attdes+j*MED_COMMENT_SIZE,MED_COMMENT_SIZE);
	  str1[MED_COMMENT_SIZE] = '\0';
	  printf("ide = "IFORMAT" - val = "IFORMAT" - des = %s\n",*(attide+j),
		 *(attval+j),str1);
	}
	free(attide);
	free(attval);
	free(attdes);

	for (j=0;j<ngro;j++) {
	  strncpy(str2,gro+j*MED_LNAME_SIZE,MED_LNAME_SIZE);
	  str2[MED_LNAME_SIZE] = '\0';
	  printf("gro = %s\n",str2);
	      }
	free(gro);
      }
    }
  }

  /* Fermeture du fichier */
  if (MEDfileClose(fid)  < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return ret;
}
