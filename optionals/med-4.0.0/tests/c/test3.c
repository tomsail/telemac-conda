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
 * - Nom du fichier : test3.c
 *
 * - Description : lecture des informations sur les maillages d'un fichier MED.
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


#define OBJEXIST(oname)\
 if ( MEDfileObjectExist(fid,MED_MESH, #oname ,& oname##exist) < 0) {	\
    MESSAGE("Erreur de test de présence du maillage "#oname);	\
    return -1;							\
  }								\
  if ( oname##exist) { MESSAGE("Le maillage "#oname" existe.");  } else \
                     { MESSAGE("Le maillage "#oname" n'existe pas.");}


int main (int argc, char **argv)


{
  med_err ret = 0;
  med_idt fid = 0;
  med_int nmaa=0,mdim=0,sdim=0,nstep=0;
  int i=0;
  /*Pour les outils de type memchecker */
/*   char maa[MED_NAME_SIZE+1]=""; */
/*   char nomu[MED_LNAME_SIZE+1]=""; */
/*   char des [MED_COMMENT_SIZE+1]=""; */
/*   char dtunit[MED_SNAME_SIZE+1]=""; */
  char *maa   ;
  char *nomu  ;
  char *des   ;
  char *dtunit;
  char *axisname=0,*axisunit=0;
  med_mesh_type     meshtype;
  med_err           inomu;
  med_sorting_type  sortingtype;
  med_axis_type     axistype;
  med_bool          maa1exist=MED_FALSE,maa2exist=MED_FALSE;
  med_bool          maa3exist=MED_FALSE,maa4exist=MED_FALSE;
  
  /*Pour les outils de type memchecker */
  maa    = (char *) malloc(sizeof(char)*(MED_NAME_SIZE+1   ));
  nomu   = (char *) malloc(sizeof(char)*(MED_LNAME_SIZE+1  ));
  des    = (char *) malloc(sizeof(char)*(MED_COMMENT_SIZE+1));
  dtunit = (char *) malloc(sizeof(char)*(MED_SNAME_SIZE+1  ));

  /* Ouverture du fichier "test2.med" en lecture seule */
  fid = MEDfileOpen("test2.med",MED_ACC_RDONLY);
  if (fid < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test2.med");
    return -1;
  }

  /* Teste la présence des maillages */
  OBJEXIST(maa1);  OBJEXIST(maa2);  OBJEXIST(maa3);  OBJEXIST(maa4);
 
  /* Lecture du nombre de maillage dans le fichier */
  nmaa = MEDnMesh(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur a la lecture du nombre de maillage");
    return  -1;
  }


  /* Boucle sur tous les maillages, pour chaque maillage, on lit :
     - Le nom.
     - Le type
     - La dimension
     - La description
     - La dimension de l'espace si elle existe
     - Le nom universel s'il existe
  */
  printf("- Nombre de maillage dans test2.med = "IFORMAT"\n",nmaa);

  for (i=0;i< nmaa;i++) {
    /* lecture des informations */

    if ((sdim=MEDmeshnAxis(fid, i+1)) <0) {
      MESSAGE("Erreur a la lecture de la dimension de l'espace du maillage :");
      SSCRUTE(maa);
      ret = -1;
    }
    axisname  = (char*) malloc(MED_SNAME_SIZE*sdim+1);
    axisunit  = (char*) malloc(MED_SNAME_SIZE*sdim+1);

    if (MEDmeshInfo(fid,i+1, maa, &sdim, &mdim,  &meshtype, des, dtunit, &sortingtype, &nstep,
		    &axistype, axisname, axisunit) < 0) {
      MESSAGE("Erreur a la lecture des informations du maillage :"); SSCRUTE(maa);
      ret = -1;
    }
    /* lecture du nom universel */
    inomu = MEDmeshUniversalNameRd(fid,maa,nomu);
    /* affichage des donnees lues */
    if (inomu < 0)
      printf("maillage %d de nom %s, de dimension "IFORMAT" \n",i+1,maa,mdim);
    else
      printf("maillage %d de nom %s, de dimension "IFORMAT" et de nom univ. %s\n",i+1,maa,mdim,nomu);

    printf("La dimension de l'espace est "IFORMAT" \n",sdim);
    if (meshtype == MED_STRUCTURED_MESH)
      printf("Il s'agit d'un maillage structure \n");
    else
      printf("Il s'agit d'un maillage non structure \n");
    printf("Description associee au maillage : %s \n\n",des);
    printf("\t -Noms des axes : %s\n",  axisname);
    printf("\t -Unités des axes : %s\n",axisunit);
    printf("\t -Type de repère : %d\n", axistype);
    printf("\t -Nombre d'étape de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unité des dates : %s\n",dtunit);

    free(axisname);
    free(axisunit);
  }

  free(  maa    );
  free(  nomu   );
  free(  des    );
  free(  dtunit );


  /* Fermeture du fichier */
  if ( MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier test2.med");
    return -1;
  }

  return ret;
}




