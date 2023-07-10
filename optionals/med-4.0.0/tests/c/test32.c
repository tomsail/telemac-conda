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
 * - Nom du fichier : test32.c
 *
 * - Description : lecture nominale d'une numerotation globale dans un maillage MED 
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
  med_idt fid;
  med_int mdim,sdim;
  char maa[MED_NAME_SIZE+1];
  /* le nombre de noeuds */
  med_int nnoe = 0;
  /* table des numeros global */
  med_int *numglobalnoe=NULL;
  /* variable de stockage pour reperer le maillage */
  med_int i;
  char des[MED_COMMENT_SIZE+1]="";
  char dtunit[MED_SNAME_SIZE+1]="";
  char nomcoo[3*MED_SNAME_SIZE+1]="";
  char unicoo[3*MED_SNAME_SIZE+1]="";
  med_axis_type rep;
  med_mesh_type type;
  med_sorting_type sort;
  med_int nstep=0;
  med_bool chgt=MED_FALSE,trsf=MED_FALSE;



  if (argc != 2) {
    MESSAGE("Il faut passer un fichier MED en param�tre");
    return -1;
  }

  /* Ouverture du fichier passe en argument */
  if ((fid = MEDfileOpen(argv[1],MED_ACC_RDWR)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier : "); SSCRUTE(argv[1]);
    return -1;
  }

  if ((sdim=MEDmeshnAxis(fid, 1)) <0) {
    MESSAGE("Erreur � la lecture de la dimension de l'espace du maillage :");
    SSCRUTE(maa);
    return -1;
  }

  /* Lecture des infos concernant le premier maillage */
  if ( MEDmeshInfo( fid, 1,  maa, &sdim, &mdim, &type, des, dtunit, &sort,
		    &nstep,  &rep, nomcoo,unicoo) < 0 ) {
    MESSAGE("Erreur a la lecture des informations sur le maillage : ");SSCRUTE(maa);
    return -1;
  } else {
    printf("Maillage de nom : |%s| , de dimension : "IFORMAT" , et de type %d\n",maa,mdim,type);
    printf("\t -Dimension de l'espace : "IFORMAT"\n",sdim);
    printf("\t -Description du maillage : %s\n",des);
    printf("\t -Noms des axes : %s\n",nomcoo);
    printf("\t -Unit�s des axes : %s\n",unicoo);
    printf("\t -Type de rep�re : %d\n",rep);
    printf("\t -Nombre d'�tapes de calcul : "IFORMAT"\n",nstep);
    printf("\t -Unit� des dates : %s\n\n",dtunit);
  }

  /* Lecture du nombre de noeuds */
  if ( (nnoe = MEDmeshnEntity(fid,maa,MED_NO_DT,MED_NO_IT,
			      MED_NODE,MED_NONE,MED_COORDINATE,MED_NO_CMODE,
			      &chgt,&trsf)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de noeuds ");
    return -1;
  }
  printf("Nombre de noeuds : "IFORMAT" \n",nnoe);


  /* Allocations memoires */

  /* table de la numerotation globale
     profil : (nombre de noeuds ) */
  if (nnoe > 0) {
    numglobalnoe = (med_int*) malloc(sizeof(med_int)*nnoe);
  }

  /* lecture de la numerotation globale attachee aux noeuds*/
  if (MEDmeshGlobalNumberRd(fid,maa,MED_NO_DT,MED_NO_IT,MED_NODE,MED_NONE,numglobalnoe)<0) {
    MESSAGE("Erreur a la lecture de de la numerotation globale");
    return -1;
  }

  /* ecriture a l'ecran des resultats */
  for (i=0;i<nnoe;i++)
    printf("Numero global du noeud "IFORMAT" : "IFORMAT" \n",i+1,numglobalnoe[i]);

  free(numglobalnoe);

  /* Fermeture du fichier */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier ");
    return -1;
  }

  return 0;
}

