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
 * - Nom du fichier : test2.c
 *
 * - Description : exemples de creation de maillages MED.
 *
 *****************************************************************************/

#include <med.h>
#define MESGERR 1
#include <med_utils.h>
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
  med_err ret=0;
  med_idt fid=0;
  char des[MED_COMMENT_SIZE+1]="";
  med_bool hdfok=MED_FALSE, medok=MED_FALSE;
  med_bool fileexist=MED_FALSE, accessok=MED_FALSE;
  char axisname[3*MED_SNAME_SIZE+1]="";
  char axisunit[3*MED_SNAME_SIZE+1]="";
  int  cstp = 0;

  strcat(axisname,"x               ");
  strcat(axisname,"y               ");
  strcat(axisname,"z               ");
  strcat(axisunit,"cm              ");
  strcat(axisunit,"cm              ");
  strcat(axisunit,"cm              ");

  /* Verification de la conformite du format med du fichier test1.med */
  MEDfileExist( "test1.med", MED_ACC_RDONLY, &fileexist, &accessok );
  if (!fileexist) {
    MESSAGE("Le fichier test1.med n'existe pas.");
  }
  if (!accessok) {
    MESSAGE("Le fichier test1.med ne peut pas être ouvert selon le mode d'accès demandé .");
  }

  /* Verification de la conformite du format med du fichier test1.med */
  ret = MEDfileCompatibility("test1.med",&hdfok,&medok);
  if (!hdfok) {
    MESSAGE("Format HDF non conforme ou fichier inexistant");
    return -1;
  }
  if (!medok) {
    MESSAGE("Format MED non conforme ou fichier inexistant");
    return -1;
  }

/*   Ouverture en mode de lecture du fichier "test1.med"  */
  fid = MEDfileOpen("test1.med",MED_ACC_RDONLY);
  if (fid < 0) {
      MESSAGE("Erreur a l'ouverture du fichier test1.med en mode MED_LECTURE");
      return -1;
  }

/*    Affiche de l'en-tete du fichier  */
  ret = MEDfileCommentRd(fid, des);
  if (ret == 0)
    printf("En-tete du fichier test1.med : %s\n",des);
  else {
    MESSAGE("Erreur a la lecture de l'en-tete du fichier test1.med");
    ret = -1;
  }

/*   Fermeture du fichier test1.med */
  ret = MEDfileClose(fid);
  if (ret < 0) {
    MESSAGE("Erreur a la fermeture du fichier test1.med");
    return -1;
  }

  /* Verification de la conformite du format med du fichier test2.med */
  MEDfileExist( "test2.med", MODE_ACCES, &fileexist, &accessok );
  if (!fileexist) {
    MESSAGE("Le fichier test2.med n'existe pas.");
  }
  if (!accessok) {
    MESSAGE("Le fichier test2.med ne peut pas être ouvert selon le mode d'accès demandé .");
  }
  /* Ouverture en mode creation du fichier test2.med */
  fid = MEDfileOpen("test2.med",MODE_ACCES);
  if (fid < 0) {
    MESSAGE("Erreur a la creation du fichier test2.med");
    return -1;
  }

  /* Creation du maillage "maa1" de type MED_UNSTRUCTURED_MESH
     et de dimension 3 */
  if (MEDmeshCr(fid,"maa1",3,3,MED_UNSTRUCTURED_MESH,
		"un premier maillage","s",MED_SORT_DTIT,
		MED_CARTESIAN,axisname,axisunit) < 0) {
    MESSAGE("Erreur a la creation du maillage maa1");
    ret = -1;
  }

  /* Ecriture du nom universel pour "maa1" */
  if (MEDmeshUniversalNameWr(fid,"maa1") < 0) {
    MESSAGE("Erreur a la creation du nom universel de maa1");
    ret = -1;
  }

  /* Creation du maillage "maa2" de type MED_UNSTRUCTURED_MESH
     et de dimension 2 dans un espace de dimension 3*/
  if (MEDmeshCr(fid,"maa2",3,2,MED_UNSTRUCTURED_MESH,
		"un second maillage","s",MED_SORT_DTIT,
		MED_CARTESIAN,axisname,axisunit) < 0) {
    MESSAGE("Erreur a la creation du maillage maa2");
    ret = -1;
  }

  cstp = 1;
  if ( MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,
			      MED_NO_DT,MED_NO_IT,0) < 0) {
    fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

  cstp++;
  if ( MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,
			      1,3,1.1) < 0) {
    fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

  cstp++;
  if ( MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,
				0,0,1.1) < 0) {
    fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

  cstp++;
  if ( MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,
				0,-1,1.1) < 0) {
    fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

  cstp++;
 if ( MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,
			      -1,20,1.1) < 0) {
    fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 fprintf(stderr,"Erreur attendue : \n");
 if ( MEDmeshComputationStepCr(fid,"maa2",MED_NO_DT,MED_NO_IT,
			      20,-1,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 fprintf(stderr,"Erreur attendue : \n");
 if ( MEDmeshComputationStepCr(fid,"maa2",0,0,
			      20,-1,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 if ( MEDmeshComputationStepCr(fid,"maa2",20,-1,
			      20,-1,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 fprintf(stderr,"Erreur attendue car l'étape a été crée précedement: \n");
 if ( MEDmeshComputationStepCr(fid,"maa2",1,3,
			       20,-1,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 if ( MEDmeshComputationStepCr(fid,"maa2",20,-1,
			       20,10,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 fprintf(stderr,"Erreur attendue car cette étape ne se place pas en dernière position : \n");
 if ( MEDmeshComputationStepCr(fid,"maa2",20,5,
			       20,5,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }

 cstp++;
 fprintf(stderr,"Erreur attendue car la création de cette étape chevauche la (20,10) : \n");
 if ( MEDmeshComputationStepCr(fid,"maa2",20,-1,
			       20,20,1.1) < 0) {
   fprintf(stderr,"Erreur à la création de l'étape de calcul n°%d du maillage maa2\n",cstp);
/*     ret = -1; */
  }


  /* Creation du maillage "maa3" de type MED_UNSTRUCTURED_MESH
     et de dimension 1 un espace de dimension 3*/
  if (MEDmeshCr(fid,"maa3",3,1,MED_UNSTRUCTURED_MESH,
		"un troisieme maillage","s",MED_SORT_DTIT,
		MED_CARTESIAN,axisname,axisunit) < 0) {
    MESSAGE("Erreur a la creation du maillage maa3");
    ret = -1;
  }

  /* Fermeture du fichier */
  if ( MEDfileClose(fid)  < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return ret;
}




