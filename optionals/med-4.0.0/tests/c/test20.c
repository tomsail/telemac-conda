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
 * - Nom du fichier : test20.c
 *
 * - Description : montage/demontage de fichiers MED 
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
  med_idt        fid,fid2,fid3,mid,mid2;
  med_int        ncha, ncomp, nmaa;
  med_field_type type;
  char           comp[3*MED_SNAME_SIZE+1]="",unit[3*MED_SNAME_SIZE+1]="";
  char           nom       [MED_NAME_SIZE+1] ="";
  char           _meshname [MED_NAME_SIZE+1] ="";
  char           _dtunit   [MED_SNAME_SIZE+1]="";
  med_int        _ncstp=0;
  med_bool       _local;
  int i;

  /*
   * Phase 1 : Test de montage de champs dans un fichier contenant des maillages
   */

  /* Ouverture du fichier test20-0.med en mode lecture et ajout */
  if ((fid = MEDfileOpen("test20-0.med",MED_ACC_RDEXT)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test20-0.med");
    return -1;
  }
  printf("On ouvre le fichier test20-0.med \n");

  /* Lecture du nombre de champs */
  if ((ncha = MEDnField(fid)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de champs");
    return -1;
  }
  printf("Nombre de champs dans test20-0.med :"IFORMAT" \n",ncha);

  /* On fait le montage dans test20-0.med de tous les champs de test10.med */
  mid = MEDfileObjectsMount(fid,"test10.med",MED_FIELD);
  if (mid < 0) {
    MESSAGE("Echec du montage des champs de test10.med");
    return -1;
  }
  printf("On monte les champs du fichier test10.med dans le fichier test20-0.med \n");

  /* Combien de champs dans le fichier "test20-0.med" apres le montage */
  if ((ncha = MEDnField(fid)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20-0.med apres montage : "IFORMAT" \n",ncha);

  /* On s'assure que les champs sont bien accessibles */
  for (i=1;i<=ncha;i++) {

    /* Combien de composantes dans le champ */
    if ((ncomp = MEDfieldnComponent(fid,i)) < 0) {
      MESSAGE("Erreur a lecture du nombre de composante du champ");
      return -1;
    }

    /* Lecture des infos sur le champ */
    if (MEDfieldInfo(fid,i,nom,_meshname,&_local,&type,comp,unit,_dtunit,&_ncstp) < 0) {
      MESSAGE("Erreur a la lecture des informations sur le champ");
      return -1;
    }

    printf("Nom du champ : |%s| de type %d\n",nom,type);
    printf("Nom des composantes : |%s|\n",comp);
    printf("Unites des composantes : |%s| \n",unit);
    printf("Unites des dates  : |%s| \n",_dtunit);
    printf("Le maillage associé est |%s|\n",_meshname);

  }

  /* On demonte le fichier */
  if (MEDfileObjectsUnmount(fid,mid,MED_FIELD) < 0) {
    MESSAGE("Echec du demontage de test10.med");
    return -1;
  }
  printf("On demonte le fichier test10.med dans test20-0.med\n");

  /* Combien de champs dans le fichier "test20-0.med" apres le demontage */
  if ((ncha = MEDnField(fid)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20-0.med apres demontage: "IFORMAT" \n",ncha);

  /* On ferme le fichier test20-0.med */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur lors de la fermeture du fichier");
    return -1;
  }
  printf("On ferme le fichier test20-0.med \n");


  /*
   * Phase 2 : Test de montage de champs et de maillages dans un fichier vierge
   */


  /* Creation du fichier test20.med */
  if ((fid = MEDfileOpen("test20.med",MODE_ACCES))  < 0) {
    MESSAGE("Erreur a la creation du fichier test20.med");
    return -1;
  }
  printf("On cree le fichier test20.med \n");

  /* Montage dans test20.med de tous les maillages de test20-0.med */
  mid2 = MEDfileObjectsMount(fid,"test20-0.med",MED_MESH);
  if (mid2 < 0) {
    MESSAGE("Echec du montage de test20-0.med");
    return -1;
  }
  printf("On monte les maillages du fichier test20-0.med dans le fichier test20.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMesh(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur lors de la lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages apres montage de test20-0.med : "IFORMAT"\n",nmaa);

  /* Montage dans test20.med de tous les champs de test10.med */
  mid = MEDfileObjectsMount(fid,"test10.med",MED_FIELD);
  if (mid < 0) {
    MESSAGE("Echec du montage de test10.med");
    return -1;
  }
  printf("On monte les champs du fichier test10.med dans le fichier test20.med \n");

  /* Combien de champs dans le fichier "test20.med" apres le montage */
  if ((ncha = MEDnField(fid)) < 0) {
    MESSAGE("Erreur lors de la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20.med apres montage : "IFORMAT" \n",ncha);

  /* Demontage du fichier test10.med */
  if (MEDfileObjectsUnmount(fid,mid,MED_FIELD) < 0) {
    MESSAGE("Echec du demontage de test10.med");
    return -1;
  }
  printf("On demonte le fichier test10.med dans test20.med \n");

  /* Demontage du fichier test20-0.med */
  if (MEDfileObjectsUnmount(fid,mid2,MED_MESH) < 0) {
    MESSAGE("Echec du demontage de test20-0.med");
    return -1;
  }
  printf("On demonte le fichier test20-0.med du test20.med\n");

  /* Fermeture du fichier test20.med */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur de la fermeture du fichier");
    return -1;
  }
  printf("On ferme le fichier test20.med \n");

  /*
   * Phase 3 : Test de montage de champs et de maillages dans un fichier vierge à
   * partir de deux fichiers HDFs contenant une sous arborescence MED avec Champs et Maillages 
   */

  /* Creation du fichier test20b.med */
  if ((fid = MEDfileOpen("test20b.med",MODE_ACCES))  < 0) {
    MESSAGE("Erreur a la creation du fichier test20.med");
    return -1;
  }
  printf("On cree le fichier test20b.med \n");

  /* Ouverture du fichier HDF test2-med.hdf */
  if ((fid2 = MEDfileOpen("test2-med.hdf",MED_ACC_RDONLY))  < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test2-med.hdf");
    return -1;
  }
  printf("On ouvre le fichier test2-med.hdf \n");

  /* Montage dans test20b.med de tous les maillages de test20-0.med */
  mid2 = MEDfileObjectsMountById(fid,fid2,"/byid2",MED_MESH);
  if (mid2 < 0) {
    MESSAGE("Echec du montage de test2-med.hdf");
    return -1;
  }
  printf("On monte les maillages du fichier test2-med.hdf dans le fichier test20b.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMesh(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur lors de la lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages apres montage de test2-med.hdf : "IFORMAT"\n",nmaa);

  /* Ouverture du fichier HDF test10-med.hdf */
  if ((fid3 = MEDfileOpen("test10-med.hdf",MED_ACC_RDONLY))  < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test10-med.hdf");
    return -1;
  }
  printf("On ouvre le fichier test10-med.hdf \n");

  /* Montage dans test20b.med de tous les champs de test10-med.hdf */
  mid = MEDfileObjectsMountById(fid,fid3,"/byid10/",MED_FIELD);
  if (mid < 0) {
    MESSAGE("Echec du montage de test10-med.hdf");
    return -1;
  }
  printf("On monte les champs du fichier test10-med.hdf dans le fichier test20b.med \n");

  /* Combien de champs dans le fichier "test20b.med" apres le montage */
  if ((ncha = MEDnField(fid)) < 0) {
    MESSAGE("Erreur lors de la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20b.med apres montage : "IFORMAT" \n",ncha);

  /* Demontage du fichier test10-med.hdf */
  if (MEDfileObjectsUnmount(fid,mid,MED_FIELD) < 0) {
    MESSAGE("Echec du demontage de test10-med.hdf");
    return -1;
  }
  printf("On demonte et on ferme le fichier test10-med.hdf de test20b.med \n");

  /* Demontage du fichier test2-med.hdf */
  if (MEDfileObjectsUnmount(fid,mid2,MED_MESH) < 0) {
    MESSAGE("Echec du demontage de test2-med.hdf");
    return -1;
  }
  printf("On demonte et on ferme le fichier test2-med.hdf de test20b.med\n");

  /* Fermeture du fichier test20b.med */
  if (MEDfileClose(fid) < 0) {
    MESSAGE("Erreur de la fermeture du fichier");
    return -1;
  }
  printf("On ferme le fichier test20b.med \n");


  return 0;
}
