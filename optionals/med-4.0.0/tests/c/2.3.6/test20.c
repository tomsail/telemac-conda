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
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

int main (int argc, char **argv)


{
  int i;
  med_idt fid,mid,mid2;
  med_int ncha, ncomp, nmaa;
  med_type_champ type;
  char comp[3*MED_TAILLE_PNOM+1],unit[3*MED_TAILLE_PNOM+1];
  char nom[MED_TAILLE_NOM+1];

  /* Ouverture du fichier test20-0.med en mode lecture et ajout */
  if ((fid = MEDouvrir("test20-0.med",MED_LECTURE_AJOUT)) < 0) {
    MESSAGE("Erreur a l'ouverture du fichier test20-0.med");
    return -1;
  }
  printf("On ouvre le fichier test20-0.med \n");

  /* Lecture du nombre de champs */
  if ((ncha = MEDnChamp(fid,0)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de champs");
    return -1;
  }
  printf("Nombre de champs dans test20-0.med :"IFORMAT" \n",ncha);

  /* On fait le montage dans test20-0.med de tous les champs de test10.med */
  mid = MEDmonter(fid,"test10.med",MED_CHAMP);
  if (mid < 0) {
    MESSAGE("Echec du montage des champs de test10.med");
    return -1;
  }
  printf("On monte les champs du fichier test10.med dans le fichier test20-0.med \n");

  /* Combien de champs dans le fichier "test20-0.med" apres le montage */
  if ((ncha = MEDnChamp(fid,0)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20-0.med apres montage : "IFORMAT" \n",ncha);

  /* On s'assure que les champs sont bien accessibles */
  for (i=1;i<=ncha;i++) {

    /* Combien de composantes dans le champ */
    if ((ncomp = MEDnChamp(fid,i)) < 0) {
      MESSAGE("Erreur a lecture du nombre de composante du champ");
      return -1;
    }

    /* Lecture des infos sur le champ */
    if (MEDchampInfo(fid,i,nom,&type,comp,unit,ncomp) < 0) {
      MESSAGE("Erreur a la lecture des informations sur le champ");
      return -1;
    }

    printf("Champ de nom [%s] de type %d et avec "IFORMAT" composantes \n",nom,type,ncomp);
  }

  /* On demonte le fichier */
  if (MEDdemonter(fid,mid,MED_CHAMP) < 0) {
    MESSAGE("Echec du demontage de test10.med");
    return -1;
  }
  printf("On demonte le fichier test10.med dans test20-0.med\n");

  /* Combien de champs dans le fichier "test20-0.med" apres le demontage */
  if ((ncha = MEDnChamp(fid,0)) < 0) {
    MESSAGE("Erreur a la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20-0.med apres demontage: "IFORMAT" \n",ncha);

  /* On ferme le fichier test20-0.med */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur lors de la fermeture du fichier");
    return -1;
  }
  printf("On ferme le fichier test20-0.med \n");

  /* Creation du fichier test20.med */
  if ((fid = MEDouvrir("test20.med",MODE_ACCES))  < 0) {
    MESSAGE("Erreur a la creation du fichier test20.med");
    return -1;
  }
  printf("On cree le fichier test20.med \n");

  /* Montage dans test20.med de tous les maillages de test20-0.med */
  mid2 = MEDmonter(fid,"test20-0.med",MED_MAILLAGE);
  if (mid2 < 0) {
    MESSAGE("Echec du montage de test20-0.med");
    return -1;
  }
  printf("On monte les maillages du fichier test20-0.med dans le fichier test20.med \n");

  /* Lecture du nombre de maillages */
  nmaa = MEDnMaa(fid);
  if (nmaa < 0) {
    MESSAGE("Erreur lors de la lecture du nombre de maillage");
    return -1;
  }
  printf("Nombre de maillages apres montage de test20-0.med : "IFORMAT"\n",nmaa);

  /* Montage dans test20.med de tous les champs de test10.med */
  mid = MEDmonter(fid,"test10.med",MED_CHAMP);
  if (mid < 0) {
    MESSAGE("Echec du montage de test10.med");
    return -1;
  }
  printf("On monte les champs du fichier test10.med dans le fichier test20.med \n");

  /* Combien de champs dans le fichier "test20.med" apres le montage */
  if ((ncha = MEDnChamp(fid,0)) < 0) {
    MESSAGE("Erreur lors de la lecture du nombre de champ");
    return -1;
  }
  printf("Nombre de champs dans test20.med apres montage : "IFORMAT" \n",ncha);

  /* Demontage du fichier test10.med */
  if (MEDdemonter(fid,mid,MED_CHAMP) < 0) {
    MESSAGE("Echec du demontage de test10.med");
    return -1;
  }
  printf("On demonte le fichier test10.med dans test20.med \n");

  /* Demontage du fichier test20-0.med */
  if (MEDdemonter(fid,mid2,MED_MAILLAGE) < 0) {
    MESSAGE("Echec du demontage de test20-0.med");
    return -1;
  }
  printf("On demonte le fichier test20-0.med dans test20.med\n");

  /* Fermeture du fichier test20.med */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur de la fermeture du fichier");
    return -1;
  }
  printf("On ferme le fichier test20.med \n");
  
  return 0;
}
