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
 * - Nom du fichier : test21.c
 *
 * - Description : ecriture de valeurs scalaires numeriques dans un fichier MED
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
  med_err ret;
  med_idt fid;
  char nom_scalaire1[MED_TAILLE_NOM+1] = "VariableEntiere";
  char description1[MED_TAILLE_DESC+1] = "Une premiere description";
  char nom_scalaire2[MED_TAILLE_NOM+1] = "VariableFlottante";
  char description2[MED_TAILLE_DESC+1] = "Une seconde description";
  med_int vali1 = 56;
  med_int vali2 = -789;
  med_float valr1 = 67.98;
 
  /* Creation du fichier test21.med */
  if ((fid = MEDouvrir("test21.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test21.med");
    return -1;
  }
 
  /* Creation d'un variable scalaire entiere */
  if (MEDscalaireCr(fid,nom_scalaire1,MED_INT,description1) < 0) {
    MESSAGE("Erreur a la creation d'une variable scalaire entière");
    return -1;
  }
  printf("Creation d'une variable scalaire entiere \n");

  /* Ecriture d'un valeur sans pas de temps et sans numero d'ordre*/
  if (MEDscalaireEntierEcr(fid,nom_scalaire1,vali1,MED_NOPDT,"",0.0,MED_NONOR) < 0) {
    MESSAGE("Erreur a l'ecriture d'une valeur entiere");
    return -1;
  }
  printf("Ecriture d'une valeur entiere sans pas de temps \n");

  /* Ecriture d'une valeur entiere avec 1 pas de temps et sans numero d'ordre */
  if (MEDscalaireEntierEcr(fid,nom_scalaire1,vali2,1,"ms",5.5,MED_NONOR) < 0) {
    MESSAGE("Erreur a l'ecriture d'une valeur entiere");
    return -1;
  }
  printf("Ecriture d'une valeur entiere avec pas de temps \n");

  /* Creation d'un variable scalaire flottante */
  if (MEDscalaireCr(fid,nom_scalaire2,MED_FLOAT64,description2) < 0) {
    MESSAGE("Erreur a la creation d'une variable scalaire flottante");
    return -1;
  }
  printf("Creation d'une variable scalaire flottante \n");

  /* Ecriture d'une valeur reelle avec 1 pas de temps et 1 numero d'ordre */
  if (MEDscalaireFlottantEcr(fid,nom_scalaire2,valr1,1,"ms",5.5,2) < 0) {
    MESSAGE("Erreur a l'ecriture d'une valeur flottante");
    return -1;
  }
  printf("Ecriture d'une valeur reelle avec pas de temps et numero d'ordre \n");
  
  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return 0;
}
