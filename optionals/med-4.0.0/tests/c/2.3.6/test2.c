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

#ifdef DEF_LECT_ECR
#define MODE_ACCES MED_LECTURE_ECRITURE
#elif DEF_LECT_AJOUT
#define MODE_ACCES MED_LECTURE_AJOUT
#else
#define MODE_ACCES MED_CREATION
#endif

int main (int argc, char **argv)

{
  med_err ret=0;
  med_idt fid;
  char des[MED_TAILLE_DESC+1]="";

  /* Verification de la conformite du format med du fichier test1.med */
  ret = MEDformatConforme("test1.med");
  if (ret < 0) {
    MESSAGE("Format HDF non conforme ou fichier inexistant");
    return -1;
  }

  /* Ouverture en mode de lecture du fichier "test1.med" */
  fid = MEDouvrir("test1.med",MED_LECTURE);
  if (fid < 0) {
      MESSAGE("Erreur a l'ouverture du fichier test1.med en mode MED_LECTURE");
      return -1;
  }
  
  /* Affiche de l'en-tete du fichier */
  ret = MEDfichEntete(fid,MED_FICH_DES, des);
  if (ret == 0) 
    printf("En-tete du fichier test1.med : %s\n",des);
  else {
    MESSAGE("Erreur a la lecture de l'en-tete du fichier test1.med");
    ret = -1;
  }
  
  /* Fermeture du fichier test1.med */
  ret = MEDfermer(fid); 
  if (ret < 0) {
    MESSAGE("Erreur a la fermeture du fichier test1.med");
    return -1;
  }
  
  /* Ouverture en mode creation du fichier test2.med */
  fid = MEDouvrir("test2.med",MODE_ACCES);
  if (fid < 0) {
    MESSAGE("Erreur a la creation du fichier test2.med");
    return -1;
  }
  
  /* Creation du maillage "maa1" de type MED_NON_STRUCTURE
     et de dimension 3 */
  if (MEDmaaCr(fid,"maa1",3,MED_NON_STRUCTURE,
		      "un premier maillage") < 0) {
    MESSAGE("Erreur a la creation du maillage maa1");
    ret = -1;
  }

  /* Ecriture du nom universel pour "maa1" */
  if (MEDunvCr(fid,"maa1") < 0) {
    MESSAGE("Erreur a la creation du nom universel de maa1");
    ret = -1;
  }

  /* Creation du maillage "maa2" de type MED_NON_STRUCTURE
     et de dimension 2 */
  if (MEDmaaCr(fid,"maa2",2,MED_NON_STRUCTURE,
		      "un second maillage") < 0) {
    MESSAGE("Erreur a la creation du maillage maa2");
    ret = -1;
  }

  /* Ecriture de la dimension de l'espace : Maillage de 
     dimension 2 dans un espace de dimension 3 */
  if (MEDdimEspaceCr(fid,"maa2",3) < 0) {
    MESSAGE("Erreur a l'ecriture de la dimension de l'espace"); 
    ret = -1; 
  }  

  /* Creation du maillage "maa3" de type MED_STRUCTURE
     et de dimension 1 */
  if (MEDmaaCr(fid,"maa3",1,MED_STRUCTURE,
		      "un troisieme maillage") < 0) {
    MESSAGE("Erreur a la creation du maillage maa3");
    ret = -1;
  }

  /* Fermeture du fichier */
  if ((ret = MEDfermer(fid))  < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }
  
  return ret;
}




