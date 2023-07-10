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
 * - Nom du fichier : test12.c
 *
 * - Description : ecriture d'une equivalence dans un maillage MED 
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
  med_idt fid;
  char maa[MED_TAILLE_NOM+1]= "maa1";
  char equ[MED_TAILLE_NOM+1] = "equivalence";
  char des[MED_TAILLE_DESC+1] = "equivalence sur les mailles MED_TRIA3";
  med_int mdim = 3;
  med_int ncor = 3;
  med_int cor[6] = {1,2,3,4,5,6};

  /* Creation du fichier "test12.med" */
  if ((fid = MEDouvrir("test12.med",MODE_ACCES)) < 0) {
    MESSAGE("Erreur a la creation du fichier test12.med");
    return -1;
  }
  
  /* Creation du maillage */
  if (MEDmaaCr(fid,maa,3,MED_NON_STRUCTURE,
		   "Un maillage pour test12") < 0) {
    MESSAGE("Erreur a la creation du maillage");
    return -1;
  }
  
  /* Creation de l'equivalence */
  if (MEDequivCr(fid,maa,equ,des) < 0) {
    MESSAGE("Erreur a la creation de l'equivalence");
    return -1;
  }

  /* Ecriture des sur les mailles MED_TRIA3 */
  if (MEDequivEcr(fid,maa,equ,cor,ncor,MED_MAILLE,MED_TRIA3) < 0) {
    MESSAGE("Erreur a l'ecriture du tableau des correspondances");
    return -1;
  }

  /* Fermeture du fichier */
  if (MEDfermer(fid) < 0) {
    MESSAGE("Erreur a la fermeture du fichier");
    return -1;
  }

  return 0;
}




