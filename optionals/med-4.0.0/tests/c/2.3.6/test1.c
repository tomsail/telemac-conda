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
 * - Nom du fichier : test1.c
 *
 * - Description : tests des routines d'ouverture/fermeture de
 *                 fichiers MED 
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
  med_err ret = 0;
  med_idt fid;
  char des[MED_TAILLE_DESC+1]="Ceci est un courte description du mon fichier test1.med";

  /* creation du fichier "test1.med" */
  fid = MEDouvrir("test1.med",MODE_ACCES);
  if (fid < 0) {
    MESSAGE("Erreur à la creation du fichier");
    return -1;
  }

  /* ecriture d'un en-tete dans le fichier */
  if (MEDfichDesEcr(fid,des) < 0) {
    MESSAGE("Erreur à l'ecriture de l'en-tete du fichier");
    ret = -1;
  }

  /* fermeture du fichier */
  if ((ret = MEDfermer(fid)) < 0) {
    MESSAGE("Erreur à la fermeture du fichier"); 
    return -1; 
  } 

  /* re-ouverture du fichier en lecture seule */
  fid = MEDouvrir("test1.med",MED_LECTURE);
  if (fid < 0) {
    MESSAGE("Erreur à l'ouverture du fichier en mode MED_LECTURE");
    return -1;
  }

  /* fermeture du fichier */
  if (MEDfermer(fid) < 0)
    ret = -1;
  
  return ret;
}




