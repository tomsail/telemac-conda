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
    return -1;
  }

/*   Fermeture du fichier test1.med */
  ret = MEDfileClose(fid);
  if (ret < 0) {
    MESSAGE("Erreur a la fermeture du fichier test1.med");
    return -1;
  }

  return ret;
}




