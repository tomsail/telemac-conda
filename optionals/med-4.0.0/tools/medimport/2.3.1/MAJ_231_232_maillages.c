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


 

#include "med_config.h"
#include "med_outils.h"
#include "string.h"

/* #ifdef __cplusplus */
/* } */
/* #endif */

#include "MAJ_231_232.h"

void MAJ_231_232_maillages(med_idt fid)
{
  med_idt gid;
  med_err ret;
  int n,i;
  char nomi   [MED_TAILLE_NOM+1];
  char nomf   [MED_TAILLE_NOM+1];
  char chemini[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
  char cheminf[MED_TAILLE_MAA+MED_TAILLE_NOM+1];
 
  /* Lecture du nombre de maillages */
  n = 0;
  _MEDnObjets(fid,MED_MAA,&n);
  EXIT_IF(n < 0,"Erreur a la lecture du nombre de maillage",NULL);

  /* 
   * Mise a jour des maillages :
   *  "
   */

  for (i=0;i<n;i++) {
    /* on recupere le nom du maillage */
    ret = _MEDobjetIdentifier(fid,MED_MAA,i,nomi);
    EXIT_IF(ret < 0,"Identification d'un maillage",NULL);

    if ( MAJ_231_232_chaine(nomi,nomf) ) {

      fprintf(stdout,"  >>> Normalisation du nom de maillage [%s] \n",nomi);
      /* on accede au maillage */
      strcpy(chemini,MED_MAA);
      strcat(chemini,nomi);
      strcpy(cheminf,MED_MAA);
      strcat(cheminf,nomf);

      ret = H5Gmove(fid, chemini, cheminf  );
      EXIT_IF(ret < 0,"Renommage du maillage en",nomf);
     fprintf(stdout,"  >>> Normalisation du nom du maillage [%s] ... OK ... \n",nomf);
    }
  }

}
