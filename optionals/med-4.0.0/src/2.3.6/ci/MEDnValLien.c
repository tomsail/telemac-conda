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


#include <med.h>
#include <med_config.h>
#include <med_outils.h>

#include <string.h>
#include <stdlib.h>

med_int 
MEDnValLien(med_idt fid, char *maa)
{
  med_int n = 0;
  med_idt gid;
  char chemin[MED_TAILLE_LIENS+MED_TAILLE_NOM+1]; 
  med_err ret;
  
  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /* 
   * ouverture du groupe /LIENS/<maa>
   */  
  strcpy(chemin,MED_LIENS);
  strcat(chemin,maa); 
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le groupe MED_LIENS : ");
    SSCRUTE(chemin); return -1;
  }

  if ((ret = _MEDattrEntierLire(gid,MED_NOM_NBR,&n)) < 0) {
    MESSAGE("Erreur à la lecture de l'attribut n : ");
    ISCRUTE(n); return -1;
  }

  /*
   * On ferme tout
   */
  if ((ret = _MEDdatagroupFermer(gid)) < 0)
    return -1; 

  return n;
}
