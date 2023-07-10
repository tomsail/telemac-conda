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

med_err 
MEDprofilLire(med_idt fid,med_int *pflval, char *profilname)
{
  med_err ret = -1;
  med_idt gid=0;
  char chemin[MED_TAILLE_PROFILS+MED_TAILLE_NOM+1]=""; 

  /*
   * On inhibe le gestionnaire d'erreur HDF 5
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;


  /* 
   * ouverture du groupe /PROFILS/"nom"
   */  
  strcpy(chemin,MED_PROFILS);
  strcat(chemin,profilname); 
  if ((gid = _MEDdatagroupOuvrir(fid,chemin)) < 0) {
    MESSAGE("Impossible d'ouvrir le datagroup : ");
    SSCRUTE(chemin); goto ERROR;
  }

  /*
   * Lecture du profil
   */
#if defined(HAVE_F77INT64)
  if ( _MEDdatasetNumLire(gid,MED_NOM_PFL,MED_INT64,
				 MED_NO_INTERLACE,1,MED_ALL,
				 MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char *) pflval) < 0) {
    MESSAGE("Impossible de lire le dataset : ");
    SSCRUTE(MED_NOM_PFL); goto ERROR;
  }
#else
  if ( _MEDdatasetNumLire(gid,MED_NOM_PFL,MED_INT32,
				 MED_NO_INTERLACE,1,MED_ALL,
				 MED_NOPF,MED_NO_PFLMOD,MED_PFL_NON_COMPACT,0,MED_NOPG,0,
				 (unsigned char *) pflval) < 0) {
    MESSAGE("Impossible de lire le dataset : ");
    SSCRUTE(MED_NOM_PFL); goto ERROR;
  }

#endif

  /*
   * On ferme tout
   */

  ret = 0;
 ERROR:

  if ( gid > 0 ) if ( _MEDdatagroupFermer(gid) < 0) {
    MESSAGE("Impossible de fermer le datagroup : ");
    ISCRUTE_id(gid); ret = -1; 
  }

  return ret;
}
