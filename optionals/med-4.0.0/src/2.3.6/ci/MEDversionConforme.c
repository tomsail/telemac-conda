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

med_err
MEDversionConforme(const char *nom) {
  med_int majeur, mineur;
  med_idt fid, gid;
  med_err ret;

  /*
   * On inhibe le gestionnaire d'erreur HDF
   */
  _MEDmodeErreurVerrouiller();

  /*
   * On ouvre le fichier MED en mode MED_LECT
   */     
  if ((fid = _MEDfichierOuvrir((char *)nom,MED_LECTURE)) < 0)
    return -1;
  
  /*
   * Lecture du numero de version 
   */
  if ((gid = _MEDdatagroupOuvrir(fid,MED_INFOS)) < 0) 
    return -1;
  
  if ((ret = _MEDattrEntierLire(gid,MED_NOM_MAJEUR,&majeur)) < 0)
    return -1;
  
  if ((ret = _MEDattrEntierLire(gid,MED_NOM_MINEUR,&mineur)) < 0)
    return -1;							
  
  /* 
   * On ferme tout 
   */
  if ((ret = _MEDdatagroupFermer(gid)) < 0)
    return -1;
  
  if ((ret = _MEDfichierFermer(fid)) < 0)
    return -1;
 
/* On autorise les versions 2.2.x et 2.3.x , ce qui est le cas depuis  med2.3.1 */ 
  if ((majeur == 2) && (mineur >1) && (mineur<4) )
    return 0;
  else
    return -1;
}
