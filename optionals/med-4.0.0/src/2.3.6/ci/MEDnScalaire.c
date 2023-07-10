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
#include <string.h>
#include <med_outils.h>

med_int 
MEDnScalaire(med_idt fid)
{
  int n1;
  med_int n2;
  char chemin[MED_TAILLE_NUM_DATA+MED_TAILLE_NOM+1];

  /*
   * On inhibe le gestionnaire d'erreur HDF 
   */
  _MEDmodeErreurVerrouiller();
if (MEDcheckVersion(fid) < 0) return -1;

  
  /* 
   * Si le Data Group "NUM_DATA" n'existe pas => 0
   * Sinon le nb de variable scalaires
   */
  strcpy(chemin,MED_NUM_DATA);
  n1 = 0;
  _MEDnObjets(fid,chemin,&n1);
  n2 = (med_int) n1;

  return n2;
}
