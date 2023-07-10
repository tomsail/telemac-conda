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
MEDversionLire(med_idt fid, med_int *majeur, med_int *mineur, med_int *release) 
{
  med_err ret = 0;
  med_idt gid;

  /* On ouvre le group ou se trouvent les infos */
  if ((gid = _MEDdatagroupOuvrir(fid,MED_INFOS)) < 0) {
    *majeur = 2;
    *mineur = -1;
    *release = -1;
    ret = 0;
  }
  else {

    if ((ret = _MEDattrEntierLire(gid,MED_NOM_MAJEUR,majeur)) < 0)
      return -1;

    if ((ret = _MEDattrEntierLire(gid,MED_NOM_MINEUR,mineur)) < 0)
      return -1;

    if ((ret = _MEDattrEntierLire(gid,MED_NOM_RELEASE,release)) < 0)
      return -1;

/*   ISCRUTE(*majeur); */
/*   ISCRUTE(*mineur); */
/*   ISCRUTE(*release); */

    /* On ferme tout */
    if ((ret = _MEDdatagroupFermer(gid)) < 0)
      return -1;
  }

  return ret;
}
