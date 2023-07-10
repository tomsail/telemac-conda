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
MEDcheckVersion(med_idt fid)
{
  med_int  majeur=0, mineur=0, release=0;
  med_int  versionMMR,versionMM;

/*   ISCRUTE_id(fid); */
/*   ISCRUTE(majeur); */
/*   ISCRUTE(mineur); */
/*   ISCRUTE(release); */

  MEDversionLire(fid, &majeur, &mineur, &release);
  versionMM  = 100*majeur+10*mineur;
  versionMMR = versionMM+release;

/*   ISCRUTE(majeur); */
/*   ISCRUTE(mineur); */
/*   ISCRUTE(release); */

  if (versionMMR >= 240 ) {
    MESSAGE("L'API appelée ne gère pas les versions de fichiers MED >=240");
    MESSAGE("Utilisez l'API MED3.x.y .");
    return -1;
  }

  return 0;
}
