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
_MEDcheckVersion30(med_idt fid)
{
  med_err _ret=-1;
  med_int  majeur=0, mineur=0, release=0;
  med_int  fileversionMMR,fileversionMM,fileversionM;
  med_int  libversionMMR,libversionMM,libversionM;

  if ( MEDfileNumVersionRd(fid, &majeur, &mineur, &release) < 0 ) {
    MED_ERR_(_ret,MED_ERR_CALL,MED_ERR_API,"MEDfileNumVersionRd");
    ISCRUTE(majeur);ISCRUTE(mineur); ISCRUTE(release);
    goto ERROR;
  }
  fileversionM   = 100*majeur;
  fileversionMM  = 100*majeur+10*mineur;
  fileversionMMR = fileversionMM+release;


  libversionM   = 100*MED_MAJOR_NUM;
  libversionMM  = libversionM+10*MED_MINOR_NUM;
  libversionMMR = libversionMM+MED_RELEASE_NUM;

/*   ISCRUTE(libversionM   ); */
/*   ISCRUTE(libversionMM  ); */
/*   ISCRUTE(libversionMMR ); */

  if (  (fileversionM > libversionM) || (fileversionMMR > libversionMM+9 ) ) {
    fprintf(stderr,"MED%d library is only able to use MED file which version is such that :\n"
	    "\t %d < version <= %d\n",(int) libversionMMR,(int)libversionM,(int)libversionMM+9);
    goto ERROR;
  }

  _ret=0;
 ERROR:
  return _ret;
}
