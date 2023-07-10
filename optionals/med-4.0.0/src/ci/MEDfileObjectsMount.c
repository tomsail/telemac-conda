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


#include <med_config.h>
#include <med.h>
#include <med_outils.h>
#include <string.h>

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

/**\ingroup MEDfile
  \brief \MEDfileObjectsMountBrief
  \param fid \fid
  \param mountfilename \mountfilename
  \param medclass \medclass
  \retval med_idt  \mountId
  \details \MEDfileObjectsMountDetails
 */

med_idt
MEDfileObjectsMount(const med_idt         fid,
		    const char*     const mountfilename,
		    const med_class       medclass )
{
  char *  name = "_MEDfileObjectsMount";
  int     dummy=0;
  med_idt fret=-1;
  med_int majeur, mineur, release;
  MedFuncType func;

  MEDfileNumVersionRd(fid, &majeur, &mineur, &release);
  func = _MEDversionedApi3(name,majeur,mineur,release);
  if ( func != (MedFuncType) NULL )
    func (dummy,
	  fid,
	  0,
	  mountfilename,
	  medclass,
	  &fret);
  return fret;
}
