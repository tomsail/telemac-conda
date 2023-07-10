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

#include "med_versioned.h"

#ifdef PPRO_NT_CALL
#define F_OK 0
#else
#include <unistd.h>
#endif

/**\ingroup MEDfile
  \brief \MEDfileObjectsMountByIdBrief
  \param fid \fid
  \param chfid \chfid
  \param chpath \chpath
  \param medclass \medclass
  \retval med_idt  \mountId
  \details \MEDfileObjectsMountByIdDetails
 */

med_idt
MEDfileObjectsMountById(const med_idt         fid,
			const med_idt         chfid,
			const char * const    chpath,
			const med_class       medclass )
{
  int     dummy=0;
  med_idt ret=-1,fret=-1;

  _MEDmodeErreurVerrouiller();
  if (_MEDcheckVersion30(fid) < 0) goto ERROR;

  _MEDfileObjectsMount30(dummy,
			 fid,
			 chfid,
			 chpath,
			 medclass,
			 &fret);
  ret=fret;
 ERROR:
  return ret;
}
